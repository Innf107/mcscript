module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert
import Language.Cobble.Util.Bitraversable
import Language.Cobble.Types
import Language.Cobble.Types.Lens
import Language.Cobble.Shared

import qualified Data.Map as M

type NextPass = 'Codegen

data TypeError = VarDoesNotExist LexInfo (Name NextPass)
               --  | FunctionDoesNotExist LexInfo (Name NextPass) -- Unused since functions are now treated as plain exprs
               | WrongFunArgs LexInfo (Maybe (Name NextPass)) [Type NextPass] [Type NextPass]
               | TooManyFunArgs Natural (Type NextPass)
--                                       ^ expected
               | TooManyAppliedArgs Natural (Type NextPass)
               | WrongReturnType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                         ^ expected
               | WrongDeclType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                  ^ expected
               | WrongAssignType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                    ^ expected
               | StructAccessOnNonStructType LexInfo (Type NextPass) UnqualifiedName
               | StructDoesNotContainField LexInfo (Type NextPass) UnqualifiedName
--                                                    ^ Type name     ^ field
               | WrongIfType LexInfo (Type NextPass)
               | WrongRecordConstructionType LexInfo (Name Typecheck) (Type NextPass) (Type NextPass)
               | DifferentIfETypes LexInfo (Type NextPass) (Type NextPass)
               | WrongSetScoreboardType LexInfo Objective Text (Type NextPass)
               | CannotUnify (Type NextPass) (Type NextPass)
               | SubstMergeError Subst Subst
               | MatchTypeMismatch (Type NextPass) (Type NextPass)
               | MatchBindRHS (Type NextPass) (TVar NextPass)
               | OccursCheck (TVar NextPass) (Type NextPass)
               | KindMismatch (Type NextPass) (Type NextPass)
               deriving (Show, Eq)

data TypeWarning = WarnIgnoredFunRetType LexInfo (Type NextPass) deriving (Show, Eq)


newtype TCState = TCState {
        varTypes::Map (Name 'Typecheck) (Type NextPass)
    } deriving stock (Show, Eq)
      deriving newtype (Semigroup, Monoid)

type TypecheckC r = Members [State TCState, Error TypeError, Output TypeWarning] r

initialTCState :: TCState
initialTCState = TCState {
        varTypes = mempty
    }

getVarType :: (Members '[State TCState, Error TypeError] r) => LexInfo -> Name 'Typecheck -> Sem r (Type NextPass)
getVarType l varName = gets varTypes <&> lookup varName >>= \case
    Nothing -> throw (VarDoesNotExist l varName)
    Just t -> pure t


insertVarType :: (Members '[State TCState] r) => Name NextPass -> Type NextPass -> Sem r ()
insertVarType varName t = void $ modify (\s -> s{varTypes=varTypes s & insert varName t})

typecheckModule :: (TypecheckC r, Members '[Error Panic] r) => Module 'Typecheck -> Sem r (Module NextPass)
typecheckModule (Module (Ext deps) mname instrs) = Module (Ext deps) mname <$> traverse typecheck instrs

typecheck :: (TypecheckC r, Members '[Error Panic] r) => Statement Typecheck -> Sem r (Statement NextPass)
typecheck = \case
    {-
    DefFun () l fname (conv -> args) stmnts lastexpr (conv -> retT) -> do
        
        let ftype = foldr (-:>) retT (map snd args)
        insertVarType fname ftype
        
        for_ args (uncurry insertVarType)
        
        stmnts' <- traverse typecheck stmnts
        lastexpr' <- typeOf lastexpr
        if (getType lastexpr' == retT)
        then pure (DefFun () l fname args stmnts' lastexpr' retT)
        else throw (WrongReturnType l fname retT (getType lastexpr'))
    -}
    Def IgnoreExt l (Decl IgnoreExt name (Ext ps) body) ty -> do
        insertVarType name (coercePass ty)
        case splitFunType (genericLength ps) (coercePass ty) of
            Nothing -> throw $ TooManyFunArgs (genericLength ps) (coercePass ty)
            Just (ptys, retTy) -> do
                zipWithM_ (insertVarType) ps ptys
                body' <- tcExpr body
                if (getType body' == retTy)
                then pure $ Def IgnoreExt l (Decl (Ext retTy) name (Ext $ zip ps ptys) body') (coercePass ty)
                else throw (WrongReturnType l name retTy (getType body'))
        
    DefStruct IgnoreExt l name (map (second coercePass) -> fields) -> pure $ DefStruct IgnoreExt l name fields -- TODO: Add to state map -- or not? (The qualifier does this already right?)
    Import IgnoreExt l modName -> pure $ Import IgnoreExt l modName
    StatementX x _l -> case x of

tcDecl :: (Members '[Error Panic, Error TypeError, State TCState] r) => Decl 'Typecheck -> Sem r (Decl NextPass)
tcDecl (Decl IgnoreExt vname (Ext []) expr) = do
    expr' <- tcExpr expr
    insertVarType vname (getType expr')
    pure (Decl (Ext (getType expr')) vname (Ext []) expr')
tcDecl (Decl IgnoreExt vname ps _expr) = panic' "Local functions are not possible yet. This is *NOT* a bug" [show vname, show ps]

tcExpr :: (Members '[Error Panic, Error TypeError, State TCState] r) => Expr 'Typecheck -> Sem r (Expr NextPass)
tcExpr = \case
    IntLit IgnoreExt l x -> pure $ IntLit IgnoreExt l x
    UnitLit l -> pure $ UnitLit l
    -- FloatLit x -> pure (FloatLit x, FloatT)
    FCall IgnoreExt l f exprs -> do
        f' <- tcExpr f
        (fargs, retT) <- maybe (throw (TooManyAppliedArgs (fromIntegral (length exprs)) (getType f'))) pure
            $ splitFunType (fromIntegral (length exprs)) (getType f')
        
        exprs' <- traverse tcExpr exprs
        
        let exprTypes = toList $ fmap getType exprs'

        argSubst <- joinSubst =<< zipWithM mgu exprTypes (toList fargs)

        pure $ FCall (Ext (apply argSubst retT)) l f' exprs'
    If x l c th el -> do
        c' <- tcExpr c
        condSubst <- mgu (getType c') boolT
        th' <- apply condSubst <$> tcExpr th
        el' <- apply condSubst <$> tcExpr el

        resSubst <- mgu (getType th') (getType el')
        let c''  = apply resSubst c'
        let th'' = apply resSubst th'
        let el'' = apply resSubst el'
        pure (If (coerce x) l c'' th'' el'')
    Var IgnoreExt l vname -> (\t -> Var (Ext t) l vname) <$> getVarType l vname
    Let IgnoreExt li d body -> Let IgnoreExt li
        <$> tcDecl d
        <*> tcExpr body
    StructConstruct def li cname fields -> do
        fields' <- traverse (rightM tcExpr) fields
        -- we can assume that the fields are all present and in the same order as in the struct definition
        zipWithM_ (\(n, e) (_, coercePass -> t) -> when (getType e /= t) (throw (WrongRecordConstructionType li n t (getType e)))) fields' (view structFields def)
        let t = TCon cname KStar --TODO?
        pure $ StructConstruct (Ext (coercePass def, t)) li cname fields'
    StructAccess possibleStructs li strEx fname -> do
        strEx' <- tcExpr strEx
        let ty = getType strEx'
        case ty of
            TCon tyName _ -> do
                structDef <- note (StructAccessOnNonStructType li ty fname) $ lookup tyName possibleStructs
                structFieldType <- note (StructDoesNotContainField li ty fname) $ preview (unqualifiedFieldType fname) structDef
                pure $ StructAccess (Ext (coercePass @_ @_ @Typecheck @Codegen structDef, coercePass structFieldType)) li strEx' fname
            _ -> throw (StructAccessOnNonStructType li ty fname)
    ExprX x _l -> absurd x

splitFunType :: Natural -> Type NextPass -> Maybe ([Type NextPass], Type NextPass)
splitFunType 0 t = Just ([], t)
splitFunType argCount (t :-> ts) = splitFunType (argCount - 1) ts <&> \(as, ret) -> (t:as, ret)
splitFunType _ _ = Nothing

tryGetFunName :: Expr 'Typecheck -> Maybe (Name 'Typecheck)
tryGetFunName (Var IgnoreExt _l n) = Just n
tryGetFunName _ = Nothing

type Subst = Map (TVar NextPass) (Type NextPass)


class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [TVar NextPass]

instance Types (Type NextPass) where
    apply s (TVar v) = case lookup v s of
        Nothing -> TVar v
        Just t -> t
    apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)
    apply _ (TCon n k) = TCon n k

    tv (TVar v) = [v]
    tv (TCon _ _) = []
    tv (TApp t1 t2) = ordNub $ tv t1 ++ tv t2

instance Types t => Types [t] where
    apply = map . apply
    tv = ordNub . concatMap tv

instance Types (Expr NextPass) where
    apply s e = over type_ (apply s) e
    tv = tv . getType

(+->) :: TVar NextPass -> Type NextPass -> Subst
v +-> t = one (v, t)

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
(@@) s1 s2 = mapBothMap (\v t -> (v, apply s1 t)) s2 <> s1

merge :: (Member (Error TypeError) r) => Subst -> Subst -> Sem r Subst
merge s1 s2
    | agree = pure $ s1 <> s2
    | otherwise = throw $ SubstMergeError s1 s2
    where
        agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (keys $ M.intersection s1 s2)

mgu :: (Member (Error TypeError) r) => Type NextPass -> Type NextPass -> Sem r Subst
mgu (TVar v) t = bindVar v t
mgu t (TVar v) = bindVar v t
mgu (TCon t1 k1) (TCon t2 k2)
    | t1 == t2 && k1 == k2 = pure mempty
mgu (TApp l1 r1) (TApp l2 r2) = (@@) <$> mgu l1 l2 <*> mgu r1 r2
mgu t1 t2 = throw $ CannotUnify t1 t2

match :: (Member (Error TypeError) r) => Type NextPass -> Type NextPass -> Sem r Subst
match (TVar v) t
    | kind v == kind t = bindVar v t
    | otherwise = throw (KindMismatch (TVar v) t)
match t1@(TCon _ _) t2@(TCon _ _)
    | t1 == t2 = pure mempty
    | otherwise = throw $ MatchTypeMismatch t1 t2
match t (TVar v) = throw $ MatchBindRHS t v
match (TApp l1 r1) (TApp l2 r2) = join $ merge <$> match l1 l2 <*> match r1 r2
match t1 t2 = throw $ MatchTypeMismatch t1 t2

bindVar :: (Member (Error TypeError) r) => TVar NextPass -> Type NextPass -> Sem r Subst
bindVar v t
    | TVar v == t           = pure mempty
    | v `elem` tv t         = throw (OccursCheck v t)
    | kind v /= kind t      = throw (KindMismatch (TVar v) t)
    | otherwise             = pure $ v +-> t

joinSubst :: (Member (Error TypeError) r) => [Subst] -> Sem r Subst
joinSubst = foldr (\x my -> merge x =<< my) (pure mempty)

