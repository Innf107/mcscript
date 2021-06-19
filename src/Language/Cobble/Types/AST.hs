{-# LANGUAGE UndecidableInstances #-}
module Language.Cobble.Types.AST where

import Language.Cobble.Prelude hiding (TVar)
import Language.Cobble.Util.Convert
import Language.Cobble.Util.TypeUtils

import Language.Cobble.Types.TH

import Language.Cobble.Shared

import Language.Cobble.MCAsm.Types (Objective)

import Data.Data
import Data.Generics.Uniplate.Data

import GHC.Show qualified as S

import qualified Unsafe.Coerce 

import qualified Data.Map as M

type family Name (p :: Pass)

type family InstanceRequirements t :: [HSType]

-- Top level module.
data Module (p :: Pass) = Module
    { xModule :: (XModule p)
    , moduleName :: (Name p)
    , moduleStatements :: [Statement p]
    }


type instance InstanceRequirements (Module p) = [XModule p, Name p, Statement p]

type family XModule (p :: Pass)

-- | The signature of a module contains
-- everything that it exports
-- (Variables, Functions, Types, etc.)
data ModSig = ModSig {
    exportedVars :: Map QualifiedName (Type 'Codegen)
,   exportedTypes :: Map QualifiedName (Kind, TypeVariant)
} deriving (Generic, Typeable) -- Instances for @Eq@ and @Data@ are defined in Language.Cobble.Types.AST.Codegen

data TypeVariant = RecordType [(QualifiedName, Type 'Codegen)]
                 | BuiltInType
                 deriving (Generic, Typeable)

type Dependencies = Map QualifiedName ModSig

instance Semigroup ModSig where ModSig vs ts <> ModSig vs' ts' = ModSig (vs <> vs') (ts <> ts')
instance Monoid ModSig where mempty = ModSig mempty mempty

-- | A data kind representing the state of the AST at a certain Compiler pass.
data Pass = SolveModules
          | QualifyNames
          | SemAnalysis
          | Typecheck
          | Codegen


data Statement (p :: Pass) =
      Def        (XDef p)       LexInfo (Decl p) (Type p)
    | Import     (XImport p)    LexInfo (Name p) -- TODO: qualified? exposing?
    | DefStruct  (XDefStruct p) LexInfo (Name p) [(Name p, Type p)]
    | StatementX (XStatement p) LexInfo

type instance InstanceRequirements (Statement p) = [XDef p, XImport p, XDefStruct p, XStatement p, Name p, Type p, Decl p]

type family XDef            (p :: Pass)
type family XParam          (p :: Pass)
type family XImport         (p :: Pass)
type family XDefStruct      (p :: Pass)
type family XStatement      (p :: Pass)


data Decl (p :: Pass) = Decl (XDecl p) (Name p) (XParam p) (Expr p)

type instance InstanceRequirements (Decl p) = [XDecl p, Name p, XParam p, Expr p]

type family XDecl (p :: Pass)

data Expr (p :: Pass) =
      FCall           (XFCall p) LexInfo (Expr p) (NonEmpty (Expr p))
    | IntLit          (XIntLit p) LexInfo Int
    | UnitLit         LexInfo
    | If              (XIf p) LexInfo (Expr p) (Expr p) (Expr p)
    | Let             (XLet p) LexInfo (Decl p) (Expr p)
    | Var             (XVar p) LexInfo (Name p)
    | StructConstruct (XStructConstruct p) LexInfo (Name p) [(Name p, Expr p)]
    | StructAccess    (XStructAccess p) LexInfo (Expr p) UnqualifiedName
    | ExprX           (XExpr p) LexInfo

type instance InstanceRequirements (Expr p) = [XFCall p, XIntLit p, XIf p, XLet p, Decl p, XVar p, Name p,
        XStructConstruct p, XStructAccess p, XExpr p]

type family XFCall           (p :: Pass)
type family XIntLit          (p :: Pass)
type family XIf              (p :: Pass)
type family XLet             (p :: Pass)
type family XVar             (p :: Pass)
type family XStructConstruct (p :: Pass)
type family XStructAccess    (p :: Pass)
type family XExpr            (p :: Pass)

data Kind = KStar | KFun Kind Kind deriving (Eq, Ord, Generic, Data, Typeable)

infixr 5 `KFun`

data Type (p :: Pass) = TCon (Name p) (XKind p)
                      | TApp (Type p) (Type p)
                      | TVar (TVar p)

data TVar (p :: Pass) = MkTVar (Name p) (XKind p)


type instance InstanceRequirements (Type p) = [Name p, XKind p, TVar p]
type instance InstanceRequirements (TVar p) = [Name p, XKind p]

(-:>) :: (TyLit (Name p), IsKind (XKind p)) => Type p -> Type p -> Type p
t1 -:> t2 = TApp (TApp (TCon tyFunT (kFun kStar (kFun kStar kStar))) t1) t2
infixr 5 -:>

pattern (:->) :: (Name p ~ QualifiedName, Eq (Name p), XKind p ~ Kind) => Type p -> Type p -> Type p
pattern (:->) t1 t2 = TApp (TApp (TCon "prims.->" (KFun KStar (KFun KStar KStar))) t1) t2

pattern (:~>) :: (Name p ~ Text, Eq (Name p), XKind p ~ Kind) => Type p -> Type p -> Type p
pattern (:~>) t1 t2 = TApp (TApp (TCon "->" (KFun KStar (KFun KStar KStar))) t1) t2


type family XKind (p :: Pass)

type FileName = Text


data LexInfo = LexInfo {
      startPos :: SourcePos
    , endPos :: SourcePos
    , file :: FileName
    } deriving (Show, Eq, Data, Typeable)

data SourcePos = SourcePos {
      line :: Int
    , column :: Int
    } deriving (Show, Eq, Ord, Data, Typeable)

-- | Combines two 'LexInfo's
-- This assumes that the first one comes before the second
-- and that they are both part of the same file.
mergeLexInfo :: LexInfo -> LexInfo -> LexInfo
mergeLexInfo (LexInfo {startPos, file}) (LexInfo {endPos}) = LexInfo {startPos, endPos, file}

data StructDef p = StructDef {
        _structName :: Name p
    ,   _structFields :: [(Name p, Type p)]
    }

type instance InstanceRequirements (StructDef p) = [Name p, Type p]

instance S.Show Kind where
    show KStar = "*"
    show (KFun k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"

class TyLit n where
    tyIntT  :: n
    tyBoolT :: n
    tyUnitT :: n 
    tyFunT  :: n
    
instance TyLit QualifiedName where
    tyIntT  = "prims.Int"
    tyBoolT = "prims.Bool"
    tyUnitT = "prims.Unit"
    tyFunT  = "prims.->"
instance TyLit Text where
    tyIntT = "Int"
    tyBoolT = "Bool"
    tyUnitT = "Unit"
    tyFunT  = "->"

intT, boolT, unitT :: (IsKind (XKind p), TyLit (Name p)) => Type p
intT  = TCon tyIntT (fromKind KStar)
boolT = TCon tyBoolT (fromKind KStar)
unitT = TCon tyUnitT (fromKind KStar)

class IsKind t where 
    kFun :: t -> t -> t
    kStar :: t
    kStar = fromKind KStar
    fromKind :: Kind -> t
    fromKind KStar = kStar
    fromKind (KFun k1 k2) = kFun (fromKind k1) (fromKind k2)
    {-# MINIMAL kFun, (fromKind | kStar) #-}
instance IsKind () where 
    fromKind _ = ()
    kFun _ _   = ()
instance IsKind Kind where 
    fromKind = id
    kFun = KFun 

class HasKind t where
    kind :: t -> Either (Kind, Kind) Kind

instance (XKind p ~ Kind) => HasKind (TVar p) where
    kind (MkTVar _ k) = Right k

instance ((XKind p) ~ Kind) => HasKind (Type p) where
    kind = \case
        TVar v -> kind v
        TCon _ k -> pure k
        TApp t1 t2 -> bisequence (kind t1, kind t2) >>= \case
            (KFun kp kr, ka)
                | kp == ka -> pure kr
            (k1, k2) -> Left (k1, k2)
    
      
class CoercePass t1 t2 p1 p2 | t1 -> p1, t2 -> p2 where
    _coercePass :: t1 -> t2
        
{-# NOINLINE coercePass #-}
coercePass :: (CoercePass t1 t2 p1 p2) => t1 -> t2
coercePass = _coercePass
{-# RULES "coercePass/coercePass" forall x. coercePass x = Unsafe.Coerce.unsafeCoerce x#-}

newtype Ext (p :: Pass) t = Ext {getExt :: t} deriving (Show, Eq, Generic, Data)
data IgnoreExt (p :: Pass) = IgnoreExt deriving (Show, Eq, Generic, Data)
data ExtVoid (p :: Pass) deriving (Show, Eq, Generic, Data)

absurd :: ExtVoid t -> a
absurd _ = error "Language.Cobble.Types.AST.absurd"

instance (Coercible t1 t2) => CoercePass (Ext p1 t1) (Ext p2 t2) p1 p2 where
    _coercePass = coercePass

instance CoercePass (IgnoreExt p1) (IgnoreExt p2) p1 p2 where
    _coercePass = coerce

instance CoercePass (ExtVoid p1) (ExtVoid p2) p1 p2 where
    _coercePass = absurd

instance (Coercible k1 k2, CoercePass v1 v2 p1 p2, Ord k2) => CoercePass (Map k1 v1) (Map k2 v2) p1 p2 where
    _coercePass = M.mapKeys coerce . fmap coercePass


instance
    (   Name p1 ~ Name p2
    ,   CoercePass (Statement p1) (Statement p2) p1 p2
    ,   CoercePass (XModule p1) (XModule p2) p1 p2
    )
    => CoercePass (Module p1) (Module p2) p1 p2 where
    _coercePass (Module x n sts) = Module (coercePass x) n (map coercePass sts)

instance
    (   Name p1 ~ Name p2
    ,   CoercePass (Expr p1) (Expr p2) p1 p2
    ,   CoercePass (Type p1) (Type p2) p1 p2
    ,   CoercePass (Decl p1) (Decl p2) p1 p2
    ,   CoercePass (XDef p1) (XDef p2) p1 p2
    ,   CoercePass (XParam p1) (XParam p2) p1 p2
    ,   CoercePass (XImport p1) (XImport p2) p1 p2
    ,   CoercePass (XDefStruct p1) (XDefStruct p2) p1 p2
    ,   CoercePass (XStatement p1) (XStatement p2) p1 p2
    )
    => CoercePass (Statement p1) (Statement p2) p1 p2 where
    _coercePass = \case
        Def x l d t -> Def (coercePass x) l (coercePass d) (coercePass t)
        Import x l n -> Import (coercePass x) l n
        DefStruct x l n fs -> DefStruct (coercePass x) l n (map (second coercePass) fs)
        StatementX x l -> StatementX (coercePass x) l

instance
    (   Name p1 ~ Name p2
    ,   CoercePass (XFCall p1) (XFCall p2) p1 p2
    ,   CoercePass (XIntLit p1) (XIntLit p2) p1 p2
    ,   CoercePass (XIf p1) (XIf p2) p1 p2
    ,   CoercePass (XLet p1) (XLet p2) p1 p2
    ,   CoercePass (XParam p1) (XParam p2) p1 p2
    ,   CoercePass (XDecl p1) (XDecl p2) p1 p2
    ,   CoercePass (XVar p1) (XVar p2) p1 p2
    ,   CoercePass (XStructConstruct p1) (XStructConstruct p2) p1 p2
    ,   CoercePass (XStructAccess p1) (XStructAccess p2) p1 p2
    ,   CoercePass (XExpr p1) (XExpr p2) p1 p2
    )
    => CoercePass (Expr p1) (Expr p2) p1 p2 where
    _coercePass = \case
        FCall x l f as           -> FCall (coercePass x) l (coercePass f) (fmap coercePass as)
        IntLit x l i             -> IntLit (coercePass x) l i
        UnitLit l                -> UnitLit l
        If x l c th el           -> If (coercePass x) l (coercePass c) (coercePass th) (coercePass el)
        Let x l d b              -> Let (coercePass x) l (coercePass d) (coercePass b)
        Var x l v                -> Var (coercePass x) l v
        StructConstruct x l c fs -> StructConstruct (coercePass x) l c (map (second coercePass) fs)
        StructAccess x l e f     -> StructAccess (coercePass x) l (coercePass e) f
        ExprX x l                -> ExprX (coercePass x) l

instance
    (   Name p1 ~ Name p2
    ,   CoercePass (XDecl p1) (XDecl p2) p1 p2
    ,   CoercePass (XParam p1) (XParam p2) p1 p2
    ,   CoercePass (Expr p1) (Expr p2) p1 p2
    )
    => CoercePass (Decl p1) (Decl p2) p1 p2 where
    _coercePass (Decl x f ps e) = Decl (coercePass x) f (coercePass ps) (coercePass e)


instance
    (   Name p1 ~ Name p2
    ,   XKind p1 ~ XKind p2
    ,   CoercePass (TVar p1) (TVar p2) p1 p2
    )
    => CoercePass (Type p1) (Type p2) p1 p2 where
    _coercePass = \case
        TCon n k -> TCon n k
        TApp t1 t2 -> TApp (coercePass t1) (coercePass t2)
        TVar t -> TVar (coercePass t)

instance
    (   Name p1 ~ Name p2
    ,   XKind p1 ~ XKind p2
    ) => CoercePass (TVar p1) (TVar p2) p1 p2 where
    _coercePass (MkTVar n k) = MkTVar n k

instance
    (   Name p1 ~ Name p2
    ,   CoercePass (Type p1) (Type p2) p1 p2
    )
    => CoercePass (StructDef p1) (StructDef p2) p1 p2 where
    _coercePass (StructDef n fs) = StructDef n (map (second coercePass) fs)


class HasLexInfo t where
    getLexInfo :: t -> LexInfo

instance HasLexInfo (Expr p) where
    getLexInfo = \case
        FCall _ li _ _           -> li
        IntLit _ li _            -> li
        UnitLit li               -> li
        If _ li _ _ _            -> li
        Let _ li _ _             -> li
        Var _ li _               -> li
        StructConstruct _ li _ _ -> li
        StructAccess _ li _ _    -> li
        ExprX _ li               -> li
        

