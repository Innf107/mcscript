{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Compiler where

import Language.Cobble.Prelude

import Language.Cobble.Util

import Language.Cobble.Types as S

import Language.Cobble.Codegen.Types hiding (PrimOpEnv(..))
import Language.Cobble.Codegen.Types qualified as P (PrimOpEnv(..))
import Language.Cobble.Codegen.PrimOps

import Data.Text qualified as T

import Language.Cobble.MCAsm.Types as A hiding (Name)

panicVarNotFoundTooLate :: (Member (Error Panic) r) => Name 'Codegen -> Sem r a
panicVarNotFoundTooLate v = panic $ "Variable " <> show v <> " not found. This should have been caught earlier!" 

panicFunNotFoundTooLate :: (Members [Error Panic, State CompileState] r) => Name 'Codegen -> Sem r a
panicFunNotFoundTooLate v = do
    st <- get
    panic $ "Function " <> show v <> " not found. This should have been caught earlier!\n    State: " <> show st

panicStructFieldNotFoundTooLate :: Name 'Codegen -> UnqualifiedName -> Panic
panicStructFieldNotFoundTooLate cname fname = Panic $ "Struct field '" <> show fname <> "' does not exist on record '"
                                            <> show cname <> "'. This should have been caught earlier!"

initialCompileState :: CompileState
initialCompileState = CompileState {
      _frames = Frame {
        _varRegs=mempty
      , _lastReg=0
      , _regs=[]
      } :| []
    , _functions = mempty
    }


emptyFrame :: Frame
emptyFrame = Frame mempty 0 []

stackReg :: Register
stackReg = NamedReg "STACK"

stackPTRReg :: Register
stackPTRReg = NamedReg "STACKPTR"


makeLenses 'Frame
makeLenses 'CompileState
makeLenses 'Function

compile :: (CompileC r) => S.Module 'Codegen -> Sem r A.Module
compile (S.Module _deps modname stmnts) = log LogVerbose ("STARTING COBBLE CODEGEN FOR MODULE: " <> show modname) 
                                       >> A.Module modname . fst <$> runWriterAssocR (traverse compileStatement stmnts)

newReg :: (CompileC r) => Sem r Register
newReg = do
    reg <- get <&> Reg . (view (frames . head1 . lastReg))
    modify (& frames . head1 . lastReg +~ 1)
    modify (& frames . head1 . regs %~ (reg:))
    pure reg

compileStatement :: forall r. (Member (Writer [Instruction]) r, CompileC r) => S.Statement 'Codegen -> Sem r ()
compileStatement s = (log LogDebugVerbose ("COMPILING STATEMENT: " <>  show s) >>) $ s & \case
    {-
    DefFun () _li name pars body retExp t -> do
        modify (& functions . at name ?~ Function {_params=pars, _returnType=Just t})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            parRegs <- traverse (\(_, pt) -> newRegForType VarReg pt) pars
            modify (& frames %~ (emptyFrame |:))
            modify (& frames . head1 . varRegs .~ fromList (zip (map fst pars) parRegs))
            modify (& frames . head1 . regs .~ parRegs)
            traverse_ compileStatement body
            res <- compileExprToReg retExp
            modify (& frames %~ unsafeTail)
            tell [MoveReg (returnReg (regRep res)) res]
    -}
    Def IgnoreExt _li (Decl (Ext retT) name (Ext ps) body) _ty -> do
        modify (& functions . at name ?~ Function {_params=ps, _returnType=retT})
        tell . pure . A.Section name . fst =<< runWriterAssocR do
            let parRegs = zipWith (\_ i -> ArgReg i) ps [0..]
            modify (& frames %~ (emptyFrame |:))
            modify (& frames . head1 . varRegs .~ fromList (zip (map fst ps) parRegs))
            modify (& frames . head1 . regs .~ parRegs)
            res <- compileExprToReg body
            modify (& frames %~ unsafeTail)
            tell [MoveReg returnReg res]
    Import IgnoreExt _ _ -> pass
    DefStruct IgnoreExt _ _ _ -> pass
    StatementX x _ -> absurd x

compileExprToReg :: forall r. (Member (Writer [Instruction]) r, CompileC r) => Expr 'Codegen -> Sem r Register
compileExprToReg e = (log LogDebugVerbose ("COMPILING EXPR: " <> show e) >>) $ e & \case
    (IntLit IgnoreExt _li i) -> mkIntReg i
    (UnitLit _li) -> pure unitReg
    (Var _t _li n) -> get <&> join . (^? frames . head1 . varRegs . at n) >>= \case
        Nothing -> panicVarNotFoundTooLate n
        Just vReg -> pure $ vReg -- Let's hope this actually works and does not break with recursion

    -- (FloatT, FloatLit i) -> pure [MoveNumReg (NumReg reg)]
    (FCall (Ext t) _li (Var _ _vli fname) args) -> case lookup fname (primOps @r) of
        Just (_, primOpF) -> primOpF primOpEnv (toList args)
        Nothing -> do
            get <&> (^. functions . at fname) >>= \case
                Nothing -> panicFunNotFoundTooLate fname
                Just f -> do
                    frame <- gets (^. frames . head1)
                    saveFrame frame

                    writeArgs (toList args)

                    tell [Call fname]

                    restoreFrame frame
                    
                    ret <- newReg
                    tell [MoveReg ret returnReg]
                    pure ret
    FCall _t li ex _as -> panic' "Cannot indirectly call a function yet. This is *NOT* a bug" [show ex, show li]
    If (Ext (name, ifID)) _li c th el -> do
        cr <- compileExprToReg c
        resReg <- newReg
        tell . pure . A.Section (name .: ("-then-e" <> show ifID)) =<< (\(is, r) -> is <> [MoveNumLit elseReg 0, MoveReg resReg r]) <$> runWriterAssocR (compileExprToReg th)
        tell . pure . A.Section (name .: ("-else-e" <> show ifID)) =<< (\(is, r) -> is <> [MoveReg resReg r]) <$> runWriterAssocR (compileExprToReg el)
        tell [MoveNumLit elseReg 1
            , CallInRange cr (RBounded 1 1) (name .: ("-then-e" <> show ifID))
            , CallInRange elseReg (RBounded 1 1) (name .: ("-else-e" <> show ifID))
            ]
        pure resReg
    Let IgnoreExt _li (Decl _t vname (Ext []) expr) body -> do
        exprReg <- compileExprToReg expr
        modify (& frames . head1 . varRegs . at vname ?~ exprReg)
        compileExprToReg body
    Let IgnoreExt _li (Decl _t vname (Ext ps) _expr) _body -> panic' "Local functions are not supported yet. This is *NOT* a bug" [show vname, show ps]
    StructConstruct (Ext (_def, t)) _li _cname fields -> do
        -- We can assume that all fields are present in ps and in the same order as in the struct definition
        fieldRegs <- traverse (compileExprToReg . snd) fields
        arrReg <- newReg
        ixRegs <- traverse mkIntReg [0..length fieldRegs - 1]
        tell (zipWith (SetNewInArray arrReg) ixRegs fieldRegs)
        pure arrReg
    StructAccess (Ext (def, t)) _li cex fieldName -> do
        fieldIndex <- note (panicStructFieldNotFoundTooLate (view structName def) fieldName)
                    $ findIndexOf (structFields . folded) (\(x,_) -> unqualifyName x == fieldName) def
        structReg <- compileExprToReg cex
        ixReg <- mkIntReg fieldIndex
        resReg <- newReg
        tell [GetInArray resReg structReg ixReg]
        pure resReg
    ExprX x _li -> absurd x

-- TODO: Move int lit initialization to program start
mkIntReg :: (CompileC r, Member (Writer [Instruction]) r) => Int -> Sem r Register
mkIntReg i = newReg >>= \reg -> tell [MoveNumLit reg i] $> reg

writeArgs :: (CompileC r, Member (Writer [Instruction]) r) => [Expr 'Codegen] -> Sem r ()
writeArgs args = do
    ress <- traverse compileExprToReg args
    zipWithM_ (\r i -> tell [MoveReg (ArgReg i) r]) ress [0..]

-- TODO: Initialize empty stack elements and write with `SetInArray`
saveFrame :: (CompileC r, Member (Writer [Instruction]) r) => Frame -> Sem r ()
saveFrame f = traverse_ pushRegToStack (toList $ f ^. regs)

restoreFrame :: (CompileC r, Member (Writer [Instruction]) r) => Frame -> Sem r ()
restoreFrame f = traverse_ popRegFromStack (reverse $ toList $ f ^. regs)

pushRegToStack :: (CompileC r, Member (Writer [Instruction]) r) => Register -> Sem r()
pushRegToStack r = tell [
        SetInArrayOrNew stackReg stackPTRReg r
    ,   AddLit stackPTRReg 1
    ]
      
popRegFromStack :: (CompileC r, Member (Writer [Instruction]) r) => Register -> Sem r()
popRegFromStack r = tell [
        SubLit stackPTRReg 1
    ,   GetInArray r stackReg stackPTRReg
    ,   DestroyInArray stackReg stackPTRReg
    ]

elseReg :: Register
elseReg = NamedReg "ELSE"

returnReg :: Register
returnReg = NamedReg "RETURN"

rtType :: Type 'Codegen -> Rep
rtType = \case
    TCon "prims.Int" KStar    -> RepNum
    TCon "prims.Bool" KStar   -> RepNum
    TCon "prims.Entity" KStar -> RepEntity
    _                   -> RepArray


unitReg :: Register
unitReg = NamedReg "UNIT"

trueReg :: Register
trueReg = NamedReg "TRUE"

falseReg :: Register
falseReg = NamedReg "FALSE"

primOpEnv :: (Member (Writer [Instruction]) r, CompileC r) => P.PrimOpEnv r
primOpEnv = P.PrimOpEnv {
        compileExprToReg
    ,   newReg
    ,   unitReg
    ,   trueReg
    ,   falseReg
    }
