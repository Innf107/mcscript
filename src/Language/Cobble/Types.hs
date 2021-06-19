module Language.Cobble.Types  
    (
      module Export
    , NameSpace
    , HasType (..)
    )
    where

import Language.Cobble.Types.Lens as Export
import Language.Cobble.Types.AST as Export
import Language.Cobble.Types.AST.SolveModules as Export
import Language.Cobble.Types.AST.QualifyNames as Export
import Language.Cobble.Types.AST.SemAnalysis as Export
import Language.Cobble.Types.AST.Typecheck as Export
import Language.Cobble.Types.AST.Codegen as Export
import Language.Cobble.Types.Instances as Export
import Language.Cobble.Shared as Export
import Language.Cobble.MCAsm.Types as Export (Objective(..)) 

import Language.Cobble.Prelude

type NameSpace = Text


class HasType t p | t -> p where
    getType :: t -> Type p
    type_ :: Lens' t (Type p)

instance HasType (Expr 'Codegen) 'Codegen where
    getType = \case
        FCall (Ext t) _ _ _                 -> t
        Var (Ext t) _ _                     -> t
        IntLit _ _ _                        -> intT
        If _ _ _ th _                       -> getType th
        UnitLit _                           -> unitT
        Let _ _ _ b                         -> getType b
        StructConstruct (Ext (_, t)) _ _ _  -> t
        StructAccess (Ext (_, t)) _ _ _     -> t
        ExprX v _                           -> absurd v
    type_ = lens getType $ flip $ \t -> \case
        FCall _ l f as                      -> FCall (Ext t) l f as
        Var _ l n                           -> Var (Ext t) l n
        IntLit x l i                        -> IntLit x l i
        If x l c th el                      -> If x l c (set type_ t th) (set type_ t el)
        UnitLit l                           -> UnitLit l
        Let x l d b                         -> Let x l d (set type_ t b)
        StructConstruct (Ext (d, _)) l n ts -> StructConstruct (Ext (d, t)) l n ts
        StructAccess (Ext (d, _)) l s f     -> StructAccess (Ext (d, t)) l s f
        ExprX v _                           -> absurd v
        --FCall _ l f as ->

instance HasType (Type p) p where
    getType = id
    type_ = id