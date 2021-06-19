{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.Codegen where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.Instances
import Language.Cobble.Shared

-- Defined here because the instances need
-- @Type 'Codegen@ to be defined
deriving instance Show ModSig
deriving instance Eq ModSig
deriving instance Data ModSig

deriving instance Show TypeVariant
deriving instance Eq TypeVariant
deriving instance Data TypeVariant

type instance XModule 'Codegen = Ext Codegen (Map (Name 'Codegen) ModSig)

type instance XDecl      'Codegen = Ext Codegen (Type 'Codegen)
type instance XParam     'Codegen = Ext Codegen [(Name 'Codegen, Type 'Codegen)]

type instance XDef       'Codegen = IgnoreExt Codegen
type instance XImport    'Codegen = IgnoreExt Codegen
type instance XDefStruct 'Codegen = IgnoreExt Codegen
type instance XStatement 'Codegen = ExtVoid Codegen

type instance XFCall            'Codegen = Ext Codegen (Type 'Codegen)
type instance XIntLit           'Codegen = IgnoreExt Codegen
type instance XIf               'Codegen = Ext Codegen (QualifiedName, Int)
type instance XLet              'Codegen = IgnoreExt Codegen
type instance XVar              'Codegen = Ext Codegen (Type 'Codegen)
type instance XStructConstruct  'Codegen = Ext Codegen (StructDef 'Codegen, Type 'Codegen)
type instance XStructAccess     'Codegen = Ext Codegen (StructDef 'Codegen, Type 'Codegen)
type instance XExpr             'Codegen = ExtVoid Codegen

type instance Name 'Codegen = QualifiedName

type instance XKind 'Codegen = Kind
