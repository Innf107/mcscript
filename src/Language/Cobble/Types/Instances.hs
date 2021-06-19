{-#OPTIONS_GHC -Wno-orphans#-}
{-#LANGUAGE TemplateHaskell, UndecidableInstances#-}
module Language.Cobble.Types.Instances where

import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.TH

deriveDefault ''Module
deriveDefault ''Statement
deriveDefault ''Decl
deriveDefault ''Expr
deriveDefault ''Type
deriveDefault ''TVar
derivePass ''Ord ''TVar
deriveDefault ''StructDef
