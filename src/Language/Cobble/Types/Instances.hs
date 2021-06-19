{-#OPTIONS_GHC -Wno-orphans#-}
{-#LANGUAGE TemplateHaskell, UndecidableInstances#-}
module Language.Cobble.Types.Instances where

import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.TH

deriveInstanceReqs

derivePass ''Ord ''TVar
