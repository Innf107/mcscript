{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Types.TH where

import Relude -- Not importing Language.Cobble.Prelude to avoid unnecessary name conflicts

import Language.Haskell.TH

import Language.Cobble.Util.TypeUtils
import Data.Data (Data)

derivePass :: Name -> Name -> Q [Dec]
derivePass className typeName = do
    let class_ = conT className
    let type_ = conT typeName
    let allC  = conT ''AllC
    let instanceReq = conT $ mkName "InstanceRequirements"
    [d|deriving instance ($allC $class_ ($instanceReq ($type_ p))) => $class_ ($type_ p)|]

derivePassWithConstraint :: Name -> Name -> Name -> Q [Dec]
derivePassWithConstraint constraint className typeName = do
    let class_ = conT className
    let type_ = conT typeName
    let allC  = conT ''AllC
    let instanceReq = conT $ mkName "InstanceRequirements"
    [d|deriving instance ($(conT constraint) p, $allC $class_ ($instanceReq ($type_ p))) => $class_ ($type_ p)|]


derivePassTypeable :: Name -> Name -> Q [Dec]
derivePassTypeable = derivePassWithConstraint ''Typeable

deriveDefault :: Name -> Q [Dec]
deriveDefault ty = concat <$> sequence [
        derivePass ''Show ty
    ,   derivePass ''Eq ty
    ,   derivePassTypeable ''Data ty
    ,   [d|deriving instance Generic ($(conT ty) p)|]
    ]

