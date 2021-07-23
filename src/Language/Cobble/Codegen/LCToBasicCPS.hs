module Language.Cobble.Codegen.LCToBasicCPS where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Shared
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C

freshVar :: (Members '[State Int] r) => QualifiedName -> Sem r QualifiedName
freshVar v = state \i -> (v <> show i, i + 1)

compileExpr :: (Members '[State Int] r) => LCExpr -> CPSVal -> Sem r CPS
compileExpr = flip \kval -> let k = staticCont kval in \case
    L.Var v             -> pure $ k (C.Var v)
    L.IntLit n          -> pure $ k (C.IntLit n)
    L.Lambda x e        -> freshVar "k" >>= \k' -> k . C.Lambda x k' <$> compileExpr e (C.Var k') -- ?
    L.App f a           -> freshVar "f" >>=  \f' -> freshVar "v" >>= \v' ->
        compileExpr f =<< C.Admin f' <$> compileExpr a (C.Admin v' (C.App3 (C.Var f') (C.Var v') kval)) -- ?
    L.Tuple es          -> foldrM   (\(i, e) r -> compileExpr e (C.Admin ("x" +. show i) r))
                                    (C.Let "t" (C.Tuple (fmap (C.Var . ("x" +.) . show) (indexes es))) (k (C.Var "t"))) -- ?
                                    (zipIndex es)
    L.Select n e        -> compileExpr e (C.Admin "t" (C.Let "y" (C.Select n (C.Var "t")) (k (C.Var "y")))) -- ?

reduceAdmin :: CPS -> CPS
reduceAdmin = transformBi adminToLambda . rewriteBi (betaLetReduce <<|>> etaReduce)
    where
        betaLetReduce :: CPS -> Maybe CPS
        betaLetReduce = \case
            C.App2 (C.Admin x e) y
                | C.Var _ <- y                                      -> Just $ replaceVar x y e
                --  | length [() | C.Var v <- universeBi e, v == x] <= 1 -> Just $ replaceVar x y e
                -- TODO: Should inlining be done in a different step?
                -- TODO: This might change evaluation order but in a purely functional language that is not important
                -- TODO: (IO might need some special cases here)
                | otherwise                                          -> Just $ C.Let x (C.Val y) e
            _                                                        -> Nothing

        etaReduce :: CPS -> Maybe CPS
        etaReduce c = case [() | C.Admin x (C.App2 _k (C.Var x')) <- universeBi c, x == x'] of
            [] -> Nothing
            _  -> Just $ c & transformBi \case
                C.Admin x (C.App2 k (C.Var x')) | x == x' -> k
                x                                         -> x

        replaceVar :: QualifiedName -> CPSVal -> CPS -> CPS
        replaceVar v e = transformBi \case
            (C.Var v') | v == v' -> e
            x -> x
            
        adminToLambda :: CPSVal -> CPSVal
        adminToLambda x = x
        -- TODO
            
staticCont :: CPSVal -> (CPSVal -> CPS)
staticCont = C.App2