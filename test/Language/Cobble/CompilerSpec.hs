module Language.Cobble.CompilerSpec where

import Language.Cobble.Prelude

import Language.Cobble.Compiler
import Language.Cobble.Types
import Language.Cobble.MCAsm.Types

import Test.Hspec

spec :: Spec
spec = do
    describe "statements" do
        describe "Decl" do
            it "Produces the same instructions as Assign" do
                compileTest [Decl () dli "test.x" Nothing (IntLit () dli 5)]
                    `shouldBe`
                    compileTest' (initialCompileState & frames . head1 . varIndices . at "test.x" ?~ 0)
                        [Assign () dli "test.x" (IntLit () dli 5)]
        describe "Assign" do
            it "panics if the variable was not previously declared" do
                compileTest [Assign () dli "test.x" (IntLit () dli 23)]
                    `shouldBe`
                    Left (TestPanic $ Panic "Variable test.x not found. This should have been caught earlier!")


data TestError = TestPanic Panic
               | TestMcAsmError McAsmError
               deriving (Show, Eq)

dli :: LexInfo
dli = LexInfo 0 0 "Test"

compileTest :: [Statement 'Codegen] -> Either TestError [Instruction]
compileTest = snd . runCompTest initialCompileState . fmap fst . runWriterAssocR . traverse compileStatement

compileTest' :: CompileState -> [Statement 'Codegen] -> Either TestError [Instruction]
compileTest' s = snd . runCompTest s . fmap fst . runWriterAssocR . traverse compileStatement

compileTestWithLogs :: [Statement 'Codegen] -> ([Log], Either TestError [Instruction])
compileTestWithLogs = runCompTest initialCompileState . fmap fst . runWriterAssocR . traverse compileStatement

runCompTest :: CompileState -> Sem '[State CompileState, Error Panic, Error McAsmError, Error TestError, Output Log] a -> ([Log], Either TestError a)
runCompTest s = run . runOutputList . runError . mapError TestMcAsmError . mapError TestPanic . evalState s
