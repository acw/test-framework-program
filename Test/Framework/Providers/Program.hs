{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Framework.Providers.Program(
         Checker
       , testProgramRuns
       , testProgramOutput
       )
 where

import Data.Typeable
import System.Directory
import System.Exit
import System.IO hiding (stdout, stderr)
import System.Process hiding (runProcess)

import Test.Framework.Providers.API

-- |A shorthand for a possible function checking an output stream.
type Checker = Maybe (String -> Bool)

runCheck :: Checker -> String -> Bool
runCheck Nothing  _ = True
runCheck (Just f) x = f x

data TestCaseResult  = Passed        | ProgramFailed ExitCode |
                       Timeout       | CheckFailed            |
                       NotExecutable
data TestCaseRunning = CheckExists | CheckRunnable | Running
data TestCase        = TestCase Checker Checker FilePath [FilePath]
 deriving (Typeable)

instance Show TestCaseResult where
  show  Passed           = "OK"
  show (ProgramFailed c) = "Program failed: Exit code " ++ show c
  show  Timeout          = "Test timed out."
  show  CheckFailed      = "Post-run check failed"
  show  NotExecutable    = "Program not found / executable."
instance Show TestCaseRunning where
  show  CheckExists      = "Checking program existence"
  show  CheckRunnable    = "Checking program is executable"
  show  Running          = "Running"

instance TestResultlike TestCaseRunning TestCaseResult where
  testSucceeded x = case x of
                      Passed -> True
                      _      -> False

instance Testlike TestCaseRunning TestCaseResult TestCase where
  testTypeName _ = "Programs"
  runTest topts (TestCase outCheck errCheck prog args) = runImprovingIO $ do
    yieldImprovement CheckExists
    exists <- liftIO $ doesFileExist prog
    if exists
      then do yieldImprovement CheckRunnable
              perms <- liftIO $ getPermissions prog
              if executable perms
                then do yieldImprovement Running
                        runProgram topts outCheck errCheck prog args
                else return NotExecutable
      else return NotExecutable

runProgram :: TestOptions' K->
              Checker -> Checker ->
              FilePath -> [String] ->
              ImprovingIO i f TestCaseResult
runProgram topts stdoutCheck stderrCheck prog args = do
  let timeout = unK (topt_timeout topts)
  mres <- maybeTimeoutImprovingIO timeout $ liftIO $ runProcess prog args
  case mres of
    Nothing  -> return Timeout
    Just (ExitSuccess, stdout, stderr)
      | runCheck stdoutCheck stdout && runCheck stderrCheck stderr ->
          return Passed
      | otherwise                                                  ->
          return CheckFailed
    Just (x, _, _) ->
      return (ProgramFailed x)

runProcess :: FilePath -> [String] -> IO (ExitCode, String, String)
runProcess prog args = do
  (_,o,e,p) <- runInteractiveProcess prog args Nothing Nothing
  hSetBuffering o NoBuffering
  hSetBuffering e NoBuffering
  sout  <- hGetContents o
  serr  <- hGetContents e
  ecode <- length sout `seq` waitForProcess p
  return (ecode, sout, serr)

-- |Test that a given program runs correctly with the given arguments. 'Runs
-- correctly' is defined as running and exiting with a successful (0) error
-- code.
testProgramRuns :: String -> FilePath -> [String] -> Test
testProgramRuns name prog args =
  testProgramOutput name prog args Nothing Nothing

-- |Test that a given program runs correctly (exits successfully), and that
-- its stdout / stderr are acceptable.
testProgramOutput :: String -> FilePath -> [String] ->
                     Checker -> Checker ->
                     Test
testProgramOutput name prog args soutCheck serrCheck =
  Test name (TestCase soutCheck serrCheck prog args)

