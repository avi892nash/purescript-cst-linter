module CLI where

import Prelude

import Data.Array (foldMap, length)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, error)
import Effect.Unsafe (unsafePerformEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Process (argv, cwd, exit, exit')
import PSLint (lintAllFiles)
import PSLint.Types (PSLintConfig)
import PureScript.CST.Types as CST
import Types (Diagnostic)
import Version (version)
import Yoga.JSON (read, readJSON, writeJSON)


-- | CLI Error type
data CliError
  = ConfigFileNotFound FilePath
  | SpagoConfigNotFound
  | ConfigParseError String
  | LintError String

instance showCliError :: Show CliError where
  show (ConfigFileNotFound path) = "Error: Config file not found at " <> path
  show SpagoConfigNotFound = "Error: spago.dhall not found in the current directory. This command must be run in a PureScript project directory."
  show (ConfigParseError err) = "Error parsing config file: " <> err
  show (LintError err) = "Linting error: " <> err

-- | Parse the configuration file
parseConfig :: String -> Either CliError PSLintConfig
parseConfig content =
  case readJSON content of
    Left err -> Left $ ConfigParseError $ "Invalid JSON: " <> show err
    Right json -> case read json of
      Left err -> Left $ ConfigParseError $ show err
      Right config -> Right config

-- | Check if required files exist
checkFiles :: Effect (Either CliError Unit)
checkFiles = do
  workDir <- liftEffect cwd
  let 
    configPath = workDir <> "/.pslintrc"
    spagoPath = workDir <> "/spago.dhall"
  
  hasSpagoConfig <- exists spagoPath
  
  if not hasSpagoConfig
    then pure $ Left SpagoConfigNotFound
    else do
      hasConfig <- exists configPath
      if not hasConfig
        then pure $ Left $ ConfigFileNotFound configPath
        else pure $ Right unit

-- | Read and parse config file
readConfig :: Aff (Either CliError PSLintConfig)
readConfig = do
  workDir <- liftEffect cwd
  let configPath = workDir <> "/.pslintrc"
  
  content <- readTextFile UTF8 configPath
  pure $ parseConfig content

-- | Count the number of errors and warnings in the diagnostic results
countErrorsAndWarnings :: Array { uri :: String, content :: String, params :: Array { status :: String, type :: String, ranges :: Array (String /\ CST.SourceRange) } } -> { errors :: Int, warnings :: Int }
countErrorsAndWarnings diagnostics = 
  foldl (\acc diag -> 
    foldl (\innerAcc param -> 
      case param.status of
        "error" -> innerAcc { errors = innerAcc.errors + length param.ranges }
        "warn" -> innerAcc { warnings = innerAcc.warnings + length param.ranges }
        _ -> innerAcc
    ) acc diag.params
  ) { errors: 0, warnings: 0 } diagnostics

printDiagnostic :: { uri :: String, content :: String, params :: Array { status :: String, type :: String, ranges :: Array (String /\ CST.SourceRange) } } -> String
printDiagnostic { uri, params } = do
  let 
    severityColor = case _ of
      "error" -> "\x1b[31m" -- Red
      "warn" -> "\x1b[33m" -- Yellow
      -- Just Hint -> "\x1b[36m" -- Cyan
      -- Just Information ->  "\x1b[38m" 
      _ -> "\x1b[40m"
    
    -- showSeverity = case _ of
    --   Just Error -> "Error"
    --   Just Warning -> "Warning"
    --   Just Hint -> "Hint"
    --   Just Information ->  "Information"
    --   Nothing -> "NA"
    
    resetColor = "\x1b[0m"

    formattedMessage :: { ranges :: (String /\ CST.SourceRange), status :: String, type :: String } -> String
    formattedMessage ({ ranges , status, type : typeofLint }) = do
      let message /\ range = ranges
      uri <> ":" <> show range.start.line <> ":" <> show range.start.column <> " - " <>
        severityColor status <>  status <> resetColor <> " " <> typeofLint <> " " <>
        message <> "\n"
  foldMap (\p -> foldMap (\r -> formattedMessage { status : p.status, type : p.type, ranges : r}) p.ranges) params 

-- | Generate summary message with error and warning counts
generateSummary :: { errors :: Int, warnings :: Int } -> String
generateSummary { errors, warnings } = do
  let
    errorColor = "\x1b[31m" -- Red
    warnColor = "\x1b[33m" -- Yellow
    resetColor = "\x1b[0m"
    
    errorText = if errors == 1 then "error" else "errors"
    warningText = if warnings == 1 then "warning" else "warnings"
    
    result = 
      if errors == 0 && warnings == 0 
        then "\n✓ No issues found"
      else "\nFound " <> 
        (if errors > 0 then errorColor <> show errors <> " " <> errorText <> resetColor else "") <>
        (if errors > 0 && warnings > 0 then " and " else "") <>
        (if warnings > 0 then warnColor <> show warnings <> " " <> warningText <> resetColor else "")
  
  result <> "\n"

-- | Main CLI run function
runCli :: Array String -> Aff (Either CliError Unit)
runCli args = do
  case args of
    ["--version"] -> do
      liftEffect $ log $ "purescript-lint version " <> version
      pure $ Right unit
    
    _ -> do
      filesResult <- liftEffect $ checkFiles
      case filesResult of
        Left err -> do
          -- Just log and return the error, don't call exit
          liftEffect $ error $ show err
          pure $ Left err
        Right _ -> do
          configResult <- readConfig
          case configResult of
            Left err -> do
              liftEffect $ error $ show err
              pure $ Left err
            Right config -> do
              diagnostics <- lintAllFiles config
              let 
                counts = countErrorsAndWarnings diagnostics
                summary = generateSummary counts
              
              -- Print all diagnostics first
              _ <- liftEffect $ log $ foldMap printDiagnostic diagnostics
              
              -- Then print the summary
              _ <- liftEffect $ log summary
              
              -- Return an error if there are any errors
              if counts.errors > 0 
                then do
                  -- Don't call exit here, just return the error
                  pure $ Left $ LintError $ "Lint failed with " <> show counts.errors <> " errors"
                else pure $ Right unit

-- | Main entry point
main :: Unit
main = unsafePerformEffect $ launchAff_ do
  args <- liftEffect $ Array.drop 2 <$> argv
  result <- runCli args
  case result of
    Left err -> liftEffect $ exit' 1 -- Exit with error code 1 for CLI errors
    Right _ -> pure unit