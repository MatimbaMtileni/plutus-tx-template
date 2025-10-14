-- OptionValidators.hs — small library of pure validators for CLI/options
module OptionValidators
  ( Validator
  , runValidator
  , intInRange
  , nonEmpty
  , oneOf
  , matches
  , combine
  , parseInt
  , parseAndValidate
  , exampleMain
  ) where

import Data.Char (isSpace)
import Text.Read (readMaybe)
import Data.List (intercalate)

-- Pure function: A validator takes a value and either returns the same value (Right)
-- or an error message (Left).
type Validator a = a -> Either String a

-- Pure function: runValidator applies a validator and returns either the error or the value.
runValidator :: Validator a -> a -> Either String a
runValidator v x = v x

-- Pure function: combine two validators into one. If the first fails, its error is returned.
combine :: Validator a -> Validator a -> Validator a
combine v1 v2 x = case v1 x of
  Left err -> Left err
  Right _  -> v2 x

infixr 3 `combine`

-- Pure function: intInRange checks that an Int lies between lo and hi (inclusive).
intInRange :: Int -> Int -> Validator Int
intInRange lo hi x
  | x < lo    = Left $ "number " ++ show x ++ " is less than minimum " ++ show lo
  | x > hi    = Left $ "number " ++ show x ++ " is greater than maximum " ++ show hi
  | otherwise = Right x

-- Pure function: nonEmpty checks that a String is not empty or only whitespace.
nonEmpty :: Validator String
nonEmpty s
  | all isSpace s = Left "value must not be empty"
  | otherwise     = Right s

-- Pure function: oneOf checks that a value is in the allowed list (requires Show & Eq).
oneOf :: (Eq a, Show a) => [a] -> Validator a
oneOf allowed x
  | x `elem` allowed = Right x
  | otherwise        = Left $ "value " ++ show x ++ " is not one of: " ++ intercalate ", " (map show allowed)

-- Pure function: matches checks a string with a predicate and supplies message on failure.
matches :: String -> (String -> Bool) -> Validator String
matches failureMsg predFn s
  | predFn s  = Right s
  | otherwise = Left failureMsg

-- Pure function: parseInt parses a String into Int, returning an error message on failure.
parseInt :: String -> Either String Int
parseInt s = case readMaybe s :: Maybe Int of
  Just n  -> Right n
  Nothing -> Left $ "cannot parse integer from \"" ++ s ++ "\""

-- Pure function: parseAndValidate parses a string (via parser) then applies a validator.
parseAndValidate :: (String -> Either String a) -> Validator a -> String -> Either String a
parseAndValidate parser validator s = case parser s of
  Left perr -> Left perr
  Right v   -> validator v

-- IO wrapper: exampleMain demonstrates validating some imaginary CLI options.
exampleMain :: IO ()
exampleMain = do
  putStrLn "Example: validating options"
  -- pretend these came from the command line
  let rawPort = "8080"
      rawMode = "production"
      rawName = "  "          -- invalid (only whitespace)

  -- validators
  let portValidator = intInRange 1024 65535
      modeValidator = oneOf ["development", "staging", "production"]
      nameValidator = nonEmpty `combine` matches "name must be at least 3 chars" (\n -> length (trim n) >= 3)

  -- helper to trim whitespace
  let trim = f . f where f = reverse . dropWhile isSpace

  -- validate port
  case parseAndValidate parseInt portValidator rawPort of
    Left err -> putStrLn $ "Port error: " ++ err
    Right port -> putStrLn $ "Port OK: " ++ show port

  -- validate mode
  case runValidator modeValidator rawMode of
    Left err -> putStrLn $ "Mode error: " ++ err
    Right m  -> putStrLn $ "Mode OK: " ++ m

  -- validate name
  case runValidator nameValidator rawName of
    Left err -> putStrLn $ "Name error: " ++ err
    Right nm -> putStrLn $ "Name OK: \"" ++ nm ++ "\""

-- End of file