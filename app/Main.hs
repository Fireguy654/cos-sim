module Main (main) where

import           Data.Foldable          (traverse_)
import           Data.Grid
import           HW6.T3
import           Options.Applicative
import           System.Random.Stateful

data CliArgs = CliArgs
    { iterations :: Int
    , gridSize   :: Int
    , covConfig  :: Config }

fillArgs :: Int -> Int -> Double -> Int -> Int -> Int -> CliArgs
fillArgs it sz pr dInc dIll dImm = CliArgs it sz $ Config pr dInc dIll dImm

cliParser :: Parser CliArgs
cliParser = fillArgs
  <$> option auto
    ( long "iterations"
    <> metavar "INT"
    <> showDefault
    <> value 10
    <> help "Amount of infection iterations" )
  <*> option auto
    ( long "grid-size"
    <> metavar "INT"
    <> showDefault
    <> value 6
    <> help "The size of the grid(it is rounded to higher odd)" )
  <*> option auto
    ( long "prob"
    <> metavar "DOUBLE"
    <> showDefault
    <> value 0.3
    <> help "The probability with infected person passes an infection to other person")
  <*> option auto
    ( long "incub"
    <> metavar "INT"
    <> showDefault
    <> value 2
    <> help "The duration of incubation period" )
  <*> option auto
    ( long "ill"
    <> metavar "INT"
    <> showDefault
    <> value 2
    <> help "The duration of illness period" )
  <*> option auto
    ( long "immun"
    <> metavar "INT"
    <> showDefault
    <> value 2
    <> help "The duration of immune period" )

main :: IO ()
main = do
  let opts = info (cliParser <**> helper)
        ( fullDesc
        <> progDesc "Simulate the infection spread"
        <> header "Simple infection spread simulator" )
  cli <- execParser opts
  seed <- uniformM globalStdGen :: IO Int
  let slides = take (iterations cli) $ simulate seed (covConfig cli)
  traverse_ (\g -> (traverse putStrLn $ toMatrix (cellSymb <$> g) $ gridSize cli `div` 2) >> putStrLn "") slides
