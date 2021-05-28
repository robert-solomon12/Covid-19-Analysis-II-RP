module Params (Params (..), cmdLineParser) where

import Options.Applicative
import Data.Text (Text, strip)

data Params = Params {
                fname :: FilePath
              , chart :: Bool
              , htmlFile :: Maybe FilePath
              , silent :: Bool
              }

mkParams :: Parser Params
mkParams =
  Params <$>                                    				-- final injection
             strArgument																-- Mandatory positional FilePath arg
               (metavar "FILE" <> help "CSV file name") -- arg's name and help text in output
         <*> switch																			-- chart is a boolean option - if 																													-- it's used, then the chart is 																														-- generated, otherwise no chart 
         																								-- is generated.
               (long "chart" <> short 'c' <>				
                help "generate chart")
         <*> optional (strOption $
               long "html" <> metavar "FILE" <>
               help "generate HTML report")
         <*> switch																			-- silent is a boolean option - if 																													-- it's used, then no stats 																													     	-- are printed
               (long "silent" <> short 's' <>
                help "don't print statistics")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper)                                  -- info is a opt function <**> incorporates the helper function
                (fullDesc <> progDesc "Stock quotes data processing")   -- this is printed after --help with the available options
                
-- cmdLineParser :: IO Params
-- cmdLineParser = execParser opts
--   where
--     opts = info mkParams                                     -- info is a opt function
--                 -- (fullDesc <> progDesc "Stock quotes data processing")