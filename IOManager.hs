module IOManager
  (
    -- * The @Filename@ type
    Filename

    -- * The @Input@ type
  , Input

    -- * The @Output@ type
  , Output

    -- * Exported for use in main pipeline
  , readInput
  , writeOutput
  ) where

import Debug.Trace (trace)
import qualified Data.Map as Map
import System.Environment (getArgs)

type Filename = String

-- | Type of values holding inputs to the program, grouped by input source.
data Input = Input
  { stdin :: String
  , file :: Map.Map Filename String
  } deriving (Show)

-- | TODO
data Output

-- | Obtains the contents of the standard input as given to the program.
-- Returns a String containing the input without any modification.
getStdIn :: Input -> String
getStdIn = stdin

-- | Obtains the contents of an input file. Returns a String containing the
-- input without any modification.
getInputFile :: Input -> Filename -> String
getInputFile i f = file i Map.! f

-- | Appends text to the standard output. No newline is printed at the end,
-- the caller must handle it. Returns a new @Output@ value, containing the
-- appended text.
writeStdOut :: Output -> String -> Output
writeStdOut = undefined

-- | Appends text to the standard error. No newline is printed at the end, the
-- caller must handle it. Returns a new @Output@ value, containing the
-- appended text.
--
-- **Note**: When running the program, the standard error text is displayed
-- after the entire text from the standard input is displayed.
writeStdErr :: Output -> String -> Output
writeStdErr = undefined

-- | Appends to an output file. If the file does not exist in the @Output@
-- value (this program didn't yet write in it), it is created as a new one.
-- Returns a new @Output@ value, containing the appended text.
writeOutputFile :: Output -> Filename -> String -> Output
writeOutputFile = undefined

-- | Reads the input from all the files given as command line arguments and
-- constructs an @Input@ value.
readInput :: IO Input
readInput = do
  args <- getArgs
  print args
  map <- readInputFiles args Map.empty
  input <- getContents
  print $ Input input map
  return $ Input input map

-- | Writes the contents of an @Output@ value to the needed files.
writeOutput :: Output -> IO ()
writeOutput = undefined

-- Reads all of the input files into the map of the Input value.
readInputFiles :: [Filename]
               -> Map.Map Filename String
               -> IO (Map.Map Filename String)
readInputFiles [] m = return m
readInputFiles (f:fs) m = do
  content <- readFile f
  readInputFiles fs $ Map.insert f content m
