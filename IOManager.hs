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

type Filename = String

-- | TODO
data Input

-- | TODO
data Output

-- | Obtains the contents of the standard input as given to the program.
-- Returns a String containing the input without any modification.
getStdIn :: Input -> String
getStdIn = undefined

-- | Obtains the contents of an input file. Returns a String containing the
-- input without any modification.
getInputFile :: Input -> Filename -> String
getInputFile = undefined

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
readInput = undefined

-- | Writes the contents of an @Output@ value to the needed files.
writeOutput :: Output -> IO ()
writeOutput = undefined
