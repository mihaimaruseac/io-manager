module Main where

-- A simple test module for IOManager which will echo the contents of the
-- input files to the output files, paired as specified in standard input:
-- each line of stdin is made of two words: «input output» meaning that the
-- contents of «input» should be echoed to «output». If input is «@stdin» then
-- the content to be echoed is from the standard input. If output is «@stdout»
-- then content should be written to standard ouput instead of a file. Same is
-- true for the case where «@stderr» is in the ouput part.
--
-- Assume that the stdin description is valid: no «@stdout»/«@stderr» on the
-- input part and no «@stdin» on the output part.

-- Import IO code from IOManager.
import IOManager

-- The main function as a pipeline.
main = readInput >>= return . solve >>= writeOutput

-- Solution of the problem. Add here your implementation.
solve :: Input -> Output
solve = undefined
