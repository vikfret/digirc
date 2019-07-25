import System
import Prelude.List as L
import Prelude.Strings as S
import Data.String

-- Helper functions.

unwrapInt : Maybe Int -> Int
unwrapInt (Just x) = x
unwrapInt Nothing = 0

strDrop : Nat -> String -> String
strDrop n = (\s => substr n (length s) s)

-- Main program.

rpn : List String -> List Int -> String
rpn [] ns = unwords $ map show ns
rpn ("+"::ss) (a::b::ns) = rpn ss (b+a::ns)
rpn ("-"::ss) (a::b::ns) = rpn ss (b-a::ns)
rpn ("*"::ss) (a::b::ns) = rpn ss (b*a::ns)
rpn ("/"::ss) (a::b::ns) = rpn ss (div b a::ns)
rpn ("dup"::ss)  (n::ns) = rpn ss (n::n::ns)
rpn ("drop"::ss) (n::ns) = rpn ss ns
rpn ("+"::ss) s = "Syntax Error: '+' is unrecognized or invalid."
rpn (s::ss) ns with (parsePositive {a = Int} s)
  | Just n = rpn ss (n::ns)
  | Nothing = "Syntax Error: '" ++ s ++ "' is unrecognized or invalid."

help : String -> String
help "say" = "Says the given args. Example: #say Hello!"
help "whoami" = "Says your username."
help "rpn" = "An RPN evaluator. Supports: '+', '-', '*', '/', 'dup', 'drop'"
help x = "Commands: say, whoami, rpn"

runCmd : Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO String
runCmd (Just "Debug") _ _ _ = pure "OK"
runCmd _ _ (Just "#say") (Just args) = pure args
runCmd _ (Just sender) (Just "#whoami") _ = pure sender
runCmd _ _ (Just "#rpn") (Just args) = pure $ rpn (words args) []
runCmd _ _ (Just "#help") (Just args) = pure $ help args
runCmd _ _ _ _ = pure "OK"

issueCmd : String -> IO String
issueCmd s =
  let barSplit = S.break (== '|') s in
  let origin = fst barSplit in
  let colonSplit = S.break (== ':') (snd barSplit) in
  let sender = strDrop 2 $ (fst colonSplit) in
  let message = strDrop 2 $ (snd colonSplit) in
  let cmdSplit = S.break (== ' ') message in
  let cmd = fst cmdSplit in
  let args = strDrop 1 $ snd cmdSplit in
  runCmd (Just origin) (Just sender) (Just cmd) (Just args)

main : IO ()
main = do
  ready <- fpoll stdin
  if ready then do
    line <- getLine
    out <- issueCmd line
    putStrLn out
    fflush stdout
    main
  else do
    main
