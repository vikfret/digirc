import System
import Prelude.List as L
import Prelude.Strings as S
import Data.String

-- Helper functions.

strDrop : Nat -> String -> String
strDrop n = (\s => substr n (length s) s)

-- Main program.

rpn : List String -> List Integer -> String
rpn [] ns = unwords . reverse $ map show ns
rpn ("+"::ss) (b::a::ns) = rpn ss (a+b::ns)
rpn ("-"::ss) (b::a::ns) = rpn ss (a-b::ns)
rpn ("*"::ss) (b::a::ns) = rpn ss (a*b::ns)
rpn ("/"::ss) (0::a::ns) = "Error: '/' by 0 is invalid."
rpn ("/"::ss) (b::a::ns) = rpn ss (div a b::ns)
rpn ("dup"::ss) (n::ns) = rpn ss (n::n::ns)
rpn ("drop"::ss) (n::ns) = rpn ss ns
rpn ("swap"::ss) (b::a::ns) = rpn ss (a::b::ns)
rpn ("over"::ss) (b::a::ns) = rpn ss (a::b::a::ns)
rpn ("rot"::ss) (c::b::a::ns) = rpn ss (a::c::b::ns)
rpn ("-rot"::ss) (c::b::a::ns) = rpn ss (b::a::c::ns)
rpn ("nip"::ss) (b::a::ns) = rpn ss (b::ns)
rpn ("tuck"::ss) (b::a::ns) = rpn ss (b::a::b::ns)
rpn ("pick"::ss) (n::ns) with (index' (toNat n) ns)
  | Just x = rpn ss (x::ns)
  | Nothing = "Error: 'pick' is invalid."
rpn ("clear"::ss) ns = rpn ss []
rpn ("depth"::ss) ns = rpn ss ((toIntegerNat $ L.length ns)::ns)
rpn ("+"::ss) s = "Error: '+' is invalid."
rpn (s::ss) ns with (parsePositive {a = Integer} s)
  | Just n = rpn ss (n::ns)
  | Nothing = "Error: '" ++ s ++ "' is unrecognized or invalid."

help : String -> String
help "say" = "Says the given args. Example: #say Hello!"
help "whoami" = "Says your username."
help "rpn" = "An RPN evaluator. Supports: '+', '-', '*', '/', 'dup', 'drop', 'swap', 'over', 'rot', '-rot', 'nip', 'tuck', 'pick', 'clear', 'depth'"
help x = "Commands: say, whoami, rpn"

runCmd : String -> String -> String -> String -> IO String
runCmd ("Debug") _ _ _ = pure "OK"
runCmd _ _ ("#say") (args) = pure args
runCmd _ (sender) ("#whoami") _ = pure sender
runCmd _ _ ("#rpn") (args) = pure $ rpn (words args) []
runCmd _ _ ("#help") (args) = pure $ help args
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
  runCmd origin sender cmd args

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
