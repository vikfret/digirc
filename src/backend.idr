import System
import Prelude.List as L
import Prelude.Strings as S
import Data.String

-- Helper functions.

strDrop : Nat -> String -> String
strDrop n = (\s => substr n (length s) s)

-- Main program.

rpn : List String -> List Double -> String
rpn [] ns = unwords . reverse $ map show ns
rpn ("+"::ss) (b::a::ns) = rpn ss (a+b::ns)
rpn ("-"::ss) (b::a::ns) = rpn ss (a-b::ns)
rpn ("*"::ss) (b::a::ns) = rpn ss (a*b::ns)
rpn ("/"::ss) (b::a::ns) = rpn ss (a/b::ns)
rpn ("^"::ss) (b::a::ns) = rpn ss ((pow a b)::ns)
rpn ("dup"::ss) (n::ns) = rpn ss (n::n::ns)
rpn ("drop"::ss) (n::ns) = rpn ss ns
rpn ("swap"::ss) (b::a::ns) = rpn ss (a::b::ns)
rpn ("over"::ss) (b::a::ns) = rpn ss (a::b::a::ns)
rpn ("rot"::ss) (c::b::a::ns) = rpn ss (a::c::b::ns)
rpn ("-rot"::ss) (c::b::a::ns) = rpn ss (b::a::c::ns)
rpn ("nip"::ss) (b::a::ns) = rpn ss (b::ns)
rpn ("tuck"::ss) (b::a::ns) = rpn ss (b::a::b::ns)
rpn ("pick"::ss) (n::ns) with (index' (toNat (cast {to=Int} n)) ns)
  | Just x = rpn ss (x::ns)
  | Nothing = "Error: 'pick' is invalid."
rpn ("clear"::ss) ns = rpn ss []
rpn ("depth"::ss) ns = rpn ss ((cast {to=Double} (toIntNat $ L.length ns))::ns)
rpn ("+"::ss) s = "Error: '+' is invalid."
rpn (s::ss) ns with (parseDouble s)
  | Just n = rpn ss (n::ns)
  | Nothing = "Error: '" ++ s ++ "' is unrecognized or invalid."

help : String -> String
help "say" = "Says the given args. Example: #say Hello!"
help "yell" = "Says the given args, but in uppercase. Example: #yell Hello!"
help "swedish" = "Says the given args, but in swedish. Example: #swedish Hello!"
help "yellswedish" = "Says the given args, but in uppercase swedish. Example: #yellswedish Hello!"
help "spanish" = "Says the given args, but in spanish. Example: #spanish Hello!"
help "yellspanish" = "Says the given args, but in uppercase spanish. Example: #yellspanish Hello!"
help "whoami" = "Says your username."
help "rpn" = "An RPN evaluator. Supports: '+', '-', '*', '/', '^', 'dup', 'drop', 'swap', 'over', 'rot', '-rot', 'nip', 'tuck', 'pick', 'clear', 'depth'"
help "monad" = "They're just monoids in the category of endofunctors. What's the problem?"
help x = "Commands: say, yell, swedish, yellswedish, spanish, yellspanish, whoami, rpn"

runCmd : String -> String -> String -> String -> IO String
runCmd ("Debug") _ _ _ = pure "OK"
runCmd _ _ ("#say") (args) = pure args
runCmd _ _ ("#yell") (args) = pure $ toUpper args
runCmd _ _ ("#swedish") (args) = pure $ pack . intersperse 'f' . unpack $ args
runCmd _ _ ("#yellswedish") (args) = pure $ pack . intersperse 'F' . unpack $ toUpper args
runCmd _ _ ("#spanish") (args) = pure $ unwords . map (++ "o") . words $ args
runCmd _ _ ("#yellspanish") (args) = pure $ unwords . map (++ "O") . words $ toUpper args
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
  if S.length args /= 0 then
    runCmd origin sender cmd args
  else
    runCmd origin sender cmd ""

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
