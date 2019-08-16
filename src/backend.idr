import System
import Prelude.List as L
import Prelude.Strings as S
import Data.String

-- Helper functions.

strDrop : Nat -> String -> String
strDrop n = (\s => substr n (length s) s)

unwrapMaybe : a -> Maybe a -> a
unwrapMaybe d (Just x) = x
unwrapMaybe d Nothing = d

ctoi : Char -> Int
ctoi '0' = 0
ctoi '1' = 1
ctoi '2' = 2
ctoi '3' = 3
ctoi '4' = 4
ctoi '5' = 5
ctoi '6' = 6
ctoi '7' = 7
ctoi '8' = 8
ctoi '9' = 9
ctoi 'a' = 10
ctoi 'A' = 10
ctoi 'b' = 11
ctoi 'B' = 11
ctoi 'c' = 12
ctoi 'C' = 12
ctoi 'd' = 13
ctoi 'D' = 13
ctoi 'e' = 14
ctoi 'E' = 14
ctoi 'f' = 15
ctoi 'F' = 15
ctoi _ = 0

parseBase : Int -> String -> Int
parseBase 0 str = 0
parseBase base str = 
  if base > 16 then
    0
  else
    parseBase' . reverse $ unpack str
      where
        parseBase' [] = 0
        parseBase' (c::cs) = ctoi c + parseBase' cs * base

showBase : Int -> Int -> String
showBase 0 num = "0"
showBase base 0 = "0"
showBase base num =
  if base > 16 then
    "0"
  else
    reverse $ showBase' num
      where
        showBase' 0 = "0"
        showBase' n = strCons val (showBase' $ div n base)
          where
            val =
              let rem = mod n base in
              if rem > 9 then
                chr $ rem - 10 + (ord 'a')
              else
                chr $ rem + (ord '0')

-- Main program.

rpn : List String -> List Double -> String
rpn [] ns = unwords . reverse $ map show ns
rpn ("+"::ss) (b::a::ns) = rpn ss (a+b::ns)
rpn ("-"::ss) (b::a::ns) = rpn ss (a-b::ns)
rpn ("*"::ss) (b::a::ns) = rpn ss (a*b::ns)
rpn ("/"::ss) (b::a::ns) = rpn ss (a/b::ns)
rpn ("^"::ss) (b::a::ns) = rpn ss ((pow a b)::ns)
rpn ("dup"::ss) s@(n::ns) = rpn ss (n::s)
rpn ("drop"::ss) (n::ns) = rpn ss ns
rpn ("swap"::ss) (b::a::ns) = rpn ss (a::b::ns)
rpn ("over"::ss) (b::a::ns) = rpn ss (a::b::a::ns)
rpn ("rot"::ss) (c::b::a::ns) = rpn ss (a::c::b::ns)
rpn ("-rot"::ss) (c::b::a::ns) = rpn ss (b::a::c::ns)
rpn ("nip"::ss) (b::a::ns) = rpn ss (b::ns)
rpn ("tuck"::ss) (b::a::ns) = rpn ss (b::a::b::ns)
rpn ("pick"::ss) (n::ns) with (index' (toNat (cast {to=Int} n)) ns)
  | Just x = rpn ss (x::ns)
  | Nothing = "Error: 'pick' went out of bounds."
rpn ("clear"::ss) ns = rpn ss []
rpn ("depth"::ss) ns = rpn ss ((cast {to=Double} (toIntNat $ L.length ns))::ns)
rpn ("+"::ss) s = "Error: '+' is invalid."
rpn (s::ss) ns with (parseDouble s)
  | Just n = rpn ss (n::ns)
  | Nothing = "Error: '" ++ s ++ "' is unrecognized or there is insufficient stack usage."

quote : Int -> String
quote 0 = "WindowsNT: Ryzen can run an infinite loop in 3 seconds"
quote 1 = "TimBread27: enigma balls"
quote 2 = "Borb: god yoinketh and god yeeteth away"
quote 3 = "Optimo: good old black and white (mostly white) family sitcoms"
quote 4 = "tokumei: it is not weeb it is actually common japanese"
quote 5 = "chibill: also the more immutable values you have the worse peformance unless it does an so odd hack to get around the slow down to get memory or cache. Or does it just replace all instances with hard coded values? But probably not what it does."
quote 6 = "kuki: anyone has a small hex incrementer?"
quote 7 = "DeCapsler258: if u go in RF chat and say 'Haskell is crapskell' 3 times, u will summon Voltz and he will yell at u"
quote 8 = "tyler: ur mom really made a killing before she went to jail for murder"
quote 9 = "thooomas: i need immediate help from ppl who can bild compuyer and typ like dis 2"
quote 10 = "TheCreatorJSA: rust is mozillas versoin of c++ right?"
quote 11 = "Dorkalert2211: my mission is to bring decency to the server"
quote 12 = "inspector95: rilly i can make a redstone computer that will have vidio games in it"
quote 13 = "QuantumDeveloper: i don't have 2 carry in's sorry"
quote 14 = "Hastumer: NEW LOGIC GATE!"
quote 15 = "gangsterlx: but yeah complex numbers but except if u go higher then 64 it aint possible? :P"
quote 16 = "Xav101: 'fake nick, fake dick, burn the heretic'"
quote 17 = "eevv: so to fix this, we got rid of the memory ~stallman 2k17"
quote 18 = "Cassiboy_NL_16: i go now to my real and delete you"
quote 19 = "Quavo_Migos: eevv is a beast"
quote 20 = "noodlot_arrain: chan can you accept my app"
quote 21 = "michaelbuerger: I love eating LGBTs"
quote 22 = "Koyarno: im too famous for that"
quote 23 = "JesseFrostMiner: so what's best language for C++?"
quote 24 = "MetalTech: why has everyone comed up with ideas before me?!?!"
quote 25 = "tyler: a implies b means b nimpllies a"
quote 26 = "tyler: 'you must provide a blood sample and be able to build an rca alu in 15 seconds from scratch blindfolded'"
quote 27 = "Digi: staff fucks robots"
quote 28 = "TheLightning1995: grill fills good cuz it has clinton rub"
quote 29 = "Decapo (formerly known as Nickster) joined the game"
quote 30 = "BigPig: if i were a white girl, id slurp you down like a pumpkin spice latte"
quote 31 = "DeadMemez: no i havent applied yet, i got drunk 9 months ago and forgot all redstone knowledge I had"
quote 32 = "Josh: whats ur most diagonal cca"
quote 33 = "obol2: in wahch language is raspary pi? linux?"
quote 34 = "Magic :^): carry cancer ladder"
quote 35 = "ElegaardReds: why is there a red torch and a yellow torch?"
quote 36 = "reepeerc709: how 2 unblock"
quote 37 = "eevv: strong like strong korean man"
quote 38 = "memeko: plz plz can i have op"
quote _ = "OK"

help : String -> String
help "ping" = "Returns 'pong'."
help "say" = "Says the given args. Example: #say Hello!"
help "yell" = "Says the given args, but in uppercase. Example: #yell Hello!"
help "swedish" = "Says the given args, but in swedish. Example: #swedish Hello!"
help "yellswedish" = "Says the given args, but in uppercase swedish. Example: #yellswedish Hello!"
help "spanish" = "Says the given args, but in spanish. Example: #spanish Hello!"
help "yellspanish" = "Says the given args, but in uppercase spanish. Example: #yellspanish Hello!"
help "whoami" = "Says your username."
help "rpn" = "An RPN evaluator. Supports: '+', '-', '*', '/', '^', 'dup', 'drop', 'swap', 'over', 'rot', '-rot', 'nip', 'tuck', 'pick', 'clear', 'depth'. Example: #rpn 2 2 +"
help "quote" = "Say a quote. Example: #quote 37"
help "baseconv" = "Convert bases. Example: #baseconv 10 2 6"
help "monad" = "They're just monoids in the category of endofunctors. What's the problem?"
help x = "Commands: say, yell, swedish, yellswedish, spanish, yellspanish, whoami, rpn, quote, baseconv"

runCmd : String -> String -> String -> String -> IO String
runCmd "Debug" _ _ _ = pure "OK"
runCmd _ _ "#ping" args = pure "pong"
runCmd _ _ "#say" args = pure args
runCmd _ _ "#yell" args = pure $ toUpper args
runCmd _ _ "#swedish" args = pure $ pack . intersperse 'f' . unpack $ args
runCmd _ _ "#yellswedish" args = pure $ pack . intersperse 'F' . unpack $ toUpper args
runCmd _ _ "#spanish" args = pure $ unwords . map (++ "o") . words $ args
runCmd _ _ "#yellspanish" args = pure $ unwords . map (++ "O") . words $ toUpper args
runCmd _ sender "#whoami" _ = pure sender
runCmd _ _ "#rpn" args = pure $ rpn (words args) []
runCmd _ _ "#quote" args with (parsePositive {a = Int} args)
  | Just x = pure $ quote x
  | Nothing = pure "OK"
runCmd _ _ "#baseconv" args =
  let argList = words args in
  if length argList >= 3 then
    let Just fromArg = index' 0 argList in
    let Just toArg = index' 1 argList in
    let Just num = index' 2 argList in
    let from = unwrapMaybe 0 $ parsePositive {a = Int} fromArg in
    let to = unwrapMaybe 0 $ parsePositive {a = Int} toArg in
    pure $ showBase to . parseBase from $ num
  else
    pure "Insufficient arguments."
runCmd _ _ "#help" args = pure $ help args
runCmd _ _ "creeper" "" = pure "no"
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
    if out == "OK" then do
      putStrLn out
    else do
      putStrLn $ "=> " ++ out
    fflush stdout
    main
  else do
    main
