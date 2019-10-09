import System
import Prelude.List as L
import Prelude.Strings as S
import Data.String

-- Helper functions.

composeN : List (a -> a) -> (a -> a)
composeN = foldr (.) id

strDrop : Nat -> String -> String
strDrop n = (\s => substr n (length s) s)

repBy : String -> String -> (String -> String)
repBy a b = (\x => if x == a then b else x)

multiReplace : List (String -> String) -> List String -> List String
multiReplace rs = map (composeN rs) 

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
quote 3 = "O_ptimo: good old black and white (mostly white) family sitcoms"
quote 4 = "tokumei: it is not weeb it is actually common japanese"
quote 5 = "chibill: also the more immutable values you have the worse peformance unless it does an so odd hack to get around the slow down to get memory or cache. Or does it just replace all instances with hard coded values? But probably not what it does."
quote 6 = "kuki: anyone has a small hex incrementer?"
quote 7 = "DeCapsler258: if u go in RF chat and say 'Haskell is crapskell' 3 times, u will summon Voltz and he will yell at u"
quote 8 = "t_yler: ur mom really made a killing before she went to jail for murder"
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
quote 20 = "noodlot_arrain: c_han can you accept my app"
quote 21 = "michaelbuerger: I love eating LGBTs"
quote 22 = "Koyarno: im too famous for that"
quote 23 = "JesseFrostMiner: so what's best language for C++?"
quote 24 = "MetalTech: why has everyone comed up with ideas before me?!?!"
quote 25 = "p_auk: puak takes te topic a gain !"
quote 26 = "t_yler: 'you must provide a blood sample and be able to build an rca alu in 15 seconds from scratch blindfolded'"
quote 27 = "SealLovah: koy's design is making me want to commit shrimp flavored grave"
quote 28 = "TheLightning1995: grill fills good cuz it has clinton rub"
quote 29 = "Decapo (formerly known as N_ickster) joined the game"
quote 30 = "BigPig: if i were a white girl, id slurp you down like a pumpkin spice latte"
quote 31 = "DeadMemez: no i havent applied yet, i got drunk 9 months ago and forgot all redstone knowledge I had"
quote 32 = "Josh: whats ur most diagonal cca"
quote 33 = "obol2: in wahch language is raspary pi? linux?"
quote 34 = "Magic :^): carry cancer ladder"
quote 35 = "ElegaardReds: why is there a red torch and a yellow torch?"
quote 36 = "reepeerc709: how 2 unblock"
quote 37 = "eevv: strong like strong korean man"
quote 38 = "HyperXti: Ev: I say very advanced words to make me look chlorophyll."
quote 39 = "Neogreenyew: time is money; and money is happiness"
quote 40 = "Neogreenyew: the power of christ rappels you"
quote 41 = "konsumlamm: sigma balls"
quote 42 = "n_ickster: paukkupalikka more like paukkupaligma"
quote 43 = "Nemes left the game; Tukeque: aaaaaaaaand she's gone"
quote 44 = "EEVV: i Reside In The s T a T e S.... ! voltz: g a s p"
quote 45 = "Nielsapie: im here on dpol and i dont see any bud ram MY GOD PLZ I WANT BUD RAM"
quote 46 = "ExApollo: OH NO I CANT HEAR YOU I HAVE AIRPODS IN"
quote 47 = "Claminuts: haskell more like ask hell"
quote _ = "OK"

help : String -> String
help "ping" = "Returns 'pong'."
help "say" = "Says the given args. Example: #say Hello!"
help "yell" = "Says the given args, but in uppercase. Example: #yell Hello!"
help "swedish" = "Says the given args, but in swedish. Example: #swedish Hello!"
help "yellswedish" = "Says the given args, but in uppercase swedish. Example: #yellswedish Hello!"
help "spanish" = "Says the given args, but in spanish. Example: #spanish Hello!"
help "yellspanish" = "Says the given args, but in uppercase spanish. Example: #yellspanish Hello!"
help "aesthetic" = "Says the given args, but in aesthetic. Example: #aesthetic Hello!"
help "mock" = "Mocks the given args. Example: #mock Hello!"
help "whoami" = "Says your username."
help "rpn" = "An RPN evaluator. Supports: '+', '-', '*', '/', '^', 'dup', 'drop', 'swap', 'over', 'rot', '-rot', 'nip', 'tuck', 'pick', 'clear', 'depth'. Example: #rpn 2 2 +"
help "quote" = "Say a quote. Example: #quote 46"
help "rip" = "RIP a user. Example: #rip Digitalis"
help "eval" = "Evaluate a haskell expression's value. Example: #eval fmap (+ 1) [1, 2, 3]"
help "type" = "Evaluate a haskell expression's type. Example: #eval fmap (+ 1)"
help "monad" = "They're just monoids in the category of endofunctors. What's the problem?"
help x = "Commands: ping, say, yell, swedish, yellswedish, spanish, yellspanish, aesthetic, mock, whoami, rpn, quote, rip, eval, type"

mock : List Char -> List Char
mock (a::b::cs) = toLower a :: toUpper b :: mock cs
mock (a::[]) = toLower a :: []
mock [] = []

qedReps : List (String -> String)
qedReps = [ repBy "\\empty" "Ø "
          , repBy "\\in" "∈ "
          , repBy "\\notin" "∉ "
          , repBy "\\union" "⋃ "
          , repBy "\\cup" "⋃ "
          , repBy "\\intersection" "⋂ "
          , repBy "\\cap" "⋂ "
          , repBy "\\subset" "⊂ "
          , repBy "\\subseteq" "⊆ "
          , repBy "\\proves" "⊢ "
          , repBy "\\qed" "∎ "
          , repBy "\\exists" "∃"
          , repBy "\\forall" "∀"
          , repBy "\\bottom" "⊥ "
          , repBy "\\top" "⊤ "
          , repBy "\\xor" "⊕ "
          , repBy "\\or" "∨ "
          , repBy "\\and" "∧ "
          , repBy "\\not" "¬"
          , repBy "\\to" "→ "
          , repBy "\\gamma" "Γ "
          , repBy "\\lambda" "λ"
          , repBy "\\mu" "μ "
          , repBy "\\int" "∫ "
          , repBy "\\cint" "∮ "
          , repBy "\\real" "ℝ "
          , repBy "\\rational" "ℚ "
          , repBy "\\natural" "ℕ "
          , repBy "\\integer" "ℤ "
          , repBy "\\equiv" "⇔ "
          ]

qed : String -> String
qed = pack . qed' [] . unpack . (++ " ")
  where qed' : List Char -> List Char -> List Char
        qed' [] ('\\'::cs) = qed' ['\\'] cs 
        qed' [] (c::cs) = c :: qed' [] cs
        qed' w@(s::ss) (' '::cs) = (unpack (composeN qedReps $ pack w)) ++ qed' [] cs
        qed' w@(s::ss) (')'::cs) = (unpack (composeN qedReps $ pack w)) ++ [')'] ++ qed' [] cs
        qed' w@(s::ss) ('}'::cs) = (unpack (composeN qedReps $ pack w)) ++ ['}'] ++ qed' [] cs
        qed' w@(s::ss) (c::cs) = qed' (w ++ [c]) cs
        qed' _ _ = []

runCmd : String -> String -> String -> String -> IO String
runCmd "Debug" _ _ _ = pure "OK"
runCmd _ _ "#ping" args = pure "pong"
runCmd _ _ "#hello" _ = pure "Hello allo"
runCmd _ _ "#say" args = pure args
runCmd _ _ "#yell" args = pure $ toUpper args
runCmd _ _ "#swedish" args = pure $ unwords . map (pack . intersperse 'f' . unpack) . words $ args
runCmd _ _ "#yellswedish" args = pure $ unwords . map (pack . intersperse 'F' . unpack) . words $ toUpper args
runCmd _ _ "#spanish" args = pure $ unwords . map (++ "o") . words $ args
runCmd _ _ "#yellspanish" args = pure $ unwords . map (++ "O") . words $ toUpper args
runCmd _ _ "#aesthetic" args = pure $ unwords . map singleton $ unpack args
runCmd _ _ "#mock" args = pure . pack . mock $ unpack args
runCmd _ _ "#spongebob" args = pure . pack . mock $ unpack args
runCmd _ _ "#thank" args = pure $ "Thank You " ++ args ++ ", Very Cool!"
runCmd _ _ "#qed" args = pure . qed $ args
runCmd _ sender "#whoami" _ = pure sender
runCmd _ _ "#rpn" args = pure $ rpn (words args) []
runCmd _ _ "#quote" args with (parsePositive {a = Int} args)
  | Just x = pure $ quote x
  | Nothing = pure "OK"
runCmd _ _ "#rip" args =
  if (fromNat $ length args) < 3 then
    pure $ substr 0 (length args) "rip"
  else
    pure $ "rip" ++ strDrop 3 args
runCmd _ _ "#help" args = pure $ help args
runCmd _ _ a bs = pure "OK"

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
  args <- getArgs
  let line = unwords $ drop 1 args
  out <- issueCmd line
  if out == "OK" then do
    putStrLn out
  else do
    putStrLn $ "=> " ++ out
