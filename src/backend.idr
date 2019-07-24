import Prelude.List as L
import Prelude.Strings as S

record Command where
  constructor MakeCommand
  origin : Maybe String
  sender : Maybe String
  cmd    : Maybe String
  args   : Maybe (List String)

runCmd : Command -> String
runCmd (MakeCommand (Just "Debug") _ _ _) = "OK"
runCmd (MakeCommand _ _ (Just "#say") (Just args)) = concat args
runCmd (MakeCommand _ _ (Just "#haskell") (Just args)) =
runCmd _ = "OK"

issueCmd : String -> String
issueCmd s =
  let barSplit = S.split (== '|') s in
  let origin = L.head' barSplit in
  let colonSplit = S.split (== ':') . concat . lowerMaybe $ L.tail' barSplit in
  let sender = L.head' colonSplit in
  let message = concat . lowerMaybe $ L.tail' colonSplit in
  let wordSplit = words message in
  let cmd = L.head' wordSplit in
  let args = L.tail' wordSplit in
  runCmd $ MakeCommand origin sender cmd args

main : IO ()
main = do
  line <- getLine
  putStrLn $ issueCmd line
  fflush stdout
  main
