import Prelude.List as L
import Prelude.Strings as S

runCmd : Maybe String -> Maybe String -> Maybe String -> Maybe (List String) -> String
runCmd (Just "Debug") _ _ _ = "OK"
runCmd _ _ (Just "#say") (Just args) = concat args
runCmd _ _ (Just "#haskell") (Just args) = "to be implemented"
runCmd _ _ _ _ = "OK"

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
  runCmd origin sender cmd args

main : IO ()
main = do
  line <- getLine
  putStrLn $ issueCmd line
  fflush stdout
  main
