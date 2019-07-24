import
  strformat,
  sequtils,
  strutils,
  options,
  tables,
  sugar,
  math,
  os

func getParser(s: string): string -> int =
  if   s.contains('x'): parseHexInt
  elif s.contains('b'): parseBinInt
  else:                 parseInt

proc rpn(expr: string): seq[int] =
  var stk: seq[int]
  for word in expr.split:
    let parser = word.getParser
    try:
      let val = parser word
      stk &= val
    except:
      case word:
      of "+":
        if stk.len < 2: break
        stk[stk.high - 1] += stk[stk.high]
        stk.del(stk.high)
      of "-":
        if stk.len < 2: break
        stk[stk.high - 1] -= stk[stk.high]
        stk.del(stk.high)
      of "*":
        if stk.len < 2: break
        stk[stk.high - 1] *= stk[stk.high]
        stk.del(stk.high)
      of "/":
        if stk.len < 2: break
        stk[stk.high - 1] = stk[stk.high - 1] div stk[stk.high]
        stk.del(stk.high)
      of "%":
        if stk.len < 2: break
        stk[stk.high - 1] = stk[stk.high - 1] mod stk[stk.high]
        stk.del(stk.high)
      of "^":
        if stk.len < 2: break
        stk[stk.high - 1] = stk[stk.high - 1] ^ stk[stk.high]
        stk.del(stk.high)
      of "nip":
        if stk.len < 2: break
        stk.del(stk.high - 1)
      of "tuck":
        if stk.len < 2: break
        let a = stk[stk.high - 1]
        stk.del(stk.high - 1)
        stk &= a
        stk &= stk[stk.high - 1]
      of "over":
        if stk.len < 2: break
        stk &= stk[stk.high - 1]
      of "swap", "swp":
        if stk.len < 2: break
        let c = stk[stk.high]
        stk[stk.high] = stk[stk.high - 1]
        stk[stk.high - 1] = c
      of "rot":
        if stk.len < 3: break
        let a = stk[stk.high - 2]
        stk[stk.high - 2] = stk[stk.high - 1]
        stk[stk.high - 1] = stk[stk.high]
        stk[stk.high] = a
      of "dup":
        if stk.len < 1: break
        stk &= stk[stk.high]
      of "drop":
        if stk.len < 1: break
        stk.del(stk.high)
      else:
        discard
  result = stk

proc runCmd*(network, sender, message, cmd, args: string): string =
  case cmd:
  of "#say": args
  of "#lol": "lol"
  of "#rpn": $args.rpn
  else:      "OK"

proc issueCmd*(line: string): string =
  let barSplit = line.split("| ", maxSplit = 1)
  let network = barSplit[0].strip
  if network == "Debug": return "OK"
  let colonSplit = barSplit[1].split(": ", maxSplit = 1)
  let sender = colonSplit[0].strip
  let message = if colonSplit.len > 1: colonSplit[1].strip else: ""
  let input = message.split(maxSplit = 1)
  let cmd = input[0]
  let args = if input.len > 1: input[1] else: ""
  result = runCmd(network, sender, message, cmd, args)
 
while true:
  for line in stdin.lines:
    echo line.issueCmd
  sleep 20
