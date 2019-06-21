import
  strformat,
  sequtils,
  strutils,
  tables,
  os

var trusted = {
  "Digi": true,
  "Digitalis": true
}.toTable
var muted = false
var say = ""

proc runCmd(argv: seq[string]) =
  if (argv[0] in trusted) and trusted[argv[0]]:
    if argv.len < 2: return
    say = ""
    if argv[1] == "#unmute":
      if argv[0] notin ["Digi", "Digitalis"]:
        return
      muted = false
    if muted:
      return
    case argv[1]:
    of "#mute":
      if argv[0] notin ["Digi", "Digitalis"]:
        return
      muted = true
    of "#say":
      if argv.len < 3: return
      for arg in argv[2 .. argv.high]:
        say &= arg & " "
    of "#rpn":
      if argv.len < 3: return
      var
        stk: seq[int] = @[]
        is_int: bool
        n: int
      for arg in argv[2 .. argv.high]:
        if arg.contains("x"):
          try:
            n = parseHexInt arg
            is_int = true
          except ValueError:
            is_int = false
        elif arg.contains("b"):
          try:
            n = parseBinInt arg
            is_int = true
          except ValueError:
            is_int = false
        else:
          try:
            n = parseInt arg
            is_int = true
          except ValueError:
            is_int = false
        if is_int:
          stk &= n
        else:
          case arg:
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
          of "dup":
            if stk.len < 1: break
            stk &= stk[stk.high]
          else:
            discard
      for sub in stk:
        say &= $sub & " "
    of "#trusted":
      for k, v in trusted.pairs:
        if v:
          say &= k & " "
    of "#untrusted":
      for k, v in trusted.pairs:
        if not v:
          say &= k & " "
    of "#trust":
      if argv.len < 3: return
      if argv[0] notin ["Digi", "Digitalis"]: return
      trusted[argv[2]] = true
    of "#untrust":
      if argv.len < 3: return
      if argv[0] notin ["Digi", "Digitalis"]: return
      trusted[argv[2]] = false
    if say == "":
      say = "OK"

proc toCmd*(s: string): seq[string] =
  result = @[]
  for sub in s.tokenize:
    if not sub.isSep:
      result &= sub.token

while true:
  for line in stdin.lines:
    say = "OK"
    let sa = line.split(" | ", maxSplit = 1)
    let serv = sa[0].strip
    if (serv notin ["Debug", "Note"]) and sa[1].contains(":"):
      let cont = sa[1].split(":", maxSplit = 1)
      let rawCmd = cont[0] & cont[1]
      runCmd(rawCmd.toCmd)
    echo say
  sleep 20
