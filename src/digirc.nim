#[
## digirc.nim | DigIrc | Main IRC daemon.
## https://github.com/davidgarland/digirc
]# 

import
  strutils,
  streams,
  options,
  osproc

import
  irc

# Define a type to store messages.

type
  OreServ = enum
    OreNone    = "None    | ",
    OreDebug   = "Debug   | ",
    OreNote    = "Note    | ",
    OreNetwork = "Network | ",
    OreDiscord = "Discord | ",
    OreIrc     = "IRC     | ",
  OreMsg = object
    server: OreServ
    sender: string
    message: string

# Parse events into messages.

proc eventOreMsg*(e: IrcEvent): OreMsg =
  let param = e.params[e.params.high]

  case e.nick:
  of ["ORENetwork", "OREDiscord"]:
    result.server = if e.nick == "ORENetwork": ORENetwork else: OREDiscord
    if not param.contains(':'):
      result.server = OreNote
      result.sender = ""
      if param.high >= 3:
        result.message = param[3 .. param.high]
      else:
        result.message = ""
    else:
      let split = param.split(':', maxSplit = 1)
      result.sender = split[0][3 ..< split[0].high]
      result.message = split[1][1 .. split[1].high]
  elif e.nick.isNilOrWhitespace:
    result.server = OreDebug
    result.sender = ""
    result.message = param
  else:
    result.server = OreIrc
    result.sender = e.nick
    result.message = param

proc `$`(msg: OreMsg): string =
  result = ""
  result &= $msg.server
  if not msg.sender.isNilOrWhitespace:
    result &= msg.sender & ": "
  result &= msg.message

# Client startup.

let room = "#openredstone"
var client = newIrc("irc.esper.net", nick = "digirc", joinChans = @[room])
client.connect()

# Backend program startup.

discard execCmd "rm -f ./backend"
discard execCmd "idris --O2 src/backend.idr -o backend"

# Frontend command handling.

proc oreMsgCommand(msg: OreMsg) =
  let parts = msg.message.split(maxsplit = 1)
  case parts[0]:
  of "#reload":
    if msg.sender == "Digi":
      discard execCmd "idris --O2 src/backend.idr -o backend"
  of "#type":
    if parts.len < 2:
      return
    var (line, errc) = execCmdEx("mueval --inferred-type -T --expression " & parts[1].quoteShell & " +RTS -N2 -RTS")
    let lins = line.strip.split(seps = Newlines)
    line = lins[lins.high]
    line = msg.sender & " => " & line.strip
    echo "Backend | ", line
    client.privmsg(room, line)
  of "#eval":
    if parts.len < 2:
      return
    var (line, ercc) = execCmdEx("mueval --expression " & parts[1].quoteShell & " +RTS -N2 -RTS")
    line = msg.sender & " => " & line.strip
    echo "Backend | ", line
    client.privmsg(room, line)
  else:
    if (not ($msg).isNilOrWhitespace) and (msg.server notin [OreNote, OreDebug]):
      var (line, errc) = execCmdEx("./backend " & ($msg).quoteShell)
      line = line.strip
      if line != "OK":
        line = msg.sender & " " & line
        echo "Backend | ", line
        client.privmsg(room, line)

# Main program.

var event: IrcEvent
while true:
  if client.poll(event, timeout = 1):
    case event.typ:
    of EvDisconnected, EvTimeout:
      client.reconnect()
    of EvMsg:
      let msg = event.eventOreMsg
      echo msg
      msg.oreMsgCommand
    else:
      var blank: OreMsg
      blank.server = OreNone
      blank.sender = ""
      blank.message = ""
      blank.oreMsgCommand
      discard
