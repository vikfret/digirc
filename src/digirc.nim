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
      result.message = param[3 .. param.high]
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

discard execCmd "rm -f src/backend"
discard execCmd "nim c -d:release src/backend.nim"
var backend = startProcess("./src/backend")

# Frontend command handling.

proc oreMsgCommand(msg: OreMsg) =
  case msg.message:
  of "#reload":
    if msg.sender == "Digi":
      discard execCmd "rm -f src/backend"
      let success = execCmd "nim c src/backend.nim"
      if success == 0:
        backend.close()
        backend = startProcess("./src/backend")
  else:
    if not ($msg).isNilOrWhitespace:
      backend.inputStream.writeLine $msg
      backend.inputStream.flush
      while not backend.hasData:
        discard
      let line = backend.outputStream.readLine
      case line:
      of "OK":
        discard
      else:
        echo "Backend | ", line
        client.privmsg(room, line)

# Main program.

var event: IrcEvent
while true:
  if client.poll(event):
    case event.typ:
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
