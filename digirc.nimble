# Package

version       = "0.1.0"
author        = "davidgarland"
description   = "A custom IRC client."
license       = "MIT"
srcDir        = "src"
bin           = @["digirc"]

# Dependencies

requires "nim >= 0.20.0"
requires "irc"

# Tasks

task clean, "Cleans up files.":
  exec "rm -f digirc backend src/*.ibc"
