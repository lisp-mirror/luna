# luna
Extensible group management bot for matrix (matrix.org) to aid moderation of rooms and spam removal.
Luna is a group management bot for matrix, however it contains a framework that can be used to make a matrix bot to do anything. [doc](https://gnuxie.gitlab.io/luna/)

See the luna.framework system (in this repo) and also see [cl-matrix](https://gitlab.com/Gnuxie/cl-matrix).

**The library is still WIP and is subject to change, it is however usable and there is some documentation on how to use luna.framework/extend luna.**

## Usage

This bot is mostly used for banning and cleaning up spam across a group of rooms in a more accessible way than a simple script.

Type `!luna help` to see a list of commands then for example `!luna help ban` to see the full details of the `ban` command.

### Preparing your rooms for luna

### 1. Set the power_level in the room for `luna.group` and `luna.soft_ban` state events. 

This can be done by using `/devtools` in riot -> `explore room state` -> `m.room.power_levels` -> edit.

Add them in the `events` section of `content` and be very careful because you can break your room if you do something silly.

### 2. Give luna the power level needed to send those events in the room (and to do anything else like ban people). 

As of writing there is no reason at all for luna to be over pl 50, however something like 60 is recommended if you don't want mods to be able to interfere with the luna state events.

## Installation

You'll want to make sure [quicklisp](https://www.quicklisp.org/beta/) is installed (and if you're completely new to CL, you'll probably want to install sbcl).
Then clone these repos into quicklisp local projects.

```
cd ~/quicklisp/local-projects/
git clone https://gitlab.com/Gnuxie/cl-matrix.git
git clone https://gitlab.com/Gnuxie/luna.git
```


### starting

#### for lispers

If you're a lisper then I recommend you just use `make-luna-kernal` then `start-luna`, or follow this example [config](https://gitlab.com/Gnuxie/luna/blob/master/example-config.lisp) that you can probably just use but it's pretty meh.

#### script

If you need to have some kind of script to start luna with then there's [this](https://gitlab.com/Gnuxie/luna/blob/master/scripts/start-bot-args.lisp) (well [this](https://gitlab.com/Gnuxie/luna/blob/master/scripts/start-bot.sh)).

Which can be started something like so

```

[ scripts]$ ./start-bot.sh --help
helper script to start and run luna.

Usage: start-bot.sh [-u|--username @<localpart>:<homeserver>] [-h|--help]
                    [-a|--access-token ACCESS-TOKEN] [-p|--password PASSWORD] [-s|--sync-rate FLOAT]
                    [-l|--log-output FILE] [--verbose-asdf] [--protocol PROTOCOL] [--port PORT]
                    [--hostname HOST] [MODULES...]

Available options:
  -u, --username @<localpart>:<homeserver>  Matrix user id for the bot
  -h, --help                                Display this help message
  -a, --access-token ACCESS-TOKEN           An access token to the user id
  -p, --password PASSWORD                   Password for the matrix user if you don't have an access token to supply
  -s, --sync-rate FLOAT                     The rate to poll sync in seconds
  -l, --log-output FILE                     A file to write the log to.
  --verbose-asdf                            show asdf output.
  --protocol PROTOCOL                       the protocol to use when communicating to the server, https by default.
  --port PORT                               The port to use when communicating to the server
  --hostname HOST                           The hostname to use when communicating to the server

You should pass what modules you want the framework to load (so if you wanted luna then just luna), and they will be loaded into the image before luna starts (so that their hooks, command parsers etc are available to the listener).
This script is not required to operate luna.

[ scripts]$ ./start-bot.sh --username @meow:clhs.gang --access-token MEOWWWWWWWWY -l log.loggo luna
```

## Documentation
Internal and framework doc can be viewed [here](https://gnuxie.gitlab.io/luna/)

There's some information about the state events in [this](https://gitlab.com/Gnuxie/luna/tree/master/doc) folder too.


## License

This code is political.

    Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>

	NON-VIOLENT PUBLIC LICENSE v1
	https://git.pixie.town/thufie/NPL

