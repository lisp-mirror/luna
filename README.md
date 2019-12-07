# Project shelved

This project is no longer being worked on because [mjolnir](https://github.com/matrix-org/mjolnir) is now a thing which has the same scope and is bankrolled by matrix.org

That being said the contents of luna.framework is just a bot framework and is technically still being used by gatekeeper and if I ever make another matrix bot, it will probably have to use luna.framework too.

Unfortunately cl-matrix has some problems that do need dealing with too and I don't really know how much time I can commit to fixing those

# luna
Extensible group management bot for matrix (matrix.org) to aid moderation of rooms and spam removal.
Luna is a group management bot for matrix, however it contains a framework that can be used to make a matrix bot to do anything. [doc](https://gnuxie.gitlab.io/luna/)

See the luna.framework system (in this repo) and also see [cl-matrix](https://gitlab.com/Gnuxie/cl-matrix).

**The library is still WIP and is subject to change, it is however usable and there is some documentation on how to use luna.framework/extend luna.**

When I say subject to change, I mean there could be pretty big changes without
warning.

If you get stuck you can talk to me in `#cl-matrix:matrix.org`

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

If you're using the startup script you'll also want:

```
git clone https://gitlab.com/Gnuxie/dunno.git
```

### starting

#### for lispers

If you're a lisper then I recommend you just use `make-luna-kernal` then `start-luna`,
or follow this example [config](https://gitlab.com/Gnuxie/luna/blob/master/example-config.lisp)
that you can probably just use but it's pretty meh.

#### script

If you need to have some kind of script to start luna with then there's [this](https://gitlab.com/Gnuxie/luna/blob/master/scripts/start-bot-args.lisp)
(well [this](https://gitlab.com/Gnuxie/luna/blob/master/scripts/start-bot.sh)).

You will first need to install [dunno](https://gitlab.com/Gnuxie/dunno/).

```
[ scripts]$ ./start-bot.sh --help
helper script to start and run luna.

Usage: start-bot.sh [-h|--help] [-c|--config CONFIG] [MODULES...]

Available options:
  -h, --help               Display this help message
  -c, --config CONFIG      A file to load for the config

You should pass which modules you want the framework to load, and they will be loaded into the image before luna starts (so that their hooks, command parsers etc are available to the listener).
This script is not required to operate luna.
```

#### configuration

The config file used by the script is a config defined by [dunno](https://gitlab.com/Gnuxie/dunno/)

if you are using the script you can find an example config file [here](https://gitlab.com/Gnuxie/luna/tree/master/scripts/config.lisp)
The configuration options are described here [here](https://gitlab.com/Gnuxie/luna/tree/master/scripts/start-bot-args.lisp#L25)

Any additional option can just be added to the alist. 


## Documentation
Internal and framework doc can be viewed [here](https://gnuxie.gitlab.io/luna/)

There's some information about the state events in [this](https://gitlab.com/Gnuxie/luna/tree/master/doc) folder too.


## License

This code is political.

    Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>

	NON-VIOLENT PUBLIC LICENSE v1
	https://git.pixie.town/thufie/NPL

