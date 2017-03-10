Basho's fork of MochiWeb, an Erlang library for building lightweight HTTP servers.

[![Build Status](https://secure.travis-ci.org/basho/mochiweb.png?branch=master)](http://travis-ci.org/basho/mochiweb)

The canonical version of MochiWeb is available at http://github.com/mochi/mochiweb

## Generating and mochiweb application skeleton

Basho's mochiweb uses `rebar3` and the mochiweb application skeleton is generated using a rebar template. `rebar3` requires user defined templates be placed in a global directory `~/.config/rebar3/templates`. To install the template, copy the contents of `support/template` directory to `~/.config/rebar3/templates/<Dir>`, note: `~/.config/rebar3` may not exist, and then create an application skeleton.

### Installing mochiweb templates

```shell
mkdir -p ~/.config/rebar3/templates/basho_mochiweb/
cp -fpr support/templates/* ~/.config/rebar3/templates/basho_mochiweb/
cp -fpr support/templates/.* ~/.config/rebar3/templates/basho_mochiweb/
```

### Compiling and starting

Now you can create a application skeleton using,

`rebar3 new mochiwebapp appid=<name> dest=<path>`

and compile and run it like

```shell
cd <path>
rebar3 compile
./start-dev.sh
```
### See also

https://www.rebar3.org/docs/using-templates
