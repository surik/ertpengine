# Sipwie ertpengine client in Erlang

## Installation (rebar.config):

    {deps, [
    	{ertpengine, ".*", {git, "https://github.com/surik/ertpengine.git", "master"}}
    ]}.

## Usage:

    ertpengine:new_connection(local, [{ip, "127.0.0.1"}, {port, 11234}]),
    pong = ertpengine:ping(local),
    ertpengine:stop_coonection(local).

See ertpengine.erl for information.