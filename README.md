Rebar3 proto plugin
=====

A rebar plugin for generating module `proto_info`, which provides frequently-used functions when using [gpb](https://github.com/tomas-abrahamsson/gpb) flow.

This plugin cooperates with [rebar3_gpb_plugin](https://github.com/lrascao/rebar3_gpb_plugin), [Configure](https://github.com/lrascao/rebar3_gpb_plugin#usage-with-umbrella-projects) it first.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```erlang
    {project_plugins, [
        {rebar3_gpb_plugin, "2.22.1"},
        {rebar3_proto_plugin, "0.1.1"}
    ]}.
```

Config the `proto_opts` and `provider_hooks`:

```erlang
    {proto_opts, [
                  {o_meta_file, "proto_info.meta"},
                  {o_proto_info, "src/proto_info.erl"}
                 ]}.


    {provider_hooks, [
        {pre, [
               {compile, {protobuf, compile}},
               {clean, {protobuf, clean}}
              ]},
        {post, [
                {compile, {proto, generate}}
               ]
        }
    ]}.
```

