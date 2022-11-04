Rebar3 proto plugin
=====

A rebar plugin for generating proto info.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```erlang
    {plugins, [
        {rebar3_proto_plugin, {git, "https://host/user/rebar3_proto_plugin.git", {tag, "0.1.0"}}}
    ]}.
```

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_proto_plugin
    ===> Fetching rebar3_proto_plugin
    ===> Compiling rebar3_proto_plugin
    <Plugin Output>
