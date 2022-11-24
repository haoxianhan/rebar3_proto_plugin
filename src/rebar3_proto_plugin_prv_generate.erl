-module(rebar3_proto_plugin_prv_generate).

% -include_lib("syntax_tools/include/merl.hrl").

-export([
         generate/2
        ]).

generate(AppInfo, State) ->
    {ok, Meta} = generate_meta_file:generate(AppInfo, State),
    MetaList = to_list(Meta),
    ok = generate_proto_info:generate(AppInfo, State, MetaList),
    ok.


to_list(Meta) ->
    {_CodeCount, Meta0} = maps:take(code_count, Meta),
    lists:keysort(1, [{{msg_name, MsgName},
                       {msg_code, MsgCode},
                       {pt_moduel, PbModule}}
                      || {MsgName, #{msg_code:=MsgCode, pb_module:=PbModule}} <- maps:to_list(Meta0)]).
