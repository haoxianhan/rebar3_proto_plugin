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
    #{message := MessageMap} = Meta,
    Sorted = lists:keysort(1, maps:to_list(MessageMap)),
    [#{msg_name => MsgName,
       msg_code => MsgCode,
       pb_module => PbModule} || {MsgName, #{msg_code:=MsgCode, pb_module:=PbModule}} <- Sorted].
