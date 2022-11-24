-module(generate_proto_info).

-export([
         generate/3,
         test/0
        ]).


generate(AppInfo, _State, MetaList) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, ProtoOpts} = dict:find(proto_opts, Opts),
    ProtoInfoFile = proplists:get_value(o_proto_info, ProtoOpts, "proto_info.erl"),
    OutProtoInfo = filename:join([AppDir, ProtoInfoFile]),

    generate_proto_info(MetaList, OutProtoInfo),
    ok.


generate_proto_info(MetaList, OutProtoInfo) ->
    ModuleName = erlang:list_to_atom(filename:rootname(filename:basename(OutProtoInfo))),
    Module = generate_module(ModuleName, MetaList),
    Formatted = erl_prettypr:format(Module),
    ok = file:write_file(OutProtoInfo, Formatted),
    ok.

generate_module(ModName, MetaList) ->
    Mod = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(ModName)]),
    ExportList = [ erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(1))
                    || Fun <- [get_msg_name, get_msg_code, get_msg_pbmodule] ],
    Export = erl_syntax:attribute(erl_syntax:atom(export),
                                   [erl_syntax:list(ExportList)]),
    Clauses = lists:append([Fun(MetaList) || Fun <- [fun generate_get_msg_name/1,
                                                     fun generate_get_msg_code/1,
                                                     fun generate_get_msg_pbmodule/1]]),

    erl_syntax:form_list([Mod, Export|Clauses]).

%% get_msg_name(MsgCode) -> MsgName.
generate_get_msg_name(MetaList) ->
    Name = erl_syntax:atom(get_msg_name),
    Clauses = [ erl_syntax:clause([erl_syntax:integer(MsgCode)], none, [erl_syntax:atom(MsgName)])
                || {{msg_name, MsgName},
                    {msg_code, MsgCode},
                    {pb_module, _PbModule}} <- MetaList ],
    AlwaysMatch = generate_clause_match_all(),
    [ erl_syntax:function(Name, Clauses ++ [AlwaysMatch]) ].

%% get_msg_code(MsgName) -> MsgCode.
generate_get_msg_code(MetaList) ->
    Name = erl_syntax:atom(get_msg_code),
    Clauses = [ erl_syntax:clause([erl_syntax:atom(MsgName)], none, [erl_syntax:integer(MsgCode)])
                || {{msg_name, MsgName},
                    {msg_code, MsgCode},
                    {pb_module, _PbModule}} <- MetaList ],
    AlwaysMatch = generate_clause_match_all(),
    [ erl_syntax:function(Name, Clauses ++ [AlwaysMatch]) ].

%% get_msg_pbmodule(MsgCode) -> PbModule.
generate_get_msg_pbmodule(MetaList) ->
    Name = erl_syntax:atom(get_msg_pbmodule),
    Clauses = [ erl_syntax:clause([erl_syntax:integer(MsgCode)], none, [erl_syntax:atom(PbModule)])
                || [{msg_name, _MsgName},
                    {msg_code, MsgCode},
                    {pb_module, PbModule}] <- MetaList ],
    AlwaysMatch = generate_clause_match_all(),
    [ erl_syntax:function(Name, Clauses ++ [AlwaysMatch]) ].

generate_clause_match_all() ->
    erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(undefined)]).

test() ->
    List = [[{msg_name, list_to_atom("msg" ++ integer_to_list(X))},
             {msg_code, 1000+X},
             {pb_module, list_to_atom("pb_msg" ++ integer_to_list(X))}] || X <- lists:seq(1,8000)],
    {Micros, Res} = timer:tc(fun generate_proto_info/2, [List, "proto.erl"]),
    io:format("haoxian ~p~n", [{Micros, Res}]).

