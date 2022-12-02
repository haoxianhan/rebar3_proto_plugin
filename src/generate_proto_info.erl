-module(generate_proto_info).

-export([
         generate/3,
         test/0
        ]).


%% create proto_info.erl file
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
    ClausesMapsList = [ #{args => [MsgCode], return => MsgName}
                        || #{msg_name := MsgName, msg_code := MsgCode} <- MetaList],
    general_create_function(#{fun_name => get_msg_name,
                              clauses => ClausesMapsList}).

%% get_msg_code(MsgName) -> MsgCode.
generate_get_msg_code(MetaList) ->
    ClausesMapsList = [ #{args => [MsgName], return => MsgCode}
                        || #{msg_name := MsgName, msg_code := MsgCode} <- MetaList],
    general_create_function(#{fun_name => get_msg_code,
                              clauses => ClausesMapsList}).


%% get_msg_pbmodule(MsgCode) -> PbModule.
generate_get_msg_pbmodule(MetaList) ->
    ClausesMapsList = [ #{args => [MsgCode], return => PbModule}
                        || #{msg_code := MsgCode, pb_module := PbModule} <- MetaList],
    general_create_function(#{fun_name => get_msg_pbmodule,
                              clauses => ClausesMapsList}).



%% MFA, A always a list
general_create_function(#{fun_name:=FunName, clauses:=ClausesMapsList}) ->
    Name = erl_syntax:atom(FunName),
    Clauses = [ erl_syntax:clause(general_create_function_variable(Args), none, [general_create_function_variable(Return)])
                || #{args := Args,
                     return := Return} <- ClausesMapsList ],
    AlwaysMatch = erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(undefined)]),
    [ erl_syntax:function(Name, Clauses ++ [AlwaysMatch]) ].

%% conver variable
general_create_function_variable(Var) when is_atom(Var) ->
    erl_syntax:atom(Var);
general_create_function_variable(Var) when is_integer(Var) ->
    erl_syntax:integer(Var);
general_create_function_variable(Var) when is_list(Var) ->
    [general_create_function_variable(VarX) || VarX <- Var];
general_create_function_variable(Var) when is_tuple(Var) ->
    List = tuple_to_list(Var),
    List1 = general_create_function_variable(List),
    list_to_tuple(List1).



test() ->
    List = [#{msg_name => list_to_atom("msg" ++ integer_to_list(X)),
              msg_code => 1000+X,
              pb_module => list_to_atom("pb_msg" ++ integer_to_list(X))} || X <- lists:seq(1,8000)],
    {Micros, Res} = timer:tc(fun generate_proto_info/2, [List, "proto.erl"]),
    io:format("haoxian ~p~n", [{Micros, Res}]).

