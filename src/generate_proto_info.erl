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

    CustomInfoFile = proplists:get_value(custom_info, ProtoOpts, undefined),

    generate_proto_info(MetaList, OutProtoInfo, CustomInfoFile, AppDir),

    case proplists:get_value(proto_hrl, ProtoOpts, false) of
        true ->
            ProtoHrlFile = proplists:get_value(o_proto_hrl, ProtoOpts, "proto.hrl"),
            OutProtoHrl = filename:join([AppDir, ProtoHrlFile]),

            IsUppercase = proplists:get_value(proto_hrl_uppercase, ProtoOpts, false),

            generate_proto_info_hrl(MetaList, OutProtoHrl, IsUppercase);
        _ ->
            skip
    end,

    ok.

%% ----------------------------------------------------------------------------------------------------
%% @doc generate_proto_info
%% ----------------------------------------------------------------------------------------------------
generate_proto_info(MetaList, OutProtoInfo, CustomInfoFile, AppDir) ->
    ModuleName = erlang:list_to_atom(filename:rootname(filename:basename(OutProtoInfo))),
    Module = generate_module(ModuleName, MetaList, CustomInfoFile, AppDir),
    Formatted = erl_prettypr:format(Module),
    ok = file:write_file(OutProtoInfo, Formatted),
    ok.

generate_module(ModName, MetaList, CustomInfoFile, AppDir) ->
    Mod = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(ModName)]),
    [CustomExportList, CustomClaues] = generate_custom_info(CustomInfoFile, MetaList, AppDir),

    Export = erl_syntax:attribute(erl_syntax:atom(export),
                                   [erl_syntax:list(CustomExportList)]),
    Clauses = CustomClaues,

    erl_syntax:form_list([Mod, Export| Clauses]).

%% generate custom function
generate_custom_info(undefined, _, _AppDir) ->
    [];
generate_custom_info(CustomInfoFile, MetaList, AppDir) ->
    LoadFile = filename:join([AppDir, filename:rootname(CustomInfoFile, ".erl")]),
    LoadInclude = filename:join([AppDir,  "include/"]),

    rebar_api:debug("load module file: ~p~n", [{LoadFile}]),
    {ok, Mod, Bin} = compile:file(LoadFile, [binary, {i, LoadInclude}]),
    code:load_binary(Mod, [], Bin),
    generate_custom_info_1(MetaList, Mod:fun_list(), [], []).

generate_custom_info_1(_MetaList, [], AccExportList, AccFunList) ->
    [AccExportList, AccFunList];
generate_custom_info_1(MetaList, [HFun|TFun], AccExportList, AccFunList) ->
    #{fun_name := FunName} = ClausesMapsList = HFun(MetaList),
    Export = erl_syntax:arity_qualifier(erl_syntax:atom(FunName), erl_syntax:integer(1)),
    Clauses = general_create_function(ClausesMapsList),
    generate_custom_info_1(MetaList, TFun, AccExportList++[Export], AccFunList++Clauses).

%% MFA, A always a list
general_create_function(#{fun_name:=FunName, clauses:=ClausesMapsList}) ->
    Name = erl_syntax:atom(FunName),
    Clauses = [ erl_syntax:clause([general_create_function_variable(Arg) || Arg <- Args],
                                  none,
                                  [general_create_function_variable(Return)])
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
    erl_syntax:list([general_create_function_variable(VarX) || VarX <- Var]);
general_create_function_variable(Var) when is_tuple(Var) ->
    List = tuple_to_list(Var),
    List1 = [general_create_function_variable(X) || X <- List],
    erl_syntax:tuple(List1).


%% ----------------------------------------------------------------------------------------------------
%% @doc generate_proto_info_hrl
%% ----------------------------------------------------------------------------------------------------
generate_proto_info_hrl(MetaList, OutProtoHrl, IsUppercase) ->
    Hrl = generate_hrl(MetaList, IsUppercase),
    Formatted = erl_prettypr:format(Hrl),
    ok = file:write_file(OutProtoHrl, Formatted),
    ok.

generate_hrl(MetaList, IsUppercase) ->
    generate_hrl(MetaList, IsUppercase, []).

generate_hrl([], _IsUppercase, Acc) ->
    erl_syntax:form_list(Acc);
generate_hrl([#{msg_name:=MsgName, msg_code:=MsgCode}|T], IsUppercase, Acc) ->
    FixMsgName = case IsUppercase of
                     true -> erlang:list_to_atom(string:uppercase(erlang:atom_to_list(MsgName)));
                     _ -> MsgName
                 end,

    Def = erl_syntax:attribute(erl_syntax:atom(define), [erl_syntax:atom(FixMsgName), erl_syntax:integer(MsgCode)]),
    generate_hrl(T, IsUppercase, [Def | Acc]).


test() ->
    List = [#{msg_name => list_to_atom("msg" ++ integer_to_list(X)),
              msg_code => 1000+X,
              pb_module => list_to_atom("pb_msg" ++ integer_to_list(X))} || X <- lists:seq(1,8000)],
    {Micros, Res} = timer:tc(fun generate_proto_info/4, [List, "proto.erl", undefined, ""]),
    io:format("test result: ~p~n", [{Micros, Res}]).


