-module(rebar3_proto_plugin_prv_generate).

-export([
        generate/2
        ]).

generate(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    DepsDir = rebar_dir:deps_dir(State),
    Opts = rebar_app_info:opts(AppInfo),

    {ok, ProtoOpts} = dict:find(proto_opts, Opts),
    MetaFileName = proplists:get_value(meta_file, ProtoOpts, "proto.meta"),
    MetaFile = filename:join([AppDir, MetaFileName]),

    Meta = read_meta_file(MetaFile),
    rebar_api:debug("proto load meta : ~p~n", [{Meta}]),

    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    FoundProtos = find_proto_files(AppDir, DepsDir, GpbOpts),
    NewMeta = load_proto_file(Meta, FoundProtos, GpbOpts),

    rebar_api:debug("proto write meta : ~p~n", [{NewMeta}]),
    write_meta_file(MetaFile, NewMeta),
    ok.

load_proto_file(Meta, [], _GpbOpts) ->
    Meta;
load_proto_file(Meta, [HProto|T], GpbOpts) ->
    ProtoBaseName = filename:basename(HProto, ".proto"),
    Mod = list_to_atom(filename:basename(get_target(HProto, GpbOpts), ".erl")),
    MsgNames = Mod:get_msg_containment(ProtoBaseName),
    NewMeta = load_msg(Mod, Meta, MsgNames),
    load_proto_file(NewMeta, T, GpbOpts).

load_msg(_Mod, Meta, []) ->
    Meta;
load_msg(Mod, Meta, [HMsg|T]) ->
    #{code_count := CodeCount} = Meta,
    NewMeta = case maps:is_key(HMsg, Meta) of
                  false ->
                      AccCodeCount = CodeCount + 1,
                      Meta1 = add_msg_to_meta(Meta, HMsg, AccCodeCount, Mod),
                      Meta2 = maps:update(code_count, AccCodeCount, Meta1),
                      Meta2;
                  _ ->
                      Meta
              end,
    load_msg(Mod, NewMeta, T).


%%
add_msg_to_meta(Meta, MsgName, MsgCode, PbModule) ->
    Value = #{msg_code => MsgCode,
              pb_module => PbModule},
    maps:put(MsgName, Value, Meta).

%% read meta file
read_meta_file(MetaFile) ->
    rebar_api:debug("read meta file: ~p~n", [MetaFile]),
    case file:consult(MetaFile) of
        {ok, [[CodeCount|RawMeta]]} ->
            Map = #{code_count => CodeCount},
            lists:foldl(fun({MsgName, MsgCode, PbModule}, AccMap) ->
                                add_msg_to_meta(AccMap, MsgName, MsgCode, PbModule)
                        end, Map, RawMeta);
        _ ->
            #{code_count => 0}
    end.

%% write meta file
write_meta_file(MetaFile, Meta) ->
    {CodeCount, Meta0} = maps:take(code_count, Meta),
    List0 = lists:keysort(1, [{MsgName, MsgCode, PbModule}
                              || {MsgName, #{msg_code:=MsgCode, pb_module:=PbModule}} <- maps:to_list(Meta0)]),
    ok = file:write_file(MetaFile, io_lib:format("~p.~n", [[CodeCount|List0]])).



%% copy from rebar3_gbp_plugin
discover(AppDir, SourceDir, Recursive) ->
    %% Convert simple extension to proper regex
    SourceExtRe = "^[^._].*\\" ++ ".proto" ++ [$$],

    %% Find all possible source files
    rebar_utils:find_files(filename:join([AppDir, SourceDir]),
                           SourceExtRe, Recursive).

%% copy from rebar3_gbp_plugin
find_proto_files(AppDir, DepsDir, GpbOpts) ->
    %% check if non-recursive
    Recursive = proplists:get_value(recursive, GpbOpts, true),
    SourceDirs = proplists:get_all_values(i, GpbOpts),
    FoundProtos = lists:foldl(fun({deps, SourceDir}, Acc) ->
                                      Acc ++ discover(DepsDir, SourceDir, Recursive);
                                 (SourceDir, Acc) ->
                                      Acc ++ discover(AppDir, SourceDir, Recursive)
                              end, [], SourceDirs),
    rebar_api:debug("proto files found~s: ~p",
                    [case Recursive of true -> " recursively"; false -> "" end, FoundProtos]),
    FoundProtos.

%% copy from rebar3_gbp_plugin
get_target(Proto, GpbOpts) ->
    InputsOutputs = gpb_compile:list_io(Proto, GpbOpts),
    {erl_output, Erl} = lists:keyfind(erl_output, 1, InputsOutputs),
    Erl.
