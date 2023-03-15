-module(generate_meta_file).

-export([
         generate/2
        ]).

%% create proto.meta file
generate(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    DepsDir = rebar_dir:deps_dir(State),
    Opts = rebar_app_info:opts(AppInfo),

    {ok, ProtoOpts} = dict:find(proto_opts, Opts),
    MetaFileName = proplists:get_value(o_meta_file, ProtoOpts, "proto.meta"),
    MetaFile = filename:join([AppDir, MetaFileName]),

    Meta = read_meta_file(MetaFile),
    rebar_api:debug("proto load meta : ~p~n", [{Meta}]),

    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    FoundProtos = find_proto_files(AppDir, DepsDir, GpbOpts),
    NewMeta = load_proto_file(Meta, FoundProtos, GpbOpts, AppDir),

    rebar_api:debug("proto write meta : ~p~n", [{NewMeta}]),
    write_meta_file(MetaFile, NewMeta),
    {ok, NewMeta}.

%% load data to form meta
load_proto_file(Meta, [], _GpbOpts, _AppDir) ->
    Meta;
load_proto_file(Meta, [HProto|T], GpbOpts, AppDir) ->
    ProtoBaseName = filename:basename(HProto, ".proto"),

    LoadFile = filename:join([AppDir, filename:rootname(get_target(HProto, GpbOpts), ".erl")]),
    LoadInclude = filename:join([AppDir,  "include/"]),
    rebar_api:debug("load module file: ~p~n", [{LoadFile}]),
    {ok, Mod, Bin} = compile:file(LoadFile, [binary, {i, LoadInclude}]),
    code:load_binary(Mod, [], Bin),

    % Mod = list_to_atom(filename:basename(get_target(HProto, GpbOpts), ".erl")),
    MsgNameList = Mod:get_msg_containment(ProtoBaseName),
    NewMeta = load_msg(Mod, Meta, MsgNameList),
    load_proto_file(NewMeta, T, GpbOpts, AppDir).

load_msg(_Mod, Meta, []) ->
    Meta;
load_msg(Mod, Meta, [HMsg|T]) ->
    #{code_count := CodeCount} = Meta,
    NewMeta = case maps:is_key(HMsg, Meta) of
                  false ->
                      AccCodeCount = CodeCount + 1,
                      Meta#{ code_count := AccCodeCount,
                             HMsg => #{msg_code => AccCodeCount,
                                       pb_module => Mod}};
                  _ ->
                      Meta
              end,
    load_msg(Mod, NewMeta, T).

%% read meta file
read_meta_file(MetaFile) ->
    rebar_api:debug("read meta file: ~p~n", [MetaFile]),
    case file:consult(MetaFile) of
        {ok, [[CodeCount|RawMeta]]} ->
            Map = #{code_count => CodeCount},
            lists:foldl(fun({MsgName, MsgCode, PbModule}, AccMap) ->
                                AccMap#{MsgName => #{msg_code => MsgCode,
                                                     pb_module => PbModule}}
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
