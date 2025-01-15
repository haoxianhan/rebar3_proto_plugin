-module(generate_meta_file).

-export([
         generate/2,
         spawn_load_proto_file/5
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
    {Meta1, FilterProtos} = filter_protos_modify(Meta, FoundProtos),
    rebar_api:debug("filter proto : ~p~n", [{FilterProtos}]),

    Meta2 = load_proto_file(Meta1, FilterProtos, GpbOpts, AppDir),

    rebar_api:debug("proto write meta : ~p~n", [{Meta2}]),
    write_meta_file(MetaFile, Meta2),
    {ok, Meta2}.

filter_protos_modify(Meta, FoundProtos) ->
    #{proto := ProtoMap} = Meta,
    {NewProtoMap, FilterProtos} = filter_protos_modify(ProtoMap, FoundProtos, []),
    {Meta#{proto := NewProtoMap}, FilterProtos}.

filter_protos_modify(ProtoMap, [], Acc) ->
    {ProtoMap, Acc};
filter_protos_modify(ProtoMap, [H|T], Acc) ->
    case filter_protos_modify_check(ProtoMap, H) of
        {true, CurrentMd5sum} ->
            HBaseName = filename:basename(H),
            filter_protos_modify(ProtoMap#{HBaseName => CurrentMd5sum}, T, [H|Acc]);
        _ ->
            filter_protos_modify(ProtoMap, T, Acc)
    end.

filter_protos_modify_check(ProtoMap, H) ->
    CurrentMd5sum = case file:read_file(H) of
                        {ok, Binary} ->
                            erlang:md5(Binary);
                        _ ->
                            <<>>
                    end,

    HBaseName = filename:basename(H),
    case maps:find(HBaseName, ProtoMap) of
        error ->
            {true, CurrentMd5sum};
        {ok, OriginMd5Str} ->
            rebar_api:debug("originmd5sum : ~s, current_md5sum: ~s", [OriginMd5Str, CurrentMd5sum]),
            {not (OriginMd5Str == CurrentMd5sum), CurrentMd5sum}
    end.

%% load data to form meta
load_proto_file(Meta, ProtoList, GpbOpts, AppDir) ->
    RefList = [begin
                   Ref = make_ref(),
                   spawn(?MODULE, spawn_load_proto_file, [self(), Ref, HProto, GpbOpts, AppDir]),
                   Ref
               end || HProto <- ProtoList],
    rebar_api:debug("prepare receive ~p", [RefList]),
    receive_load_proto_file(RefList, Meta).

receive_load_proto_file([], Meta) ->
    Meta;
receive_load_proto_file([Ref|T], Meta) ->
    receive
        {RRef, {ok, Mod, MsgNameList}} when Ref == RRef ->
            NewMeta = load_msg(Mod, Meta, MsgNameList),
            receive_load_proto_file(T, NewMeta);
        {RRef, {error, Reason}} when Ref == RRef ->
            rebar_api:error("Proto compilation failed: ~p", [Reason]),
            receive_load_proto_file(T, Meta)
    after 30*1000 ->
        rebar_api:error("Proto compilation timeout for ref: ~p", [Ref]),
        receive_load_proto_file(T, Meta)
    end.

spawn_load_proto_file(From, Ref, HProto, GpbOpts, AppDir) ->
    ProtoBaseName = filename:basename(HProto, ".proto"),

    LoadFile = filename:join([AppDir, filename:rootname(get_target_erl(HProto, GpbOpts), ".erl")]),
    LoadInclude = filename:join([AppDir, proplists:get_value(o_hrl, GpbOpts)]),

    rebar_api:debug("load module file: ~p~n", [{LoadFile, LoadInclude}]),
    try
        case compile:file(LoadFile, [binary, {i, LoadInclude}]) of
            {ok, Mod, Bin} ->
                case code:load_binary(Mod, [], Bin) of
                    {module, _} ->
                        MsgNameList = Mod:get_msg_containment(ProtoBaseName),
                        From ! {Ref, {ok, Mod, MsgNameList}};
                    Error ->
                        From ! {Ref, {error, {load_failed, Error}}}
                end;
            Error ->
                From ! {Ref, {error, {compile_failed, Error}}}
        end
    catch
        Error1:Reason:Stack ->
            From ! {Ref, {error, {Error1, Reason, Stack}}}
    end.

load_msg(_Mod, Meta, []) ->
    Meta;
load_msg(Mod, Meta, [HMsg|T]) ->
    #{code_count := CodeCount, message := MessageMap} = Meta,
    NewMeta = case maps:get(HMsg, MessageMap, false) of
                  false ->
                      AccCodeCount = CodeCount + 1,
                      Meta#{ code_count := AccCodeCount,
                             message => MessageMap#{ HMsg => #{msg_code => AccCodeCount,
                                                               pb_module => Mod}}};
                  #{pb_module := OldMod} = OldHMsgInfo when OldMod =/= Mod ->
                      Meta#{message => MessageMap#{HMsg => OldHMsgInfo#{pb_module => Mod}}};
                  _ ->
                      Meta
              end,
    load_msg(Mod, NewMeta, T).

%% read meta file
read_meta_file(MetaFile) ->
    rebar_api:debug("read meta file: ~p~n", [MetaFile]),
    case file:consult(MetaFile) of
        {ok, [Meta]} ->
            Meta;
        _ ->
            #{code_count => 0,
              message => #{},
              proto => #{}
             }
    end.

%% write meta file
write_meta_file(MetaFile, Meta) ->
   ok = file:write_file(MetaFile, io_lib:format("~w.~n", [Meta])).


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
get_target_erl(Proto, GpbOpts) ->
    InputsOutputs = gpb_compile:list_io(Proto, GpbOpts),
    {erl_output, Erl} = lists:keyfind(erl_output, 1, InputsOutputs),
    Erl.

get_target_hrl(Proto, GpbOpts) ->
    InputsOutputs = gpb_compile:list_io(Proto, GpbOpts),
    {hrl_output, Hrl} = lists:keyfind(hrl_output, 1, InputsOutputs),
    Hrl.
