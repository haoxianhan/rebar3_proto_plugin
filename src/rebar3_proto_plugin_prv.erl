-module(rebar3_proto_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, generate).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {namespace, proto},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 rebar3_proto_plugin"},
            {opts, []},
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
           end,
    lists:foreach(fun(App) ->
                    generate(App, State)
                  end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


generate(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    DepsDir = rebar_dir:deps_dir(State),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts0} = dict:find(gpb_opts, Opts),

    FoundProtos = find_proto_files(AppDir, DepsDir, GpbOpts0),

    rebar_api:debug("haoxian found protos ~p~n", [FoundProtos]),

    [begin
         Mod = "pb_" ++ filename:basename(P, ".proto"),
         rebar_api:debug("haoxian msg name: ~p~n", [list_to_atom(Mod:get_msg_names())])
     end || P <- FoundProtos],
    ok.


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

