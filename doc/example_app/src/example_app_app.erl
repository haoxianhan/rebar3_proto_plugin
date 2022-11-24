%%%-------------------------------------------------------------------
%% @doc example_app public API
%% @end
%%%-------------------------------------------------------------------

-module(example_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    example_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
