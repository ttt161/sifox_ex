%%%-------------------------------------------------------------------
%% @doc sifox_test public API
%% @end
%%%-------------------------------------------------------------------

-module(sifox_ex_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sifox_ex_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
