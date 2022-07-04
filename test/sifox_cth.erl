-module(sifox_cth).

-include_lib("common_test/include/ct.hrl").
%% API
-export([init/2, terminate/1, pre_init_per_suite/3]).

init(_Id, State) ->
    Start = os:timestamp(),
    mock_services(),
    start_sifox_ex(),

    io:format(user, "Sifox_ex started in ~p ms\n",
        [timer:now_diff(os:timestamp(), Start) / 1000]),
    State.

pre_init_per_suite(_SuiteName, Config, State) ->
    {Config ++ State, State}.

terminate(_State) -> ok.

mock_services() ->
    meck:expect(sifox_storage, start_link, fun() -> ignore end).

start_sifox_ex() ->
    application:ensure_all_started(sifox_ex).