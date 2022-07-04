-module(sifox_restrict_SUITE).

%% API
-export([
    init_per_suite/1,
    end_per_suite/1,
    all/0,
    test/1
]).

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() -> [
    test
].

test(_Config) ->
    meck:expect(sifox_storage, any_restrict, fun([<<"1111">>, <<"2222">>]) -> false end),
    Body = jsx:encode(#{from_uri => <<"sip:1111">>, to_uri => <<"sip:2222">>}),
    {ok, {{_, 204, _}, _, _}} = httpc:request(post, {"http://localhost:8081/check_restrict", [], "application/json", Body}, [], []),
    ok = meck:delete(sifox_storage, any_restrict, 1),
    meck:expect(sifox_storage, any_restrict, fun([<<"1111">>, <<"2222">>]) -> true end),
    {ok, {{_, 403, _}, _, _}} = httpc:request(post, {"http://localhost:8081/check_restrict", [], "application/json", Body}, [], []),
    ok = meck:delete(sifox_storage, any_restrict, 1).
