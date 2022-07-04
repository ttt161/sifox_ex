-module(sifox_restrict_handler).
-behaviour(cowboy_rest).

-record(state, {}).

-export([
    init/2,
    process/2,
    allowed_methods/2,
    content_types_accepted/2
]).

init(Req, _Opts) ->
    {cowboy_rest, Req, #state{}}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, process}], Req, State}.

process(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    try jsx:decode(Body, [return_maps]) of
        #{<<"from_uri">> := <<"sip:", FromLogin/binary>>, <<"to_uri">> := <<"sip:", ToLogin/binary>>} ->
            case sifox_storage:any_restrict([FromLogin, ToLogin]) of
                false -> {true, Req1, State};
                true ->
                    Req2 = cowboy_req:reply(403, Req1),
                    {stop, Req2, State}
            end
    catch _:_ ->
        Req2 = cowboy_req:reply(403, Req1),
        {stop, Req2, State}
    end.
