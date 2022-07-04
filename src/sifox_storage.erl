%%%-------------------------------------------------------------------
%%% @author losto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(sifox_storage).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([
    add/1,
    delete/1,
    get_all/0,
    any_restrict/1
]).
-define(SERVER, ?MODULE).

-record(sifox_storage_state, {connection, cache_table}).

%%% API
-spec(add(Abonent::binary()) -> ok).
add(Abonent) ->
    gen_server:call(?MODULE, {add, Abonent}).

-spec(delete(Abonent::binary()) -> ok).
delete(Abonent) ->
    gen_server:call(?MODULE, {delete, Abonent}).

-spec(get_all() -> {ok, [binary()]}).
get_all() ->
    gen_server:call(?MODULE, get_all).

-spec(any_restrict([binary()]) -> boolean()).
any_restrict(AbonList) ->
    gen_server:call(?MODULE, {any_restrict, AbonList}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ConnOpts = conn_opts(),
    {ok, Conn} = epgsql:connect(ConnOpts),
    {ok, CacheTbl} = init_cache(Conn),
    {ok, #sifox_storage_state{connection = Conn, cache_table = CacheTbl}}.

handle_call({add, Abonent}=Msg, _From, State = #sifox_storage_state{connection = Conn, cache_table = Tbl}) ->
    {ok, _} = epgsql:equery(Conn, "insert into restrict values ($1)", [Abonent]),
    ok = update_cache(Msg, Tbl),
    {reply, ok, State};

handle_call({delete, Abonent}=Msg, _From, State = #sifox_storage_state{connection = Conn, cache_table = Tbl}) ->
    {ok, _} = epgsql:equery(Conn, "delete from restrict where \"abonent\" = $1", [Abonent]),
    ok = update_cache(Msg, Tbl),
    {reply, ok, State};

handle_call(get_all, _From, State = #sifox_storage_state{cache_table = Tbl}) ->
    AbonList = ets:tab2list(Tbl),
    {reply, {ok, AbonList}, State};

handle_call({any_restrict, AbonList}, _From, State = #sifox_storage_state{cache_table = Tbl}) ->
    Result = lists:any(fun(Abonent) -> ets:member(Tbl, Abonent) end, AbonList),
    {reply, Result, State};

handle_call(_Request, _From, State = #sifox_storage_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #sifox_storage_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #sifox_storage_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #sifox_storage_state{}) ->
    ok.

code_change(_OldVsn, State = #sifox_storage_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

conn_opts() ->
    {ok, Host} = application:get_env(sifox_ex, db_host),
    {ok, Port} = application:get_env(sifox_ex, db_port),
    {ok, Database} = application:get_env(sifox_ex, database),
    {ok, User} = application:get_env(sifox_ex, db_user),
    {ok, Password} = application:get_env(sifox_ex, db_password),
    #{
        host => Host,
        port => Port,
        username => User,
        password => Password,
        database => Database
    }.

init_cache(Conn) ->
    Tbl = ets:new(restrict_tbl, []),
    {ok, _, Rows} = epgsql:squery(Conn, "select \"abonent\" from restrict"),
    true = ets:insert(Tbl, Rows),
    {ok, Tbl}.

update_cache({add, Abonent}, Tbl) ->
    true = ets:insert(Tbl, {Abonent}),
    ok;
update_cache({delete, Abonent}, Tbl) ->
    true = ets:delete(Tbl, Abonent),
    ok.