%%%-------------------------------------------------------------------
%% @doc sifox_test top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sifox_ex_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([
    init/1,
    start_listener/0
]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{
            id => listener,
            start => {?MODULE, start_listener, []}
        },
        #{
            id => storage,
            start => {sifox_storage, start_link, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

start_listener() ->
    IP = application:get_env(sifox_test, ip, {0,0,0,0}),
    Port = application:get_env(sifox_test, port, 8081),
    Routes = [
        {"/check_restrict", sifox_restrict_handler, #{}}
    ],
    Args = [
        {ip, IP},
        {port, Port}
    ],
    RouteDispatch = cowboy_router:compile([{'_', Routes}]),
    Opts = #{
        env => #{
            dispatch => RouteDispatch
        },
        middlewares => [
            cowboy_router,
            cowboy_handler
        ]
    },
    cowboy:start_clear(http, Args, Opts).

%% internal functions
