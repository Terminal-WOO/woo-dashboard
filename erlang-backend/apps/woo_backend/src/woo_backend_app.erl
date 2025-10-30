%%%-------------------------------------------------------------------
%%% @doc WOO Backend Application Module
%%% @end
%%%-------------------------------------------------------------------
-module(woo_backend_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc Start the WOO Backend application
start(_StartType, _StartArgs) ->
    io:format("~n=== Starting WOO Backend Application ===~n"),

    %% Start the supervisor tree
    case woo_sup:start_link() of
        {ok, Pid} ->
            %% Start Cowboy HTTP server
            start_http_server(),
            io:format("WOO Backend started successfully~n"),
            io:format("HTTP API listening on port 8080~n"),
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Stop the WOO Backend application
stop(_State) ->
    io:format("~n=== Stopping WOO Backend Application ===~n"),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Start Cowboy HTTP server with routing
start_http_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Document endpoints
            {"/api/documents", woo_http_handler, #{action => list_documents}},
            {"/api/documents/:id", woo_http_handler, #{action => get_document}},
            {"/api/documents/:id/status", woo_http_handler, #{action => update_status}},

            %% Statistics endpoints
            {"/api/statistics", woo_http_handler, #{action => get_statistics}},
            {"/api/statistics/:org", woo_http_handler, #{action => get_org_stats}},

            %% Simulation endpoints
            {"/api/simulation/start", woo_http_handler, #{action => start_simulation}},
            {"/api/simulation/stop", woo_http_handler, #{action => stop_simulation}},

            %% Health check
            {"/api/health", woo_http_handler, #{action => health_check}},

            %% CORS preflight
            {"/api/[...]", woo_http_handler, #{action => cors_preflight}}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(
        woo_http_listener,
        [{port, 8080}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [cowboy_router, cowboy_handler]
        }
    ),
    ok.
