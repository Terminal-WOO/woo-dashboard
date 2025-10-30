%%%-------------------------------------------------------------------
%%% @doc WOO Backend Top-Level Supervisor
%%% Implements OTP supervisor pattern with one_for_one restart strategy
%%% @end
%%%-------------------------------------------------------------------
-module(woo_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the supervisor
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @doc Initialize the supervisor with child specifications
init([]) ->
    io:format("Initializing WOO Supervisor~n"),

    SupFlags = #{
        strategy => one_for_one,      % Restart only failed child
        intensity => 10,               % Max 10 restarts
        period => 60                   % Within 60 seconds
    },

    ChildSpecs = [
        %% Document Manager Gen_Server
        #{
            id => woo_document_manager,
            start => {woo_document_manager, start_link, []},
            restart => permanent,       % Always restart
            shutdown => 5000,
            type => worker,
            modules => [woo_document_manager]
        },

        %% Event Manager Gen_Event
        #{
            id => woo_event_manager,
            start => {woo_event_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [woo_event_manager]
        },

        %% Simulation Server Gen_Server
        #{
            id => woo_simulation_server,
            start => {woo_simulation_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [woo_simulation_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
