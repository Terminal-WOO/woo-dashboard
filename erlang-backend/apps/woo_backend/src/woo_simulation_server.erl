%%%-------------------------------------------------------------------
%%% @doc WOO Simulation Server - Gen_Server Implementation
%%% Simulates automatic status progression of documents
%%% @end
%%%-------------------------------------------------------------------
-module(woo_simulation_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([start_simulation/0, stop_simulation/0, is_running/0]).

%% Gen_Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TICK_INTERVAL, 2000).  % 2 seconds

-record(state, {
    running = false :: boolean(),
    timer_ref = undefined :: reference() | undefined,
    workflow = [
        <<"Ontvangen">>,
        <<"In behandeling">>,
        <<"1e Concept">>,
        <<"2e Concept">>,
        <<"Definitief">>,
        <<"Gepubliceerd">>
    ] :: list(binary())
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Start the simulation
start_simulation() ->
    gen_server:call(?SERVER, start_simulation).

%% @doc Stop the simulation
stop_simulation() ->
    gen_server:call(?SERVER, stop_simulation).

%% @doc Check if simulation is running
is_running() ->
    gen_server:call(?SERVER, is_running).

%%%===================================================================
%%% Gen_Server callbacks
%%%===================================================================

init([]) ->
    io:format("Starting WOO Simulation Server~n"),
    {ok, #state{}}.

handle_call(start_simulation, _From, State) ->
    case State#state.running of
        true ->
            {reply, {error, already_running}, State};
        false ->
            io:format("Starting simulation (tick interval: ~pms)~n", [?TICK_INTERVAL]),
            TimerRef = erlang:send_after(?TICK_INTERVAL, self(), tick),
            {reply, ok, State#state{running = true, timer_ref = TimerRef}}
    end;

handle_call(stop_simulation, _From, State) ->
    case State#state.running of
        false ->
            {reply, {error, not_running}, State};
        true ->
            io:format("Stopping simulation~n"),
            case State#state.timer_ref of
                undefined -> ok;
                Ref -> erlang:cancel_timer(Ref)
            end,
            {reply, ok, State#state{running = false, timer_ref = undefined}}
    end;

handle_call(is_running, _From, State) ->
    {reply, State#state.running, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    %% Process one document status update
    process_tick(State#state.workflow),

    %% Schedule next tick if still running
    NewTimerRef = case State#state.running of
        true -> erlang:send_after(?TICK_INTERVAL, self(), tick);
        false -> undefined
    end,

    {noreply, State#state{timer_ref = NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    io:format("Stopping WOO Simulation Server~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Process a simulation tick - update one random document
process_tick(Workflow) ->
    {ok, AllDocs} = woo_document_manager:get_all_documents(),

    %% Filter documents that are not yet "Gepubliceerd"
    UpdatableDocs = [Doc || Doc <- AllDocs,
                            maps:get(status, Doc) =/= <<"Gepubliceerd">>],

    case UpdatableDocs of
        [] ->
            %% All documents are published, reset one randomly
            case AllDocs of
                [] -> ok;
                _ ->
                    RandomDoc = lists:nth(rand:uniform(length(AllDocs)), AllDocs),
                    DocId = maps:get(id, RandomDoc),
                    woo_document_manager:update_status(DocId, <<"Ontvangen">>),
                    io:format("Reset ~s to Ontvangen (cycle restart)~n", [DocId])
            end;
        _ ->
            %% Pick a random document and advance its status
            RandomDoc = lists:nth(rand:uniform(length(UpdatableDocs)), UpdatableDocs),
            DocId = maps:get(id, RandomDoc),
            CurrentStatus = maps:get(status, RandomDoc),

            %% Find next status in workflow
            case find_next_status(CurrentStatus, Workflow) of
                {ok, NextStatus} ->
                    woo_document_manager:update_status(DocId, NextStatus),
                    io:format("Updated ~s: ~s -> ~s~n",
                             [DocId, CurrentStatus, NextStatus]);
                not_found ->
                    io:format("Warning: Unknown status ~s for ~s~n",
                             [CurrentStatus, DocId])
            end
    end.

%% @private
%% @doc Find the next status in the workflow
find_next_status(CurrentStatus, Workflow) ->
    case lists:member(CurrentStatus, Workflow) of
        false -> not_found;
        true ->
            Index = index_of(CurrentStatus, Workflow),
            case Index < length(Workflow) of
                true -> {ok, lists:nth(Index + 1, Workflow)};
                false -> {ok, lists:last(Workflow)}
            end
    end.

%% @private
%% @doc Find index of element in list (1-based)
index_of(Element, List) ->
    index_of(Element, List, 1).

index_of(_, [], _) -> 0;
index_of(Element, [Element | _], Index) -> Index;
index_of(Element, [_ | Rest], Index) -> index_of(Element, Rest, Index + 1).
