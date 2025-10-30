%%%-------------------------------------------------------------------
%%% @doc WOO Event Manager - Gen_Event Implementation
%%% Manages event notifications and subscriptions
%%% @end
%%%-------------------------------------------------------------------
-module(woo_event_manager).
-behaviour(gen_event).

%% API
-export([start_link/0, stop/0]).
-export([
    add_handler/1,
    delete_handler/1,
    notify_status_change/5,
    get_recent_events/0
]).

%% Gen_Event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_EVENTS, 50).

-record(state, {
    events = [] :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?SERVER}).

stop() ->
    gen_event:stop(?SERVER).

%% @doc Add an event handler
add_handler(Handler) ->
    gen_event:add_handler(?SERVER, Handler, []).

%% @doc Delete an event handler
delete_handler(Handler) ->
    gen_event:delete_handler(?SERVER, Handler, []).

%% @doc Notify all handlers about a status change
notify_status_change(DocId, Organization, OrgType, OldStatus, NewStatus) ->
    Event = #{
        id => generate_event_id(),
        timestamp => iso8601_timestamp(),
        type => <<"status_change">>,
        documentId => DocId,
        organization => Organization,
        organizationType => OrgType,
        oldStatus => OldStatus,
        newStatus => NewStatus,
        message => format_status_message(DocId, OldStatus, NewStatus)
    },
    gen_event:notify(?SERVER, {status_change, Event}).

%% @doc Get recent events
get_recent_events() ->
    gen_event:call(?SERVER, woo_event_handler, get_events).

%%%===================================================================
%%% Gen_Event callbacks
%%%===================================================================

init([]) ->
    io:format("Starting WOO Event Handler~n"),
    {ok, #state{events = []}}.

handle_event({status_change, Event}, State) ->
    %% Store event in history (limited to MAX_EVENTS)
    Events = [Event | State#state.events],
    NewEvents = lists:sublist(Events, ?MAX_EVENTS),

    %% Log the event
    io:format("Event: ~s changed from ~s to ~s~n", [
        maps:get(documentId, Event),
        maps:get(oldStatus, Event),
        maps:get(newStatus, Event)
    ]),

    {ok, State#state{events = NewEvents}};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(get_events, State) ->
    {ok, State#state.events, State};

handle_call(_Request, State) ->
    {ok, {error, unknown_request}, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_event_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    list_to_binary(io_lib:format("evt-~p-~p", [Timestamp, Random])).

iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                     [Year, Month, Day, Hour, Min, Sec])
    ).

format_status_message(DocId, OldStatus, NewStatus) ->
    list_to_binary(io_lib:format("Document ~s: ~s → ~s",
                                 [DocId, OldStatus, NewStatus])).
