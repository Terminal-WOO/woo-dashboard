%%%-------------------------------------------------------------------
%%% @doc WOO Document Manager - Gen_Server Implementation
%%% Manages all WOO document state and operations
%%% Uses ETS for fast in-memory storage
%%% @end
%%%-------------------------------------------------------------------
-module(woo_document_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([
    get_all_documents/0,
    get_document/1,
    get_by_organization/1,
    get_by_status/1,
    update_status/2,
    get_statistics/0,
    initialize_data/0
]).

%% Gen_Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, woo_documents).

-record(state, {
    table :: ets:tid()
}).

-record(document, {
    id :: binary(),
    title :: binary(),
    status :: binary(),
    submitted_date :: binary(),
    organization :: binary(),
    organization_type :: binary(),
    category :: binary(),
    subject :: binary(),
    requester :: binary(),
    handler :: binary(),
    decided_date :: binary() | undefined,
    last_modified :: binary()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Get all documents
get_all_documents() ->
    gen_server:call(?SERVER, get_all_documents).

%% @doc Get a specific document by ID
get_document(Id) ->
    gen_server:call(?SERVER, {get_document, Id}).

%% @doc Get documents by organization
get_by_organization(Org) ->
    gen_server:call(?SERVER, {get_by_organization, Org}).

%% @doc Get documents by status
get_by_status(Status) ->
    gen_server:call(?SERVER, {get_by_status, Status}).

%% @doc Update document status
update_status(Id, NewStatus) ->
    gen_server:call(?SERVER, {update_status, Id, NewStatus}).

%% @doc Get statistics
get_statistics() ->
    gen_server:call(?SERVER, get_statistics).

%% @doc Initialize sample data
initialize_data() ->
    gen_server:cast(?SERVER, initialize_data).

%%%===================================================================
%%% Gen_Server callbacks
%%%===================================================================

init([]) ->
    io:format("Starting WOO Document Manager~n"),

    %% Create ETS table for documents
    Table = ets:new(?TABLE, [
        set,
        named_table,
        {keypos, #document.id},
        {read_concurrency, true}
    ]),

    %% Initialize with sample data
    initialize_sample_data(Table),

    {ok, #state{table = Table}}.

handle_call(get_all_documents, _From, State) ->
    Documents = ets:tab2list(?TABLE),
    Reply = [document_to_map(Doc) || Doc <- Documents],
    {reply, {ok, Reply}, State};

handle_call({get_document, Id}, _From, State) ->
    Reply = case ets:lookup(?TABLE, Id) of
        [Doc] -> {ok, document_to_map(Doc)};
        [] -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({get_by_organization, Org}, _From, State) ->
    Pattern = #document{organization = Org, _ = '_'},
    Documents = ets:match_object(?TABLE, Pattern),
    Reply = [document_to_map(Doc) || Doc <- Documents],
    {reply, {ok, Reply}, State};

handle_call({get_by_status, Status}, _From, State) ->
    Pattern = #document{status = Status, _ = '_'},
    Documents = ets:match_object(?TABLE, Pattern),
    Reply = [document_to_map(Doc) || Doc <- Documents],
    {reply, {ok, Reply}, State};

handle_call({update_status, Id, NewStatus}, _From, State) ->
    Reply = case ets:lookup(?TABLE, Id) of
        [Doc] ->
            OldStatus = Doc#document.status,
            UpdatedDoc = Doc#document{
                status = NewStatus,
                last_modified = iso8601_timestamp()
            },
            ets:insert(?TABLE, UpdatedDoc),

            %% Notify event manager
            woo_event_manager:notify_status_change(
                Id,
                Doc#document.organization,
                Doc#document.organization_type,
                OldStatus,
                NewStatus
            ),

            {ok, document_to_map(UpdatedDoc)};
        [] ->
            {error, not_found}
    end,
    {reply, Reply, State};

handle_call(get_statistics, _From, State) ->
    AllDocs = ets:tab2list(?TABLE),

    Total = length(AllDocs),
    Received = count_status(AllDocs, <<"Ontvangen">>),
    InProgress = count_status(AllDocs, <<"In behandeling">>) +
                 count_status(AllDocs, <<"1e Concept">>) +
                 count_status(AllDocs, <<"2e Concept">>) +
                 count_status(AllDocs, <<"Definitief">>),
    Completed = count_status(AllDocs, <<"Gepubliceerd">>),

    Stats = #{
        total_requests => Total,
        received => Received,
        in_progress => InProgress,
        completed => Completed,
        average_handling_days => 0
    },

    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(initialize_data, State) ->
    initialize_sample_data(State#state.table),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Stopping WOO Document Manager~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
initialize_sample_data(Table) ->
    Documents = [
        %% Gemeente Utrecht
        #document{
            id = <<"WOO-UTR-2024-001">>,
            title = <<"Informatie over nieuwbouwproject Merwedekanaal">>,
            status = <<"Ontvangen">>,
            submitted_date = <<"2024-01-15">>,
            organization = <<"Gemeente Utrecht">>,
            organization_type = <<"gemeente">>,
            category = <<"Ruimtelijke ordening">>,
            subject = <<"Verzoek om alle documenten betreffende het nieuwbouwproject">>,
            requester = <<"Jan de Vries">>,
            handler = <<"Afdeling Ruimtelijke Ordening">>,
            decided_date = undefined,
            last_modified = iso8601_timestamp()
        },
        #document{
            id = <<"WOO-UTR-2024-002">>,
            title = <<"Verkeersplan binnenstad Utrecht">>,
            status = <<"In behandeling">>,
            submitted_date = <<"2024-01-20">>,
            organization = <<"Gemeente Utrecht">>,
            organization_type = <<"gemeente">>,
            category = <<"Verkeer en vervoer">>,
            subject = <<"Verzoek om inzage in voorgestelde verkeersmaatregelen">>,
            requester = <<"Marie Jansen">>,
            handler = <<"Afdeling Mobiliteit">>,
            decided_date = undefined,
            last_modified = iso8601_timestamp()
        },
        #document{
            id = <<"WOO-UTR-2024-003">>,
            title = <<"Subsidieverlening culturele instellingen">>,
            status = <<"1e Concept">>,
            submitted_date = <<"2024-02-01">>,
            organization = <<"Gemeente Utrecht">>,
            organization_type = <<"gemeente">>,
            category = <<"Cultuur en onderwijs">>,
            subject = <<"Overzicht subsidies aan culturele instellingen 2023-2024">>,
            requester = <<"Peter Bakker">>,
            handler = <<"Afdeling Cultuur">>,
            decided_date = undefined,
            last_modified = iso8601_timestamp()
        },
        #document{
            id = <<"WOO-UTR-2024-004">>,
            title = <<"Contracten afvalverwerking 2024">>,
            status = <<"2e Concept">>,
            submitted_date = <<"2024-02-10">>,
            organization = <<"Gemeente Utrecht">>,
            organization_type = <<"gemeente">>,
            category = <<"Milieu en duurzaamheid">>,
            subject = <<"Alle contracten met afvalverwerkingsbedrijven">>,
            requester = <<"Anna Smit">>,
            handler = <<"Afdeling Milieu">>,
            decided_date = undefined,
            last_modified = iso8601_timestamp()
        },

        %% Provincie Flevoland
        #document{
            id = <<"WOO-FLE-2024-001">>,
            title = <<"Stikstofrapportage landbouw Flevoland">>,
            status = <<"Definitief">>,
            submitted_date = <<"2024-01-25">>,
            organization = <<"Provincie Flevoland">>,
            organization_type = <<"provincie">>,
            category = <<"Milieu en duurzaamheid">>,
            subject = <<"Stikstofmetingen en impact landbouw">>,
            requester = <<"Boeren Collectief Flevoland">>,
            handler = <<"Afdeling Milieu en Water">>,
            decided_date = undefined,
            last_modified = iso8601_timestamp()
        },
        #document{
            id = <<"WOO-FLE-2024-002">>,
            title = <<"Windmolenpark Noordoostpolder vergunningen">>,
            status = <<"Gepubliceerd">>,
            submitted_date = <<"2024-02-05">>,
            organization = <<"Provincie Flevoland">>,
            organization_type = <<"provincie">>,
            category = <<"Energie en klimaat">>,
            subject = <<"Alle vergunningen en bezwaarschriften windmolenpark">>,
            requester = <<"Stichting Leefbaar NOP">>,
            handler = <<"Afdeling Vergunningen">>,
            decided_date = <<"2024-03-01">>,
            last_modified = iso8601_timestamp()
        },
        #document{
            id = <<"WOO-FLE-2024-003">>,
            title = <<"N23 reconstructie projectplan">>,
            status = <<"Ontvangen">>,
            submitted_date = <<"2024-02-15">>,
            organization = <<"Provincie Flevoland">>,
            organization_type = <<"provincie">>,
            category = <<"Verkeer en vervoer">>,
            subject = <<"Projectplan en kostenraming N23 reconstructie">>,
            requester = <<"Belangenvereniging Ondernemers Flevoland">>,
            handler = <<"Afdeling Infrastructuur">>,
            decided_date = undefined,
            last_modified = iso8601_timestamp()
        },
        #document{
            id = <<"WOO-FLE-2024-004">>,
            title = <<"Subsidieregeling duurzame landbouw 2024">>,
            status = <<"In behandeling">>,
            submitted_date = <<"2024-02-20">>,
            organization = <<"Provincie Flevoland">>,
            organization_type = <<"provincie">>,
            category = <<"Landbouw en natuur">>,
            subject = <<"Criteria en toegekende subsidies duurzame landbouw">>,
            requester = <<"Agrarische Natuurvereniging Flevoland">>,
            handler = <<"Afdeling Economie en Landbouw">>,
            decided_date = undefined,
            last_modified = iso8601_timestamp()
        }
    ],

    [ets:insert(Table, Doc) || Doc <- Documents],
    io:format("Initialized ~p sample documents~n", [length(Documents)]),
    ok.

%% @private
document_to_map(#document{} = Doc) ->
    #{
        id => Doc#document.id,
        title => Doc#document.title,
        status => Doc#document.status,
        submittedDate => Doc#document.submitted_date,
        organization => Doc#document.organization,
        organizationType => Doc#document.organization_type,
        category => Doc#document.category,
        subject => Doc#document.subject,
        requester => Doc#document.requester,
        handler => Doc#document.handler,
        decidedDate => Doc#document.decided_date,
        lastModified => Doc#document.last_modified
    }.

%% @private
count_status(Documents, Status) ->
    length([D || D <- Documents, D#document.status =:= Status]).

%% @private
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                     [Year, Month, Day, Hour, Min, Sec])
    ).
