%%%-------------------------------------------------------------------
%%% @doc WOO HTTP Handler - Cowboy REST Handler
%%% Implements RESTful API endpoints for the WOO Dashboard
%%% @end
%%%-------------------------------------------------------------------
-module(woo_http_handler).

-export([init/2]).

%%%===================================================================
%%% Cowboy Handler Callbacks
%%%===================================================================

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Action = maps:get(action, State, undefined),

    %% Add CORS headers
    Req1 = add_cors_headers(Req0),

    %% Handle OPTIONS request (CORS preflight)
    Req2 = case Method of
        <<"OPTIONS">> ->
            cowboy_req:reply(204, Req1);
        _ ->
            handle_request(Method, Action, Req1)
    end,

    {ok, Req2, State}.

%%%===================================================================
%%% Request Handlers
%%%===================================================================

%% @private
handle_request(<<"GET">>, list_documents, Req) ->
    {ok, Documents} = woo_document_manager:get_all_documents(),
    json_response(200, #{documents => Documents}, Req);

handle_request(<<"GET">>, get_document, Req) ->
    Id = cowboy_req:binding(id, Req),
    case woo_document_manager:get_document(Id) of
        {ok, Document} ->
            json_response(200, Document, Req);
        {error, not_found} ->
            json_response(404, #{error => <<"Document not found">>}, Req)
    end;

handle_request(<<"POST">>, update_status, Req) ->
    Id = cowboy_req:binding(id, Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),

    case jsx:decode(Body, [return_maps]) of
        #{<<"status">> := NewStatus} ->
            case woo_document_manager:update_status(Id, NewStatus) of
                {ok, UpdatedDoc} ->
                    json_response(200, UpdatedDoc, Req1);
                {error, not_found} ->
                    json_response(404, #{error => <<"Document not found">>}, Req1)
            end;
        _ ->
            json_response(400, #{error => <<"Missing status field">>}, Req1)
    end;

handle_request(<<"GET">>, get_statistics, Req) ->
    {ok, Stats} = woo_document_manager:get_statistics(),
    json_response(200, Stats, Req);

handle_request(<<"GET">>, get_org_stats, Req) ->
    Org = cowboy_req:binding(org, Req),
    OrgName = case Org of
        <<"utrecht">> -> <<"Gemeente Utrecht">>;
        <<"flevoland">> -> <<"Provincie Flevoland">>;
        _ -> Org
    end,
    {ok, Documents} = woo_document_manager:get_by_organization(OrgName),
    json_response(200, #{
        organization => OrgName,
        count => length(Documents),
        documents => Documents
    }, Req);

handle_request(<<"POST">>, start_simulation, Req) ->
    case woo_simulation_server:start_simulation() of
        ok ->
            json_response(200, #{
                status => <<"started">>,
                message => <<"Simulation started successfully">>
            }, Req);
        {error, already_running} ->
            json_response(400, #{
                error => <<"Simulation already running">>
            }, Req)
    end;

handle_request(<<"POST">>, stop_simulation, Req) ->
    case woo_simulation_server:stop_simulation() of
        ok ->
            json_response(200, #{
                status => <<"stopped">>,
                message => <<"Simulation stopped successfully">>
            }, Req);
        {error, not_running} ->
            json_response(400, #{
                error => <<"Simulation not running">>
            }, Req)
    end;

handle_request(<<"GET">>, health_check, Req) ->
    IsSimulating = woo_simulation_server:is_running(),
    {ok, Stats} = woo_document_manager:get_statistics(),
    json_response(200, #{
        status => <<"healthy">>,
        simulation_running => IsSimulating,
        total_documents => maps:get(total_requests, Stats),
        timestamp => iso8601_timestamp()
    }, Req);

handle_request(_, _, Req) ->
    json_response(404, #{error => <<"Endpoint not found">>}, Req).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
%% @doc Send JSON response with CORS headers
json_response(Status, Data, Req) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-allow-headers">> => <<"Content-Type, Authorization">>
    }, Body, Req).

%% @private
%% @doc Add CORS headers to request
add_cors_headers(Req) ->
    cowboy_req:set_resp_headers(#{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-allow-headers">> => <<"Content-Type, Authorization">>
    }, Req).

%% @private
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                     [Year, Month, Day, Hour, Min, Sec])
    ).
