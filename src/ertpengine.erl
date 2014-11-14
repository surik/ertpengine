-module(ertpengine).

%% API
-export([new_connection/1, new_connection/2,
         stop_connection/1,
         do_command/2,
         ping/1,
         offer/4, offer/5,
         answer/5, answer/6,
         delete/3, delete/4,
         list/1, list/2,
         query/2, query/3
        ]).

-type ertpengine_command_key() :: nonempty_string() | binary().
-type ertpengine_command_value() :: nonempty_string() | integer() | 
                                    {list, list(ertpengine_command_value())} | 
                                    [{dict, dict:dict()}].
-type ertpengine_command() :: [{ertpengine_command_key(), ertpengine_command_value()}].

%%%===================================================================
%%% API
%%%===================================================================
-spec new_connection(Name :: atom()) -> atom().
new_connection(Name) ->
    new_connection(Name, []).

-spec new_connection(Name :: atom(), Opts :: [proplists:properties()]) -> 
    atom().
new_connection(Name, Opts) ->
    case ertpengine_sup:start_server(Name, Opts) of
        {error, _} =  Error -> Error;
        _ -> ok
    end.

-spec stop_connection(Name :: atom()) -> ok | {error, Error :: term()}.
stop_connection(Name) ->
    ertpengine_sup:stop_server(Name).

-spec do_command(Name :: atom(), Command :: ertpengine_command()) -> dict:dict().
do_command(Name, Args) ->
    ertpengine_server:do_command(Name, Args).

-spec ping(Name :: atom()) -> pong | {error, Error :: term()}.
ping(Name) ->
    Args = [{"command", "ping"}],
    case dict:fetch(<<"result">>, do_command(Name, Args)) of
        <<"pong">> -> pong;
        E -> {error, E}
    end.

-spec offer(Name :: atom(), CallId :: nonempty_string(), 
            FromTag :: nonempty_string(), SDP :: nonempty_string()) ->
    {ok, NewSDP :: nonempty_string()} | {error, Error :: term()}.
offer(Name, CallId, FromTag, SDP) -> offer(Name, CallId, FromTag, SDP, []).

-spec offer(Name :: atom(), CallId :: nonempty_string(), 
            FromTag :: nonempty_string(), SDP :: nonempty_string(), Args :: list()) ->
    {ok, NewSDP :: nonempty_string()} | {error, Error :: term()}.
offer(Name, CallId, FromTag, SDP, Args) -> 
    Args1 = [{"command", "offer"}, 
             {"call-id", CallId}, 
             {"from-tag", FromTag},
             {"sdp", SDP} | Args],
    Data = do_command(Name, Args1),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> {ok, dict:fetch(<<"sdp">>, Data)};
        E -> {error, E}
    end.

-spec answer(Name :: atom(), CallId :: nonempty_string(), 
             FromTag :: nonempty_string(), ToTag :: nonempty_string(), SDP :: nonempty_string()) ->
    {ok, NewSDP :: nonempty_string()} | {error, Error :: term()}.
answer(Name, CallId, FromTag, ToTag, SDP) -> answer(Name, CallId, FromTag, ToTag, SDP, []).

-spec answer(Name :: atom(), CallId :: nonempty_string(), 
             FromTag :: nonempty_string(), ToTag :: nonempty_string(), 
             ToTag :: nonempty_string(), SDP :: nonempty_string()) ->
    {ok, NewSDP :: nonempty_string()} | {error, Error :: term()}.
answer(Name, CallId, FromTag, ToTag, SDP, Args) -> 
    Args1 = [{"command", "answer"}, 
             {"call-id", CallId}, 
             {"from-tag", FromTag}, 
             {"to-tag", ToTag},
             {"sdp", SDP} | Args],
    Data = do_command(Name, Args1),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> {ok, dict:fetch(<<"sdp">>, Data)};
        E -> {error, E}
    end.

-spec delete(Name :: atom(), CallId :: nonempty_string(), FromTag :: nonempty_string()) ->
    {ok, NewSDP :: nonempty_string()} | {error, Error :: term()}.
delete(Name, CallId, FromTag) -> delete(Name, CallId, FromTag, []).

-spec delete(Name :: atom(), CallId :: nonempty_string(), FromTag :: nonempty_string(), Args :: list()) ->
    {ok, Data :: dict:dict()} | {error, Error :: term()}.
delete(Name, CallId, FromTag, Args) -> 
    Args1 = [{"command", "delete"}, 
             {"call-id", CallId}, 
             {"from-tag", FromTag} | Args],
    Data = do_command(Name, Args1),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> {ok, Data};
        E -> {error, E}
    end.

-spec list(Name :: atom()) ->
    {ok, List :: [binary()]} | {error, Error :: term()}.
list(Name) -> list(Name, 32).

-spec list(Name :: atom(), Limit :: integer()) ->
    {ok, List :: [binary()]} | {error, Error :: term()}.
list(Name, Limit) ->
    Args = [{"command", "list"}, {"limit", Limit}],
    Data = do_command(Name, Args),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> 
            {list, List} = dict:fetch(<<"calls">>, Data),
            {ok, List};
        E -> {error, E}
    end.

-spec query(Name :: atom(), CallId :: nonempty_string()) ->
    {ok, Data :: dict:dict()} | {error, Error :: term()}.
query(Name, CallId) -> query(Name, CallId, []).

-spec query(Name :: atom(), CallId :: nonempty_string(), Args :: list()) ->
    {ok, Data :: dict:dict()} | {error, Error :: term()}.
query(Name, CallId, Args) ->
    Args1 = [{"command", "query"}, {"call-id", CallId} | Args],
    Data = do_command(Name, Args1),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> {ok, Data};
        E -> {error, E}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================