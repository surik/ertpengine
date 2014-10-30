-module(ertpengine).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_link/2,
         do_command/1,
         ping/0,
         offer/3, offer/4,
         answer/4, answer/5,
         delete/2, delete/3,
         list/0, list/1,
         query/1, query/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    socket,
    proxy_ip = "127.0.0.1",
    proxy_port = 11234,
    tx_list = gb_trees:empty()
}).

-define(COOKIE_LEN, 16).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Ip) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{ip, Ip}], []).

start_link(Ip, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{ip, Ip}, {port, Port}], []).

do_command(Args) ->
    {{dict, Data}, _} = gen_server:call(?MODULE, {do_command, Args}),
    Data.

ping() ->
    Args = [{"command", "ping"}],
    case dict:fetch(<<"result">>, do_command(Args)) of
        <<"pong">> -> ok;
        E -> {error, E}
    end.

offer(CallId, FromTag, SDP) -> offer(CallId, FromTag, SDP, []).
offer(CallId, FromTag, SDP, Args) -> 
    Args1 = [{"command", "offer"}, 
             {"call-id", CallId}, 
             {"from-tag", FromTag},
             {"sdp", SDP} | Args],
    Data = do_command(Args1),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> {ok, dict:fetch(<<"sdp">>, Data)};
        E -> {error, E}
    end.

answer(CallId, FromTag, ToTag, SDP) -> answer(CallId, FromTag, ToTag, SDP, []).
answer(CallId, FromTag, ToTag, SDP, Args) -> 
    Args1 = [{"command", "answer"}, 
             {"call-id", CallId}, 
             {"from-tag", FromTag}, 
             {"to-tag", ToTag},
             {"sdp", SDP} | Args],
    Data = do_command(Args1),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> {ok, dict:fetch(<<"sdp">>, Data)};
        E -> {error, E}
    end.

delete(CallId, FromTag) -> delete(CallId, FromTag, []).
delete(CallId, FromTag, Args) -> 
    Args1 = [{"command", "delete"}, 
             {"call-id", CallId}, 
             {"from-tag", FromTag} | Args],
    Data = do_command(Args1),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> {ok, Data};
        E -> {error, E}
    end.

list() -> list(32).
list(Limit) ->
    Args = [{"command", "list"}, {"limit", Limit}],
    Data = do_command(Args),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> 
            {list, List} = dict:fetch(<<"calls">>, Data),
            {ok, List};
        E -> {error, E}
    end.

query(CallId) -> query(CallId, []).
query(CallId, Args) ->
    Args1 = [{"command", "query"}, {"call-id", CallId} | Args],
    Data = do_command(Args1),
    case dict:fetch(<<"result">>, Data) of
        <<"ok">> -> {ok, Data};
        E -> {error, E}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    IP = proplists:get_value(ip, Args, "127.0.0.1"),
    Port = proplists:get_value(port, Args, 11234),
    {ok, #state{socket = Socket, proxy_ip = IP, proxy_port = Port}}.

handle_call({do_command, Args}, From, #state{tx_list = TXList, socket = Socket, proxy_ip = IP, proxy_port = Port} = State) ->
    error_logger:info_msg("do_command: ~p", [Args]),
    Cookie = cookie(),
    Data = bencode(Args),
    error_logger:info_msg("Packet: ~p ~p", [Cookie, Data]),
    gen_udp:send(Socket, IP, Port, <<Cookie/binary, " ", Data/binary>>),
    {noreply, State#state{tx_list = gb_trees:enter(Cookie, From, TXList)}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _, _, _, <<Cookie:?COOKIE_LEN/binary, " ", Data/binary>>}, #state{tx_list = TXList} = State) ->
    case gb_trees:lookup(Cookie, TXList) of
        none -> 
            error_logger:error_msg("Not found tx for: ~p ~p", [Cookie, Data]);
        {value, From} -> 
            error_logger:info_msg("Recieved data: ~p for ~p", [Data, From]),
            gen_server:reply(From, bencode:decode(Data))
    end,
    {noreply, State#state{tx_list = gb_trees:delete_any(Cookie, TXList)}};

handle_info(_Info, State) ->
    error_logger:warning_msg("Unknown info message: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
cookie() ->
    Len = round(?COOKIE_LEN/2),
    << <<Y>> || <<X:4>> <= crypto:strong_rand_bytes(Len), 
                Y <- integer_to_list(X, 16) >>.

bencode(List) ->
    Dict = lists:foldl(fun({K, V}, D) ->
                           dict:store(K, V, D)
                       end, dict:new(), List),
    Data = {dict, Dict},
    bencode:encode(Data).
