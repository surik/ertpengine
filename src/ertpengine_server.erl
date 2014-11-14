-module(ertpengine_server).

-behaviour(gen_server).

%% API
-export([start_link/2,
         do_command/2
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
    proxy_port = 2223,
    tx_list = gb_trees:empty()
}).

-define(COOKIE_LEN, 16).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

do_command(Name, Args) ->
    {{dict, Data}, _} = gen_server:call(Name, {do_command, Args}),
    Data.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    IP = proplists:get_value(ip, Args, "127.0.0.1"),
    Port = proplists:get_value(port, Args, 2223),
    {ok, #state{socket = Socket, proxy_ip = IP, proxy_port = Port}}.

handle_call({do_command, Args}, From, #state{tx_list = TXList, socket = Socket, proxy_ip = IP, proxy_port = Port} = State) ->
    Cookie = cookie(),
    Data = bencode(Args),
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
            gen_server:reply(From, bencode:decode(Data))
    end,
    {noreply, State#state{tx_list = gb_trees:delete_any(Cookie, TXList)}};

handle_info(_Info, State) ->
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
