-module(estatsd_udp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(to_int(Value), list_to_integer(binary_to_list(Value))).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {port          :: non_neg_integer(),
                socket        :: inet:socket(),
                batch = []    :: [binary()],
                batch_max     :: non_neg_integer(),
                batch_max_age :: non_neg_integer()
               }).

init([]) ->
    {ok, Port} = application:get_env(estatsd, udp_listen_port),
    {ok, RecBuf} = application:get_env(estatsd, udp_recbuf),
    {ok, BatchMax} = application:get_env(estatsd, udp_max_batch_size),
    {ok, BatchAge} = application:get_env(estatsd, udp_max_batch_age),
    error_logger:info_msg("estatsd will listen on UDP ~p with recbuf ~p~n",
                          [Port, RecBuf]),
    error_logger:info_msg("batch size ~p with max age of ~pms~n",
                          [BatchMax, BatchAge]),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, once},
                                       {recbuf, RecBuf}]),
    {ok, #state{port = Port, socket = Socket,
                batch = [],
                batch_max = BatchMax,
                batch_max_age = BatchAge}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({udp, Socket, _Host, _Port, Bin},
            #state{batch=Batch, batch_max=Max,
                   batch_max_age=MaxAge}=State) when length(Batch) == Max ->
    error_logger:info_msg("spawn batch ~p FULL~n", [Max]),
    start_batch_worker(Batch),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{batch=[Bin]}};
handle_info({udp, Socket, _Host, _Port, Bin}, #state{batch=Batch,
                                                     batch_max_age=MaxAge}=State) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{batch=[Bin|Batch]}, MaxAge};
handle_info(timeout, #state{batch=Batch, batch_max_age=MaxAge}=State) ->
    error_logger:info_msg("spawn batch ~p TIMEOUT~n", [length(Batch)]),
    start_batch_worker(Batch),
    {noreply, State#state{batch=[]}};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
start_batch_worker(Batch) ->
    %% Make sure we process messages in the order received
    proc_lib:spawn(fun() -> handle_messages(lists:reverse(Batch)) end).

handle_messages(Batch) ->
    [ handle_message(M) || M <- Batch ],
    ok.
handle_message(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    [ parse_line(L) || L <- Lines ],
    ok.

parse_line(<<>>) ->
    skip;
parse_line(Bin) ->
    [Key, Value, Type] = binary:split(Bin, [<<":">>, <<"|">>], [global]),
    {ok, _} = estatsd_folsom:ensure_metric(Key, Type),
    send_metric(Type, Key, Value),
    ok.

send_metric(Type, Key, Value) when Type =:= <<"d">> orelse Type =:= <<"c">> ->
    {EstatsFun, FolsomTag} = case Type of
                                 <<"d">> -> {decrement, dec};
                                 <<"c">> -> {increment, inc}
                             end,
    IntValue = ?to_int(Value),
    estatsd:EstatsFun(Key, IntValue),
    folsom_metrics:notify({Key, {FolsomTag, IntValue}});
send_metric(<<"ms">>, Key, Value) ->
    IntValue = ?to_int(Value),
    estatsd:timing(Key, IntValue),
    folsom_metrics:notify({Key, IntValue});
send_metric(<<"e">>, Key, Value) ->
    folsom_metrics:notify({Key, Value});
send_metric(_Type, Key, Value) ->
    IntValue = ?to_int(Value),
    folsom_metrics:notify({Key, IntValue}).

% TODO:
        % <<"mr">> ->
        %     % meter reader (expects monotonically increasing values)
        %     erlang:error({not_implemented, <<"mr">>});
