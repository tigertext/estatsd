-module(estatsd_udp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(metric_type_map, [{<<"g">>, {folsom_metrics, new_gauge}},
                          {<<"m">>, {folsom_metrics, new_meter}},
                          {<<"h">>, {folsom_metrics, new_histogram}}
                         ]).
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
-record(state, {port,
                socket}).

init([]) ->
    {ok, Port} = application:get_env(estatsd, udp_listen_port),
    {ok, RecBuf} = application:get_env(estatsd, udp_recbuf),
    error_logger:info_msg("estatsd will listen on UDP ~p with recbuf ~p~n",
                          [Port, RecBuf]),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, once},
                                       {recbuf, RecBuf}]),
    {ok, #state{port = Port, socket = Socket}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, _Host, _Port, Bin}, State) ->
    proc_lib:spawn(fun() -> handle_message(Bin) end),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_message(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    [ parse_line(L) || L <- Lines ],
    ok.

parse_line(<<>>) ->
    skip;
parse_line(Bin) ->
    [Key, Value, Type] = binary:split(Bin, [<<":">>, <<"|">>], [global]),
    send_metric(Type, Key, Value),
    ok.

send_metric(Type, Key, Value) when Type =:= <<"d">> orelse Type =:= <<"c">> ->
    {EstatsFun, FolsomTag} = case Type of
                                 <<"d">> -> {decrement, dec};
                                 <<"c">> -> {increment, inc}
                             end,
    IntValue = ?to_int(Value),
    estatsd:EstatsFun(Key, IntValue),
    folsom_metrics:new_counter(Key),
    folsom_metrics:notify({Key, {FolsomTag, IntValue}});
send_metric(<<"ms">>, Key, Value) ->
    IntValue = ?to_int(Value),
    estatsd:timing(Key, IntValue),
    folsom_metrics:new_histogram(Key),
    folsom_metrics:notify({Key, IntValue});
send_metric(<<"e">>, Key, Value) ->
    folsom_metrics:new_history(Key),
    folsom_metrics:notify({Key, Value});
send_metric(Type, Key, Value) ->
    IntValue = ?to_int(Value),
    case proplists:get_value(Type, ?metric_type_map) of
        undefined ->
            erlang:error({unknown_metric_type, Type, IntValue});
        {Mod, Fun} ->
            Mod:Fun(Key),
            folsom_metrics:notify({Key, IntValue})
    end.


% TODO:
        % <<"mr">> ->
        %     % meter reader (expects monotonically increasing values)
        %     erlang:error({not_implemented, <<"mr">>});
