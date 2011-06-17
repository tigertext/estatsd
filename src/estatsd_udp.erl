-module(estatsd_udp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Port) ->
    gen_server:start_link(?MODULE, Port, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {port,
                socket}).

init(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    {ok, #state{port = Port, socket = Socket}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _Host, _Port, Bin}, State) ->
    proc_lib:spawn(fun() -> handle_message(Bin) end),
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
    % FIXME: faster/more direct way?
    IntValue = list_to_integer(binary_to_list(Value)),
    case Type of
        <<"d">> -> estatsd:decrement(Key, IntValue);
        <<"c">> -> estatsd:increment(Key, IntValue);
        <<"ms">> -> estatsd:timing(Key, IntValue)
    end,
    io:format("Handled: ~p~n", [[Key, IntValue, Type]]),
    ok.
