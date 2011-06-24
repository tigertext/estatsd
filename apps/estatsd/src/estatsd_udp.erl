-module(estatsd_udp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(metric_type_map, [{<<"g">>, {folsom_metrics, new_guage}},
                          {<<"m">>, {folsom_metrics, new_meter}},
                          {<<"h">>, {folsom_metrics, new_histogram}}
                         ]).

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
    {ok, Socket} = gen_udp:open(Port, [binary, {active, once}]),
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
    IntValue = list_to_integer(binary_to_list(Value)),
    case Type of
        <<"d">> ->
            % counter decrement
            estatsd:decrement(Key, IntValue),
            folsom_metrics:new_counter(Key),
            folsom_metrics:notify({Key, {dec, IntValue}});
        <<"c">> ->
            % counter increment
            estatsd:increment(Key, IntValue),
            folsom_metrics:new_counter(Key),
            folsom_metrics:notify({Key, {inc, IntValue}});
        <<"ms">> ->
            % time (histogram)
            estatsd:timing(Key, IntValue),
            folsom_metrics:new_histogram(Key),
            folsom_metrics:notify({Key, IntValue});
        _Else ->
            case proplists:get_value(Type, ?metric_type_map) of
                undefined ->
                    erlang:error({unknown_metric_type, Type, IntValue});
                {Mod, Fun} ->
                    Mod:Fun(Key),
                    folsom_metrics:notify({Key, IntValue})
            end
    end,
    ok.


% TODO:
        % <<"mr">> ->
        %     % meter reader (expects monotonically increasing values)
        %     erlang:error({not_implemented, <<"mr">>});
        % <<"e">> ->
        %     % histories (events)
        %     % implementing will require different handling of message
        %     % since value is essentially arbitrary binary (hopefully string).
        %     erlang:error({not_implemented, <<"e">>})
