-module (estatsd_graphite).

-behaviour (estatsd_adapter).

% estatsd_adapter behaviour callbacks
-export ([
  init/1,
  handle_metrics/2
]).

%% @doc estatsd_graphite process state, just hostname and port number.
-record (state, {host, port}).


%% @doc estatsd_adapter callback, builds estatsd_graphite's initial state.
init(_InitArgs) ->
  % Read the Graphite remote host configuration from the environment
  {ok, Host} = application:get_env(estatsd, graphite_host),
  {ok, Port} = application:get_env(estatsd, graphite_port),
  error_logger:info_msg(
    "[~s] Going to send metrics to Graphite at: '~s:~s'~n",
    [?MODULE, Host, Port]),

  State = #state{host = Host, port = Port},
  {ok, State}.


%% @doc estatsd_adapter callback, Sends the recorded metrics to Graphite.
handle_metrics(Metrics, State) ->
  spawn(fun() -> sync_handle_metrics(Metrics, State) end),
  {ok, State}.


%% @doc estatsd_adapter callback, Sends the recorded metrics to Graphite.
sync_handle_metrics(Metrics, State) ->
  case render_(Metrics) of
    % Don't actuelly send anything if there is nothing to report
    undefined -> {ok, State, ok};
    Message -> {ok, State, send_(Message, State)}
  end.


%% @doc Renders recorded metrics into a message readable by Graphite.
render_({Counters, Timers}) ->
  % One timestamp used in all stats lines
  Timestamp = estatsd:num2str(estatsd:unixtime()),

  CounterMessage = render_counters_(Counters, Timestamp),
  TimersMessage = render_timers_(Timers, Timestamp),

  case length(Counters) + length(TimersMessage) of
    % Nothing to report!
    0 -> undefined;
    NumStats -> [
      % Build the final message send to Graphite
      CounterMessage, TimersMessage,
      % Also graph the number of graphs we're graphing
      "statsd.numStats ", estatsd:num2str(NumStats), " ", Timestamp, "\n"
    ]
  end.


%% @doc Sends an already rendered message to Graphite via TCP.
send_(Message, #state{host = Host, port = Port}) ->
  error_logger:info_msg("[~s] Sending data to Graphite ...~n", [?MODULE]),

  case gen_tcp:connect(Host, Port, [list, {packet, 0}]) of
    {ok, Socket} ->
      gen_tcp:send(Socket, Message),
      gen_tcp:close(Socket),
      ok;
    Error ->
      error_logger:error_msg(
        "[~s] Failed to connect to Graphite: ~p", [?MODULE, Error]),
      Error
  end.


%% @doc Renders the counter metrics into a message readable by Graphite.
render_counters_(Counters, Timestamp) ->
  {ok, FlushInterval} = application:get_env(estatsd, flush_interval),
  lists:foldl(
    fun({Key, {Value, Times}}, Acc) ->
      KeyString = estatsd:key2str(Key),
      ValuePerSec = Value / (FlushInterval / 1000),

      % Build stats string fragment for Graphite
      Fragment = [
        "stats.", KeyString, " ",
          estatsd:num2str(ValuePerSec), " ", Timestamp, "\n",

        "stats_counts.", KeyString, " ",
          estatsd:num2str(Times), " ", Timestamp, "\n"
      ],
      % Fold step
      [Fragment | Acc]
    end,
    [], Counters).


%% @doc Renders the timer metrics into a message readable by Graphite.
render_timers_(Timers, Timestamp) ->
  lists:foldl(
    fun({Key, Durations_}, Acc) ->
      KeyString = estatsd:key2str(Key),
      Durations = lists:sort(Durations_),

      Count = length(Durations),
      Min = hd(Durations),
      Max = lists:last(Durations),

      UpperThreshold = 90, % In percent
      NumUpperValues  = erlang:round(((100 - UpperThreshold) / 100) * Count),
      NumLowerValues  = Count - NumUpperValues,

      LowerDurations = lists:sublist(Durations, NumLowerValues),
      LowerMax = lists:nth(NumLowerValues, Durations),
      Mean = lists:sum(LowerDurations) / NumLowerValues,

      Prefix = ["stats.timers.", KeyString, "."],
      Postfix = [" ", Timestamp, "\n"],

      % Build stats string fragment for Graphite
      Fragment = [lists:map(
        fun({Name, Value}) ->
          [Prefix, Name, " ", estatsd:num2str(Value), Postfix]
        end,
        % Wrap every entry with prefix and postfix
        [
          {"mean", Mean},
          {"upper", Max},
          {"upper_" ++ estatsd:num2str(UpperThreshold), LowerMax},
          {"lower", Min},
          {"count", Count}
        ]
      )],
      % Fold step
      [Fragment | Acc]
  end,
  [], Timers).
