-module (estatsd_graphite).

-behaviour (gen_event).

% gen_event behaviour callbacks
-export ([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% @doc estatsd_graphite process state, just hostname and port number.
-record (state, {host, port}).


%% @doc gen_event callback, builds the process' initial state.
init(_InitArgs) ->
  % Read the Graphite remote host configuration from the environment
  {ok, Host} = application:get_env(estatsd, graphite_host),
  {ok, Port} = application:get_env(estatsd, graphite_port),
  error_logger:info_msg(
    "[~s] Going to send metrics to Graphite at: '~s:~s'~n",
    [?MODULE, Host, Port]),

  State = #state{host = Host, port = Port},
  {ok, State}.


%% @doc gen_event callback, sends recorded metrics to Graphite.
handle_event({publish, Metrics}, State) ->
  % Publish the metrics in another process, immediately return.
  spawn(fun() -> publish_(Metrics, State) end),
  {ok, State};


%% @doc gen_event callback, logs and drops unexpected events.
handle_event(Event, State) ->
  error_logger:warning_msg(
    "[~s] Received unexpected event: '~p'~n", [?MODULE, Event]),
  {ok, State}.


%% @doc gen_event callback, synchronously sends recorded metrics to Graphite.
handle_call({publish, Metrics}, State) ->
  % Publish the metrics in the calling process, return only after delivery
  Reply = publish_(Metrics, State),
  {ok, State, Reply};


%% @doc gen_event callback, logs and drops unexpected calls.
handle_call(Request, State) ->
  error_logger:warning_msg(
    "[~s] Received unexpected call: '~p'~n", [?MODULE, Request]),
  {ok, State, undefined}.


%% @doc gen_event callback, logs and drops unexpected infos.
handle_info(Info, State) ->
  error_logger:warning_msg(
    "[~s] Received unexpected info: '~p'~n", [?MODULE, Info]),
  {ok, State}.


%% @doc gen_event callback, Just the old state, no cleanup required.
terminate(_Arg, State) -> State.


%% @doc gen_event callback, Just the old state, no update required.
code_change(_OldVsn, State, _Extra) -> State.


%% @doc Aggregate the stats and generate a report to send to graphite
publish_(Metrics, State) ->
  case render_(Metrics) of
    % Don't actuelly send anything if there is nothing to report
    undefined -> ok;
    Message -> send_(Message, State)
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
