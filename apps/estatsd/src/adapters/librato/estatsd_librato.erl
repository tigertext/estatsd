-module (estatsd_librato).

-behaviour (estatsd_adapter).

% estatsd_adapter behaviour callbacks
-export ([
  init/1,
  handle_metrics/2,
  sync_handle_metrics/2
]).

%% @doc estatsd_librato process state, librato api username and auth-token.
-record (state, {user, token}).


%% @doc gen_event callback, builds estatsd_librato's initial state.
init({User, Token}) ->
  error_logger:info_msg(
    "[~s] Going to send metrics to Librato as user: '~s'~n",
    [?MODULE, User]),

  State = #state{user = User, token = Token},
  {ok, State}.


%% @doc estatsd_adapter callback, asynchronously calls sync_handle_metrics.
handle_metrics(Metrics, State) ->
  spawn(fun() -> sync_handle_metrics(Metrics, State) end),
  {ok, State}.


%% @doc estatsd_adapter callback, sends a report to send to Librato Metrics.
sync_handle_metrics(Metrics, State) ->
  {ok, State, send_(render_(Metrics), State)}.


%% @doc Renders recorded metrics into a message readable by Librato Metrics.
render_({Counters, Timers}) ->
  CountersMessage = render_counters_(Counters),
  TimersMessage = render_timers_(Timers),

  % Mochijson2 JSON struct
  Term = {struct, [{counters, CountersMessage}, {gauges, TimersMessage}]},

  % Encode the final message
  Msg = erlang:iolist_to_binary(mochijson2:encode(Term)),
  io:format("~s~n", [Msg]),
  Msg.


%% @doc Renders the counter metrics
render_counters_(Counters) ->
  {ok, FlushInterval} = application:get_env(estatsd, flush_interval),
  lists:map(
    fun({Key, {Value, Times}}) ->
      KeyString = erlang:list_to_binary(estatsd:key2str(Key)),
      ValuePerSec = Value / (FlushInterval / 1000),

      % Build Mochijson2 JSON fragment
      {struct, [{name, KeyString}, {value, ValuePerSec}]}
    end,
    Counters).


%% @doc Renders the timer metrics
render_timers_(Timers) -> "".  % Empty for now


%% @doc
send_(Message, #state{user = User, token = Token}) ->
  % Call to the Librato Metrics API
  librato:create_metrics(User, Token, "localhost", Message, undefined).
