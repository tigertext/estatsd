%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2012 Johannes Huning
%% @doc Librato Metrics adapter, sends metrics to librato.
-module (estatsda_librato).

%% This is an estatsd adapter.
-behaviour (estatsda_adapter).

% Adapter callbacks.
-export ([
  init/1,
  handle_metrics/2,
  sync_handle_metrics/2
]).

%% @doc Process state: Librato Metrics API username and auth-token.
-record (state, {
  user :: string(),
  token :: string()
}).


% ====================== \/ ESTATSD_ADAPTER CALLBACKS ==========================

%% @doc gen_event callback, builds estatsd_librato's initial state.
init({User, Token}) ->
  State = #state{user = User, token = Token},
  {ok, State}.


%% @doc estatsd_adapter callback, asynchronously calls sync_handle_metrics.
handle_metrics(Metrics, State) ->
  spawn(fun() -> sync_handle_metrics(Metrics, State) end),
  {ok, State}.


%% @doc estatsd_adapter callback, sends a report to send to Librato Metrics.
sync_handle_metrics(Metrics, State) ->
  {ok, send_(render_(Metrics), State)}.

% ====================== /\ ESTATSD_ADAPTER CALLBACKS ==========================


% ====================== \/ HELPER FUNCTIONS ===================================

%% @doc Renders recorded metrics into a message readable by Librato Metrics.
render_({Counters, Timers}) ->
  CountersMessage = render_counters_(Counters),
  TimersMessage = render_timers_(Timers),

  % Mochijson2 JSON struct
  Term = {struct, [{gauges, CountersMessage}, {counters, TimersMessage}]},

  % Encode the final message
  Msg = erlang:iolist_to_binary(mochijson2:encode(Term)),
  io:format("~s~n", [Msg]),
  Msg.


%% @doc Renders the counter metrics
render_counters_(Counters) ->
  {ok, FlushInterval} = application:get_env(estatsd, flush_interval),
  lists:map(
    fun({Key, {Value, _NoIncrements}}) ->
      KeyString = erlang:list_to_binary(estatsd:key2str(Key)),
      ValuePerSec = Value / (FlushInterval / 1000),

      % Build Mochijson2 JSON fragment
      {struct, [{name, KeyString}, {value, ValuePerSec}]}
    end,
    Counters).


%% @doc Renders the timer metrics
render_timers_(_Timers) -> "".  % TODO: Implement


%% @doc
send_(Message, #state{user = User, token = Token}) ->
  Url = "https://metrics-api.librato.com/v1/metrics.json",

  Headers = [
    {"connection", "keep-alive"},
    {"content-type", "application/json"}
  ],
  Options = [
    {basic_auth, {User, Token}},
    {connect_timeout, 2000}
  ],

  case ibrowse:send_req(Url, Headers, post, Message, Options, 5000) of
    {error, Reason} ->
      error_logger:error_msg("[~s] Delivery failed: '~p'", [?MODULE, Reason]),
      {error, Reason};

    _ -> {ok, noreply}
  end.

% ====================== /\ HELPER FUNCTIONS ===================================
