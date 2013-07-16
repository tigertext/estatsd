%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2012 Johannes Huning
%% @doc Librato Metrics adapter, sends metrics to librato.
-module (estatsda_librato).

%% See estatsd.hrl for a complete list of introduced types.
-include ("../estatsd.hrl").

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
  Term = {struct, [{counters, CountersMessage}, {gauges, TimersMessage}]},
  % Encode the final message
  Json = mochijson2:encode(Term),
  erlang:iolist_to_binary(Json).


%% @doc Renders the counter metrics
-spec render_counters_(prepared_counters()) -> JsonStruct::term().
render_counters_(Counters) ->
  lists:map(
    fun({KeyAsBinary, Value, _NoIncrements}) ->
      case binary:split(KeyAsBinary, <<"-">>, []) of
        % A counter adhering to the group convention;
        % That is, minus ("-") separates group from actual key.
        [Group, Source] -> {struct, [
          {name, Group},
          {source, Source},
          {value, Value}
        ]};
        % This is a common counter.
        _ -> {struct, [
          {name, KeyAsBinary},
          {value, Value}
        ]}
      end
    end,
    Counters).


%% @doc Renders the timer metrics
-spec render_timers_(prepared_timers()) -> JsonStruct::term().
render_timers_(Timers) ->
  lists:map(
    fun({KeyAsBinary, Durations, Count, Min, Max}) ->
      % Calculate the sum and the sum of all squares.
      {Sum, SumSquares} = lists:foldl(fun(Duration, {SumAcc, SumSquaresAcc}) ->
        {SumAcc + Duration, SumSquaresAcc + (Duration * Duration)}
      end, {0, 0}, Durations),

      % Build Mochijson2 JSON fragment
      case binary:split(KeyAsBinary, <<"-">>, []) of
        [Group, Source] ->
          {struct, [
            {name, Group},
            {source, Source},
            {count, Count},
            {sum, Sum},
            {max, Max},
            {min, Min},
            {sum_squares, SumSquares}
          ]};

        _ ->
          {struct, [
            {name, KeyAsBinary},
            {count, Count},
            {sum, Sum},
            {max, Max},
            {min, Min},
            {sum_squares, SumSquares}
          ]}
      end
    end,
    Timers).


%% @doc Sends the given metrics JSON to librato.
-spec send_(Message::string(), State::#state{}) ->
  {error, Reason::term()} | {ok, noreply}.
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
    {ok, "400", _ResponseHeaders, ResponseBody} ->
        error_logger:error_msg("Bad request: ~s", [ResponseBody]);
    _ -> {ok, noreply}
  end.

% ====================== /\ HELPER FUNCTIONS ===================================
