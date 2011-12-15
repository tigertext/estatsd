-module (estatsd_adapter).

-behaviour (gen_event).

% Behaviour definition
-export ([
  behaviour_info/1
]).

% gen_event behaviour callbacks
-export ([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% @doc
-record (state, {adapter, adapter_state}).


%% @doc
behaviour_info(callbacks) ->
  [
    {init,1},
    {handle_metrics, 2}
  ];

behaviour_info(_) -> undefined.


%% @doc
init({Adapter, Args}) ->
  State = #state{adapter = Adapter, adapter_state = Adapter:init(Args)},
  {ok, State}.


%% @doc gen_event callback, forwards recorded metrics to the adapter.
handle_event({publish, Metrics}, State) ->
  Adapter = State#state.adapter,
  AdapterState = State#state.adapter_state,
  % Publish the metrics in another process, immediately return
  spawn(fun() -> Adapter:handle_metrics(Metrics, AdapterState) end),
  % Do not respect any state changes in the adapter itself, just fire and forget
  {ok, State};


%% @doc gen_event callback, logs and drops unexpected events.
handle_event(Event, State) ->
  error_logger:warning_msg(
    "[~s] Received unexpected event: '~p'~n", [?MODULE, Event]),
  {ok, State}.


%% @doc gen_event callback, synchronously sends recorded metrics to Graphite.
handle_call({publish, Metrics}, State) ->
  Adapter = State#state.adapter,
  AdapterState = State#state.adapter_state,
  % Publish the metrics in the calling process, return only after delivery
  Reply = Adapter:handle_metrics(Metrics, AdapterState),
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
