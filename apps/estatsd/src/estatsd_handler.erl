-module (estatsd_handler).

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

%% @doc Backing adapter's state
-record (state, {adapter, adapter_state}).


%% @doc gen_event callback, initializes the backing adapter.
init({Adapter, Args}) ->
  {ok, AdapterState} = Adapter:init(Args),
  State = #state{adapter = Adapter, adapter_state = AdapterState},
  {ok, State}.


%% @doc gen_event callback, forwards recorded metrics to the adapter.
handle_event({publish, Metrics}, State) ->
  Adapter = State#state.adapter,
  AState = State#state.adapter_state,
  % Forward the metrics to the adapter
  {ok, NewAState} = Adapter:handle_metrics(Metrics, AState),
  {ok, State#state{adapter_state = NewAState}};


%% @doc gen_event callback, logs and drops unexpected events.
handle_event(Event, State) ->
  error_logger:warning_msg(
    "[~s] Received unexpected event: '~p'~n", [?MODULE, Event]),
  {ok, State}.


%% @doc gen_event callback, synchronously sends recorded metrics to Graphite.
handle_call({publish, Metrics}, State) ->
  Adapter = State#state.adapter,
  AState = State#state.adapter_state,
  % Forward the metrics to the adapter
  {ok, NewAState, Reply} = Adapter:sync_handle_metrics(Metrics, AState),
  {ok, State#state{adapter_state = NewAState}, Reply};


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
