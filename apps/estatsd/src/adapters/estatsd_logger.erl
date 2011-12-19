-module (estatsd_logger).

-behaviour (estatsd_adapter).

% estatsd_adapter behaviour callbacks
-export ([
  init/1,
  handle_metrics/2,
  sync_handle_metrics/2
]).


%% @doc estatsd_adapter callback, initializes the metrics logger
init(_InitArgs) -> nostate.


%% @doc estatsd_adapter callback, asynchronously calls sync_handle_metrics.
handle_metrics(Metrics, State) ->
  spawn(fun() -> sync_handle_metrics(Metrics, State) end),
  {ok, State}.


%% @doc estatsd_adapter callback, simply logs the metrics.
sync_handle_metrics(Metrics, State) ->
  io:format("[~s] Metrics: ~p~n", [?MODULE, Metrics]),
  {ok, State, noreply}.
