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


%% @doc estatsd_adapter callback, sends a report to send to Librato Metrics
sync_handle_metrics(Metrics, State) ->
  io:format("[~s] Pretending to send metrics to Librato...~n", [?MODULE]),
  {ok, State, noreply}.
