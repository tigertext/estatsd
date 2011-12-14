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


% @doc gen_event callback, builds the process' initial state.
init(_InitArgs) ->
  % Read the Graphite remote host configuration from the environment
  {ok, Host} = application:get_env(estatsd, graphite_host),
  {ok, Port} = application:get_env(estatsd, graphite_port),
  error_logger:info_msg(
    "[~s] Going to send metrics to Graphite at: '~s:~s'~n",
    [?MODULE, Host, Port]),

  State = #state{host = Host, port = Port},
  {ok, State}.


% @doc gen_event callback, sends recorded metrics to Graphite.
handle_event({publish, Metrics}, State) ->
  % Publish the metrics in another process, immediately return.
  spawn(fun() -> publish_(Metrics, State) end),
  {ok, State};


% @doc gen_event callback, logs and drops unexpected events.
handle_event(Event, State) ->
  error_logger:warning_msg(
    "[~s] Received unexpected event: '~p'~n", [?MODULE, Event]),
  {ok, State}.


% @doc gen_event callback, synchronously sends recorded metrics to Graphite.
handle_call({publish, Metrics}, State) ->
  % Publish the metrics in the calling process, return only after delivery
  Reply = publish_(Metrics, State),
  {ok, State, Reply};


% @doc gen_event callback, logs and drops unexpected calls.
handle_call(Request, State) ->
  error_logger:warning_msg(
    "[~s] Received unexpected call: '~p'~n", [?MODULE, Request]),
  {ok, State, undefined}.


% @doc gen_event callback,
handle_info(Info, State) ->
  error_logger:warning_msg(
    "[~s] Received unexpected info: '~p'~n", [?MODULE, Info]),
  {ok, State}.


% @doc gen_event callback, Just the old state, no cleanup required.
terminate(_Arg, State) -> State.


% @doc gen_event callback, Just the old state, no update required.
code_change(_OldVsn, State, _Extra) -> State.


% @doc
publish_(Metrics, State) -> undefined.


% @doc
render_(Metrics) -> undefined.


% @doc
send_(Message, State) -> undefined.
