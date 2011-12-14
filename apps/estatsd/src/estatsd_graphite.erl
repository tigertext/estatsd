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



% @doc gen_event callback,
handle_event(Event, State) -> undefined.


% @doc gen_event callback,
handle_event(Event, State) -> undefined.


% @doc gen_event callback,
handle_call(Request, State) -> undefined.


% @doc gen_event callback,
handle_call(Request, State) -> undefined.


% @doc gen_event callback,
handle_info(Info, State) -> undefined.


% @doc gen_event callback,
terminate(Arg, State) -> undefined.


% @doc gen_event callback,
code_change(OldVsn, State, Extra) -> undefined.
