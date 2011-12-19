-module (estatsd_librato).

-behaviour (estatsd_adapter).

% estatsd_adapter behaviour callbacks
-export ([
  init/1,
  handle_metrics/2
]).

%% @doc estatsd_librato process state, librato api username and auth-token.
-record (state, {user, token}).


%% @doc gen_event callback, builds estatsd_librato's initial state.
init(_InitArgs) ->
  % Read the Graphite remote host configuration from the environment
  {ok, User} = application:get_env(estatsd, librato_user),
  {ok, Token} = application:get_env(estatsd, librato_token),
  error_logger:info_msg(
    "[~s] Going to send metrics to Librato with user: '~s'~n",
    [?MODULE, User]),

  State = #state{user = User, token = Token},
  {ok, State}.


%% @doc Aggregate the stats and generate a report to send to Librato
%% TODO: Implement!
handle_metrics(_Metrics, _State) -> undefined.
