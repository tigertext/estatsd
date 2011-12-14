-module(estatsd_sup).

-behaviour(supervisor).

%% Client API
-export([
  start_link/0
]).

% supervisor behaviour callbacks
-export([
  init/1
]).


% @doc Starts estatsd's main supervisor, registers estatsd_sup locally.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).


% @doc supervisor callback, starts the udp- and estatsd-server.
init(_) ->
  ChildSpec = [
    worker_spec_(estatsd_server),
    worker_spec_(estatsd_udp)
  ],
  RestartStrategy = one_for_one,
  % If more than MaxRestarts number of restarts occur in the last
  % RestartTimeframe seconds, then the supervisor terminates all the child
  % processes and then itself.
  MaxRestarts = 10000,
  RestartTimeframe = 10,
  % Supervisor spec itself
  {ok, {{RestartStrategy, MaxRestarts, RestartTimeframe}, ChildSpec}}.


% @doc Builds a worker spec. Gives tuple-hell some semantics.
worker_spec_(Module) ->
  Id = Module,
  StartFunc = {Module, start_link, []},
  RestartType = permanent,
  ShutdownTimeout = 5000,
  ChildType = worker,
  Modules = [Module],
  {Id, StartFunc, RestartType, ShutdownTimeout, ChildType, Modules}.
