-module(estatsd_app).

-behaviour(application).

% application behaviour callbacks
-export([
  start/2,
  stop/1
]).


% @doc application callback, starts the estatsd main supervisor
start(_StartType, _StartArgs) -> estatsd_sup:start_link().

% @doc application callback, Just ok.
stop(_State) -> ok.
