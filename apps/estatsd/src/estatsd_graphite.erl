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


% @doc gen_event callback, 
init(InitArgs) -> undefined.


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
