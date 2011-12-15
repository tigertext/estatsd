-module (estatsd_pub).

% Client API
-export ([
  start_link/0
]).


%% @doc Starts the estatsd event manager to publish metrics via its handlers.
start_link() ->
  % Start the event manager itself
  {ok, Pid} = gen_event:start_link({local, estatsd_pub}),
  {ok, HandlerList} = application:get_env(estatsd, handler_list),
  % Add all the handlers asked for to the event manager
  lists:foreach(
    fun({Adapter, Args}) ->
      gen_event:add_handler(Pid, estatsd_adapter, Args),
      error_logger:info_msg("[~s] Added handler: '~w'~n", [?MODULE, Adapter])
    end,
    HandlerList
  ).
