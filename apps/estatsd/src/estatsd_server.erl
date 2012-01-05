%%% Stats Aggregation Process.
%%% Periodically dumps metrics to a graphing system of your choice.
%%% Inspired by etsy statsd:
%%% http://codeascraft.etsy.com/2011/02/15/measure-anything-measure-everything
%%%
%%% Richard Jones <rj@metabrew.com>
%%% Johannes Huning <hi@johanneshuning.com>

-module (estatsd_server).

-behaviour (gen_server).

% Client API
-export ([
  start_link/0
]).

% Callback functions for gen_server
-export ([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% @doc estatsd process state.
-record (state, {
  timers,          % Timer Metrics stored in a gb_tree
  flush_interval,  % Interval between stats flushing, in ms
  flush_timer      % Reference to the interval timer
}).


%% @doc Starts estatsd.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc gen_server callback,
init(_InitArgs) ->
  % Retrieve the startup options
  {ok, FlushInterval} = application:get_env(estatsd, flush_interval),
  {ok, Adapters} = application:get_env(estatsd, adapters),

  error_logger:info_msg(
    "[~s] Going to flush stats every ~wms~n", [?MODULE, FlushInterval]),

  % Start the metrics event manager
  gen_event:start_link({local, estatsd_manager}),

  % Register all specified adapters
  lists:foreach(
    fun(InitArgs) ->
      {AdapterModule, _} = InitArgs,
      error_logger:info_msg("[~s] Adding adapter: '~p'~n",
        [?MODULE, AdapterModule]),
      gen_event:add_handler(estatsd_manager, estatsd_handler, InitArgs)
    end,
    Adapters
  ),

  % Initialize the table for counter metrics
  ets:new(statsd, [named_table, set]),

  % Flush out stats periodically
  {ok, TimerRef} = timer:apply_interval(
    FlushInterval, gen_server, cast, [?MODULE, flush]),

  State = #state{
    timers         = gb_trees:empty(),
    flush_interval = FlushInterval,
    flush_timer    = TimerRef
  },
  {ok, State}.


%% @doc gen_server callback, increments or creates the given counter.
handle_cast({increment, Key, IncrementBy, SampleRate}, State)
    % Only handle increments with proper sample rates
    when SampleRate >= 0 andalso SampleRate =< 1 ->

  % Account for sample rates < 1.0
  Delta = IncrementBy * (1 / SampleRate),
  case ets:lookup(statsd, Key) of
    % Initialize new counters
    % The table holds both the counters value and the number of increments
    [] -> ets:insert(statsd, {Key, {Delta, 1}});

    % Otherwise update the counter
    [{Key, {Total, Times}}] ->
      ets:insert(statsd, {Key, {Total + Delta, Times + 1}})
  end,
  {noreply, State};


%% @doc gen_server callback, logs 'n drops increments with invalid sample rates.
handle_cast({increment, Key, _IncrementBy, SampleRate}, State) ->
  error_logger:warning_msg(
    "[~s] Requested to increment '~s' with invalid sample rate of: ~f~n",
    [?MODULE, Key, SampleRate]),
  {noreply, State};


%% @doc gen_server callback, inserts or updates the timing for the given timer.
handle_cast({timing, Key, Duration}, State) ->
  Timers = State#state.timers,
  case gb_trees:lookup(Key, Timers) of
    % Initialize new timers
    none ->
      {noreply, State#state{
        timers = gb_trees:insert(Key, [Duration], Timers)
      }};

    % Otherwise just append the measured duration
    {value, Val} ->
      {noreply, State#state{
        timers = gb_trees:update(Key, [Duration|Val], Timers)
      }}
  end;


%% @doc Flushes the current metrics to estats_pub.
handle_cast(flush, State) ->
  % Publish the metrics in another process
  Counters = ets:tab2list(statsd),
  Timers = gb_trees:to_list(State#state.timers),
  spawn(fun() -> publish_metrics_(Counters, Timers) end ),

  % Clear the metrics table and reset all timing counters
  ets:delete_all_objects(statsd),
  NewState = State#state{timers = gb_trees:empty()},
  % Continue with a blank slate
  {noreply, NewState}.


%% @doc Publishes the metrics using the estatsd_manager event manager
publish_metrics_(Counters, Timers) ->
  case length(Counters) + length(Timers) of
    0 -> emptyset;  % Only send an event if there are any metrics at all
    _ -> gen_event:notify(estatsd_manager, {publish, {Counters, Timers}})
  end.


%% @doc gen_server callback, logs and drops.
handle_call(Request, From, State) ->
  error_logger:warning_msg(
    "[~s] Ignored call '~p' from '~w'~n", [?MODULE, Request, From]),
  {reply, ok, State}.


%% @doc gen_server callback, logs and drops.
handle_info(Info, State) ->
  error_logger:info_msg(
    "[~s] Ignored info '~p'~n", [?MODULE, Info]),
  {noreply, State}.


%% @doc gen_server callback, continues with the old state.
code_change(_OldVsn, State, _Extra) -> {noreply, State}.


%% @doc gen_server callback, just returns ok.
terminate(_Arg, _State) -> ok.
