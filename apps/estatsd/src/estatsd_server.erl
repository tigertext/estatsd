%% @author Richard Jones <rj@metabrew.com>
%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2011 Richard Jones
%% @doc Stats aggregation process, periodically dumping metrics.
-module (estatsd_server).

%% This is an OTP gen_server.
-behaviour (gen_server).

%% Client API
-export ([
  start_link/0
]).

%% OTP gen_server callbacks.
-export ([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% The state, consisting of settings and the timer metrics.
-record (state, {
  timers,  % Timers stored in a gb_tree
  flush_interval :: non_neg_integer(),  % Interval between flushing stats, in ms
  flush_timer :: non_neg_integer() % Reference to the interval timer
}).


% ====================== \/ CLIENT API =========================================

%% @doc Starts the estatsd statistics server.
%%      Registers a process named `estatsd_server`.
-spec start_link() -> {ok, Pid::pid()} | {error, Reason::term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% ====================== /\ CLIENT API =========================================


% ====================== \/ GEN_SERVER CALLBACKS ===============================

%% @doc Initializes the server's state.
%%      Registers a process named `estatsda_manager`.
%%      Sets up an event manager and all configured adapters which will forward
%%      to some graphing system or log, or whatever you want your adapter to do.
init([]) ->
  % Time between each flush, default to 10 seconds.
  FlushInterval = estatsd:env_or_default(flush_interval, 10000),
  error_logger:info_msg("[~s] Flushing every ~wms", [?MODULE, FlushInterval]),

  % Adapters to handle the collected metrics. Defaults to logging only.
  DefaultAdapter = {estatsda_logger, []},
  Adapters = estatsd:env_or_default(adapters, [DefaultAdapter]),
  setup_adapters_(Adapters),

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


%% @doc Increments or creates the counter.
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


%% @doc Drops requests with invalid sample rates.
handle_cast({increment, _, _, _}, State) -> {noreply, State};


%% @doc Inserts or updates the given timing.
handle_cast({timing, Key, Duration}, State) ->
  Timers = State#state.timers,
  case gb_trees:lookup(Key, Timers) of
    % Initialize a new timer
    none ->
      {noreply, State#state{
        timers = gb_trees:insert(Key, [Duration], Timers)
      }};

    % Otherwise just append the duration
    {value, Val} ->
      {noreply, State#state{
        timers = gb_trees:update(Key, [Duration|Val], Timers)
      }}
  end;


%% @doc Flushes the current set of metrics.
handle_cast(flush, State) ->
  % Retrieve the metrics from the state and the env
  Counters = ets:tab2list(statsd),
  Timers = gb_trees:to_list(State#state.timers),

  % Handle the flush in another process
  spawn(fun() -> flush_metrics_(Counters, Timers) end),

  % Continue with a blank slate
  ets:delete_all_objects(statsd),
  NewState = State#state{timers = gb_trees:empty()},
  {noreply, NewState}.


%% @doc Logs and drops all calls.
handle_call(Request, From, State) ->
  error_logger:warning_msg(
    "[~s] Ignored call '~p' from '~p'~n", [?MODULE, Request, From]),
  {reply, ok, State}.


%% @doc Logs and drops all info messages.
handle_info(Info, State) ->
  error_logger:info_msg("[~s] Ignored info '~p'~n", [?MODULE, Info]),
  {noreply, State}.


%% @doc Returns the old state.
code_change(_OldVsn, State, _Extra) -> {noreply, State}.


%% @doc Does nothing.
terminate(_Arg, _State) -> ok.

% ====================== /\ GEN_SERVER CALLBACKS ===============================


% ====================== \/ HELPERS ============================================

%% @doc Flushes the given metrics to registered adapters.
flush_metrics_(Counters, Timers) ->
  case length(Counters) + length(Timers) of
    0 -> emptyset;  % Do nothing if no metrics were collected
    _ -> gen_event:notify(estatsda_manager, {handle, {Counters, Timers}})
  end.


%% @doc Setup the metrics manager and its handlers, aka adapters.
-spec setup_adapters_([{Module::atom(), InitArgs::term()}]) -> no_return().
setup_adapters_(Adapters) ->
  % Start the metrics event manager
  gen_event:start_link({local, estatsda_manager}),

  % Register the specified adapters with the manager
  lists:foreach(
    fun(AdapterSpec) ->
      {AdapterModule, _} = AdapterSpec,
      % Every adapter implements the estatsda_handler behaviour and is also
      % invoked through an instance of estatsda_handler, which implements the
      % generic parts of each adapter.
      gen_event:add_handler(estatsda_manager, estatsda_adapter, AdapterSpec),
      error_logger:info_msg("[~s] Added adapter: '~p'~n",
        [?MODULE, AdapterModule])
    end,
    Adapters
  ).

% ====================== /\ HELPERS ============================================
