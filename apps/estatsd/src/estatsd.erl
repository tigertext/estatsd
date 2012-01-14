%% @author Richard Jones <rj@metabrew.com>
%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2011 Richard Jones
%% @doc The EStatsD module including a a client API and helper functions.
-module (estatsd).

%% Application control.
-export ([
  start/0,
  stop/0
]).

%% Helper functions.
-export ([
  key2str/1,
  num2str/1,
  unixtime/0,
  env_or_default/2
]).

%% Client API for metrics in this VM.
-export ([
  increment/1,
  increment/2,
  increment/3,
  decrement/1,
  decrement/2,
  decrement/3,
  timing/2
]).


% ====================== \/ HELPER FUNCTIONS  ==================================

%% @doc Attempts to read the given key from the environment,
%%      returns the default otherwise.
-spec env_or_default(Key::atom(), Default::term()) -> term().
env_or_default(Key, Default) ->
  case application:get_env(ogrex, Key) of
    {ok, Value} -> Value;
    _ -> Default
  end.


%% @doc Renders the given key's string representation.
-spec key2str(Key :: atom() | binary() | string()) -> string().

key2str(Key) when is_atom(Key) -> atom_to_list(Key);

key2str(Key) when is_binary(Key) -> key2str(binary_to_list(Key));

key2str(Key) when is_list(Key) ->
  {ok, R1} = re:compile("\\s+"),
  {ok, R2} = re:compile("/"),
  {ok, R3} = re:compile("[^a-zA-Z_\\-0-9\\.]"),

  Opts = [global, {return, list}],

  S1 = re:replace(Key,  R1, "_", Opts),
  S2 = re:replace(S1, R2, "-", Opts),
  S3 = re:replace(S2, R3, "", Opts),
  S3.


% @doc Renders the given number's string representation.
num2str(Number) -> lists:flatten(io_lib:format("~w", [Number])).


% @doc Retrieves the current UNIX time.
unixtime() ->
  {Megasec, Sec, _MicroSec} = erlang:now(),
  Megasec * 1000000 + Sec.


% ====================== /\ HELPER FUNCTIONS  ==================================


% ====================== \/ APPLICATION CONTROL  ===============================

%% @doc Starts the estatsd server.
start() -> application:start(estatsd).


%% @doc Stops the estatsd server.
stop() -> application:stop(estatsd).

% ====================== /\ APPLICATION CONTROL  ===============================


% ====================== \/ METRICS CLIENT API  ================================

-define (SERVER, estatsd_server).

%% @doc Measure timing information given in ms.
timing(Key, Duration) when is_integer(Duration) ->
  gen_server:cast(?SERVER, {timing, Key, Duration});


%% @doc Measure timing information.
timing(Key, Duration) ->
  gen_server:cast(?SERVER, {timing, Key, erlang:round(Duration)}).


%% @doc Alias for increment(Key, 1, 1).
increment(Key) -> increment(Key, 1, 1).

%% @doc Alias for increment(Key, Amount, 1).
increment(Key, Amount) -> increment(Key, Amount, 1).

%% @doc Increments the named counter.
increment(Key, Amount, Sample) ->
  gen_server:cast(?SERVER, {increment, Key, Amount, Sample}).


%% @doc Alias for decrement(Key, -1, 1).
decrement(Key) -> decrement(Key, -1, 1).

%% @doc Alias for decrement(Key, Amount, 1).
decrement(Key, Amount) -> decrement(Key, Amount, 1).

%% @doc Decrements the named counter.
decrement(Key, Amount, Sample) -> increment(Key, 0 - Amount, Sample).

% ====================== /\ METRICS CLIENT API  ================================
