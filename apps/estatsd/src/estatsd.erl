-module (estatsd).

% Application Control
-export ([
  start/0,
  stop/0
]).

% Client API
-export ([
  increment/1,
  increment/2,
  increment/3,
  decrement/1,
  decrement/2,
  decrement/3,
  timing/2
]).

-define (SERVER, estatsd_server).

%% @spec start() -> ok
%% @doc Start the estatsd server.
start() -> application:start(estatsd).


%% @spec stop() -> ok
%% @doc Stop the estatsd server.
stop() -> application:stop(estatsd).


%% @doc Log timing information given in ms.
timing(Key, Duration) when is_integer(Duration) -> 
  gen_server:cast(?SERVER, {timing, Key, Duration});


%% @doc Log timing information.
timing(Key, Duration) -> 
  gen_server:cast(?SERVER, {timing, Key, erlang:round(Duration)}).


%% @doc Alias for increment(Key, 1, 1).
increment(Key) -> increment(Key, 1, 1).

%% @doc Alias for increment(Key, Amount, 1).
increment(Key, Amount) -> increment(Key, Amount, 1).

%% @doc Increments the given stat.
increment(Key, Amount, Sample) ->
  gen_server:cast(?SERVER, {increment, Key, Amount, Sample}).


%% @doc Alias for decrement(Key, -1, 1).
decrement(Key) -> decrement(Key, -1, 1).

%% @doc Alias for decrement(Key, Amount, 1).
decrement(Key, Amount) -> decrement(Key, Amount, 1).

%% @doc Decrements the given stat by negatively incrementing.
decrement(Key, Amount, Sample) -> increment(Key, 0 - Amount, Sample).
