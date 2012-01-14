%% @author Richard Jones <rj@metabrew.com>
%% @author Johannes Huning <hi@johanneshuning.com>
%% @copyright 2011 Richard Jones
%% @doc Global type and record definitions.

%% A key is either an atom, a binary or a list/string.
-type key() :: atom() | binary() | list().

%% A counter consists of a naming key and a tuple of it's total value and
%% the number of increments.
-type counter() ::
  {key(), {Value::non_neg_integer(), NoIncrements::non_neg_integer()}}.
-type counters() :: [counter()].

%% A duration is an integer >= 0. Unit is ms.
-type duration() :: non_neg_integer().

%% A timer consists of a naming key and contains a list of measured durations.
-type timer() :: {key(), [duration()]}.
-type timers() :: [timer()].

%% A set of metrics is tuple of counters and timers.
-type metrics() :: {counters(), timers()}.


% TODO: Document!
-type shp_metric_type() :: 'm' | 'mr' | 'g' | 'h'.

% TODO: Document!
-record (shp_metric, {
  key         :: binary(),
  value       :: integer(),
  type        :: shp_metric_type(),
  sample_rate :: float() | undefined
}).
