-module (elibrato_gauge).

% Client API
-export ([
  create_gauge/6,
  create_gauge/9,
  get_gauge/5,
  get_gauges/4,
  update_gauge/6,
  delete_gauge/2
]).


%% @doc See http://dev.librato.com/v1/post/gauges
create_gauge(
    Client, Name, DisplayUnits, Description, Attributes, DisplayMin) ->
  undefined.


%% @doc See
create_gauge(
    Client, Value, Source, MeasureTime, Count, Sum, Max, Min, SumSquares) ->
  undefined.


%% @doc See http://dev.librato.com/v1/get/gauges/:name
get_gauge(Client, Source, SourceTag, SummarizeTime, SummarizeSources) ->
  undefined.


%% @doc See http://dev.librato.com/v1/get/gauges
get_gauges(Client, Tag, Name, PaginationParams) ->
  undefined.


%% @doc See http://dev.librato.com/v1/put/gauges/:name
update_gauge(
    Client, Name, DisplayMin, Description, DisplayUnits, Attributes) ->
  undefined.


%% @doc See http://dev.librato.com/v1/delete/gauges/:name
delete_gauge(Client, Name) ->
  undefined.
