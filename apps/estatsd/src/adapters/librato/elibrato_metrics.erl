-module (elibrato_metrics).

% Client API
-export ([
  get_metrics/4,
  create_metrics/4
]).


%% @doc See http://dev.librato.com/v1/get/metrics
get_metrics(Client, Tags, Name, PaginationParams) ->
  undefined.


%% @doc See http://dev.librato.com/v1/post/metrics
create_metrics(Client, Source, Contents, MeasureTime) ->
  undefined.
