-module (librato).

% Client API
-export ([
  create_metrics/5
]).


%% @doc See http://dev.librato.com/v1/post/metrics
create_metrics(User, Token, Source, Contents, MeasureTime) ->
  undefined.
