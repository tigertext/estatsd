-module (elibrato_counter).

% Client API
-export ([
  create_counter/4,
  create_counter/5,
  get_counter/2,
  get_counters/4,
  update_counter/4,
  delete_counter/2
]).


%% @doc See http://dev.librato.com/v1/post/counters
create_counter(Client, Name, Description, Attributes) ->
  undefined.


%% @doc See http://dev.librato.com/v1/post/counters/:name
create_counter(Client, Name, Value, Source, MeasureTime) ->
  undefined.


%% @doc http://dev.librato.com/v1/get/counters/:name
get_counter(Client, Name) ->
 undefined.


%% @doc See http://dev.librato.com/v1/get/counters
get_counters(Client, Tag, Name, PaginationParams) ->
  undefined.


%% @doc See http://dev.librato.com/v1/put/counters/:name
update_counter(Client, Name, Description, Attributes) ->
  undefined.


%% @doc See http://dev.librato.com/v1/delete/counters/:name
delete_counter(Client, Name) ->
  undefined.
