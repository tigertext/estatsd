-module (estatsd_adapter).

-export ([
  behaviour_info/1
]).


%% @doc Behaviour definition
behaviour_info(callbacks) -> [
  {init, 1},
  {handle_metrics, 2},
  {sync_handle_metrics, 2}
];

behaviour_info(_) -> undefined.
