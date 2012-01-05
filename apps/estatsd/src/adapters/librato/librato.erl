-module (librato).

% Client API
-export ([
  create_metrics/5
]).


%% @doc See http://dev.librato.com/v1/post/metrics
create_metrics(User, Token, Source, Contents, MeasureTime) ->
  Url = "https://metrics-api.librato.com/v1/metrics.json",
  Headers = [auth_header_(User, Token), {"Accept", "application/json"}],
  ContentType = "application/json",

  Request = {Url, Headers, ContentType, Contents},
  HTTPOptions = [],
  Options = [],

  error_logger:info_msg("[~s] Sending metrics to Librato ...~n", [?MODULE]),
  case httpc:request(post, Request, HTTPOptions, Options) of
    {error, Reason} ->
      error_logger:error_msg("[~s] Failed to send metrics to Librato: '~p'~n",
        [?MODULE, Reason]);
    _ -> ok
  end.


%% @doc
auth_header_(User, Token) ->
  Encoded = base64:encode_to_string(User ++ ":" ++ Token),
  {"Authorization", "Basic " ++ Encoded}.
