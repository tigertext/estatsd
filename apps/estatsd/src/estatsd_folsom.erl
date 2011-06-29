-module(estatsd_folsom).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(ALL_METRICS, all_metrics_keys).
-include_lib("folsom/include/folsom.hrl").

%-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, ensure_metric/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {blacklist = [] :: [re:mp()]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ensure_metric(Key, Type) ->
    {Table, Fun} = table_for_type(Type),
    gen_server:call(?SERVER, {ensure_metric, Key, Table, Fun}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    {ok, Blacklist} = application:get_env(estatsd, folsom_blacklist),
    ets:new(?ALL_METRICS, [set, named_table, public]),
    {ok, #state{blacklist = build_blacklist_regex(Blacklist)}}.

handle_call({ensure_metric, Key, Table, Fun}, _From,
            State = #state{blacklist = Blacklist}) ->
    Blacklisted = is_blacklisted(Key, Blacklist),
    Exists = ets:member(?ALL_METRICS, {Key, Table}),
    Status = create_maybe(Blacklisted, Exists, Key, Table, Fun),
    {reply, {ok, Status}, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

table_for_type(Type) when Type =:= <<"d">> orelse Type =:= <<"i">> ->
    {?COUNTER_TABLE, new_counter};
table_for_type(Type) when Type =:= <<"ms">> orelse Type =:= <<"h">> ->
    {?HISTOGRAM_TABLE, new_histogram};
table_for_type(<<"e">>) ->
    {?HISTORY_TABLE, new_history};
table_for_type(<<"g">>) ->
    {?GAUGE_TABLE, new_gauge};
table_for_type(Type) when Type =:= <<"m">> orelse Type =:= <<"c">> ->
    % we treat our current "c" as a meter because it is essentially
    % request count so treating as a meter will give us req/s data.
    {?METER_TABLE, new_meter};
table_for_type(<<"mr">>) ->
    {?METER_READER_TABLE, new_meter_reader};
table_for_type(BadType) ->
    erlang:error({error, {bad_type, BadType}}).


create_maybe(true, _, _Key, _Table, _Fun) ->
    blacklisted;
create_maybe(false, true, _Key, _Table, _Fun) ->
    {ok, already_exists};
create_maybe(false, false, Key, Table, Fun) ->
    folsom_metrics:Fun(Key),
    ets:insert(?ALL_METRICS, {{Key, Table}, true}),
    error_logger:info_msg("created|~p|~p~n", [{Key, Table}, Fun]),
    {ok, created}.

build_blacklist_regex(List) ->
    lists:map(fun(P) ->
                      {ok, RE} = re:compile(P),
                      RE
              end, List).

is_blacklisted(Key, [RE|Rest]) ->
    case re:run(Key, RE) of
        {match, _} -> true;
        nomatch -> is_blacklisted(Key, Rest)
    end;
is_blacklisted(_Key, []) ->
    false.
