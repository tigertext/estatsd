-module(opscode_stats_gen).

-define(TYPES, [<<"c">>, <<"ms">>, <<"e">>]).
-define(TYPE_COUNT, length(?TYPES)).

-define(SHP_TYPES, [<<"h">>, <<"m">>, <<"mr">>, <<"g">>]).
-define(SHP_TYPE_COUNT, length(?SHP_TYPES)).

-export([new_stat/1]).

new_stat(_) ->
    case crypto:rand_uniform(0, 2) > 0 of
        true ->
            fun() -> shp_stat() end;
        false ->
            fun() -> legacy_stat() end
    end.

shp_stat() ->
    Name = random_shp_name(),
    Type = get_shp_metric_type(Name),
    Value = new_value(Type),
    Body = iolist_to_binary([Name, ":", Value, "|", Type]),
    Size = integer_to_list(size(Body) + 1),
    iolist_to_binary(["1|", Size, "\n", Body]).

legacy_stat() ->
    Name = random_name(),
    Type = get_metric_type(Name),
    Value = new_value(Type),
    list_to_binary([Name, "|", Value, "|", Type]).
    
new_value(Type) when Type =:= <<"d">>;
                     Type =:= <<"c">>;
                     Type =:= <<"h">>;
                     Type =:= <<"mr">> ->
    integer_to_list(crypto:rand_uniform(1, 25));
new_value(Type) when Type =:= <<"ms">>;
                     Type =:= <<"m">> ->
    integer_to_list(crypto:rand_uniform(1, 5000));
new_value(<<"e">>) ->
    integer_to_list(crypto:rand_uniform(1, 500)).

% random_name/0 and random_shp_name/0 determine how many unique
% metrics will be tracked in the system during the benchmark run.

random_name() ->
    io_lib:format("metric~4..0B", [crypto:rand_uniform(1, 1000)]).

random_shp_name() ->
    io_lib:format("shp_metric~4..0B", [crypto:rand_uniform(1, 1000)]).

select_type() ->
    lists:nth(crypto:rand_uniform(1, ?TYPE_COUNT), ?TYPES).

select_shp_type() ->
    lists:nth(crypto:rand_uniform(1, ?SHP_TYPE_COUNT), ?SHP_TYPES).

get_metric_type(Name) ->
    create_table(opscode_stats),
    case ets:lookup(opscode_stats, Name) of
        [] ->
            Type = select_type(),
            ets:insert_new(opscode_stats, {Name, Type}),
            Type;
        [{Name, Type}] ->
            Type
    end.

get_shp_metric_type(Name) ->
    create_table(opscode_shp_stats),
    case ets:lookup(opscode_shp_stats, Name) of
        [] ->
            Type = select_shp_type(),
            ets:insert_new(opscode_shp_stats, {Name, Type}),
            Type;
        [{Name, Type}] ->
            Type
    end.

create_table(Table) ->
    case catch ets:new(Table, [named_table, public]) of
        Table ->
            ok;
        %% Get this if the table is already created
        {'EXIT', {badarg, _Stacktrace}} ->
            ok
    end.
