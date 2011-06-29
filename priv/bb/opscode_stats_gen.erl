-module(opscode_stats_gen).

-define(TYPES, [<<"c">>, <<"ms">>, <<"e">>]).
-define(TYPE_COUNT, length(?TYPES)).

-export([new_stat/1]).

new_stat(_) ->
    fun() ->
            Name = random_name(),
            Type = get_metric_type(Name),
            Value = new_value(Type),
            list_to_binary([Name, "|", Value, "|", Type]) end.

new_value(Type) when Type =:= <<"d">>;
                     Type =:= <<"c">> ->
    integer_to_list(crypto:rand_uniform(1, 25));
new_value(<<"ms">>) ->
    integer_to_list(crypto:rand_uniform(1, 5000));
new_value(<<"e">>) ->
    integer_to_list(crypto:rand_uniform(1, 500)).

random_name() ->
    io_lib:format("metric~4..0B", [crypto:rand_uniform(1, 1000)]).

select_type() ->
    lists:nth(crypto:rand_uniform(1, ?TYPE_COUNT), ?TYPES).

get_metric_type(Name) ->
    create_table(),
    case ets:lookup(opscode_stats, Name) of
        [] ->
            Type = select_type(),
            ets:insert_new(opscode_stats, {Name, Type}),
            Type;
        [{Name, Type}] ->
            Type
    end.

create_table() ->
    case catch ets:new(opscode_stats, [named_table, public]) of
        opscode_stats ->
            ok;
        %% Get this if the table is already created
        {'EXIT', {badarg, _Stacktrace}} ->
            ok
    end.
