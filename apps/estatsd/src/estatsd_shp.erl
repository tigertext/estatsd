-module(estatsd_shp).

-export([parse_packet/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SHP_VERSION, 1).

-type shp_metric_type() :: 'm' | 'mr' | 'g' | 'h'.

-record(shp_metric, {key         :: binary(),
                     value       :: integer(),
                     type        :: shp_metric_type(),
                     sample_rate :: float() | undefined}).

-spec parse_packet(binary()) ->
        {bad_version, binary()}
        | {bad_length, {integer(), binary()}}
        | {?SHP_VERSION, non_neg_integer(),
           [#shp_metric{}|{bad_metric, term()}]}.

% @doc Parse a binary in Stats Hero Protocol Version 1
%
parse_packet(<<"1|", Rest/binary>>) ->
    parse_packet(Rest, []);
parse_packet(Packet) when is_binary(Packet) ->
    {bad_version, Packet}.

parse_packet(<<"\n", Rest/binary>>, Acc) ->
    Acc1 = lists:reverse(Acc),
    Length = try
                 list_to_integer(Acc1)
             catch
                 error:badarg ->
                     Acc1
             end,
    Actual = size(Rest) + 1, % to account for the consumed \n
    case Length =:= Actual of
        true ->
            parse_body({Length, Rest});
        false ->
            {bad_length, {Length, Rest}}
    end;
parse_packet(<<C:8, Rest/binary>>, Acc) ->
    parse_packet(Rest, [C|Acc]);
parse_packet(<<>>, Acc) ->
    {bad_length, {lists:reverse(Acc), <<>>}}.

-spec parse_body({non_neg_integer(), binary()}) ->
    [(#shp_metric{} | {bad_metric, term()})].

parse_body({Length, GZBody = <<31, 139, _Rest/binary>>}) ->
    Body = zlib:gunzip(GZBody),
    parse_body({Length, Body});
parse_body({_Length, Body}) ->
    try
        Lines = binary:split(Body, <<"\n">>, [global]),
        [ parse_metric(L) || L <- Lines, L =/= <<>> ]
    catch
        error:Why ->
            error_logger:error_report({bad_metric,
                                       {Body, Why, erlang:get_stacktrace()}}),
            throw({bad_metric_body, Body})
    
    end.

-spec parse_metric(binary()) -> #shp_metric{}.

parse_metric(Bin) ->
    try
        [Key, Value, Type | Rate] = binary:split(Bin, [<<":">>, <<"|">>],
                                                 [global]),
        #shp_metric{key = Key, value = to_int(Value), type = parse_type(Type),
                    sample_rate = parse_sample_rate(Rate)}
    catch
        throw:{bad_metric, Why} ->
            {bad_metric, Why};
        error:{badmatch, _} ->
            {bad_metric, {parse_error, Bin}}
    end.

-spec parse_type(binary()) -> atom().

parse_type(<<"m">>) ->
    m;
parse_type(<<"h">>) ->
    h;
parse_type(<<"mr">>) ->
    mr;
parse_type(<<"g">>) ->
    g;
parse_type(Unknown) ->
    throw({bad_metric, {unknown_type, Unknown}}).

-spec parse_sample_rate([binary()]) -> float().

parse_sample_rate([]) ->
    undefined;
parse_sample_rate([<<"@", FloatBin/binary>>]) ->
    try
        list_to_float(binary_to_list(FloatBin))
    catch
        error:badarg ->
            throw({bad_metric, {bad_sample_rate, FloatBin}})
    end;
parse_sample_rate(L) ->
    throw({bad_metric, {bad_sample_rate, L}}).

-spec to_int(binary()) -> integer().

to_int(Value) when is_binary(Value) ->
    try
        list_to_integer(binary_to_list(Value))
    catch
        error:badarg ->
            throw({bad_metric, {bad_value, Value}})
    end.

estatsd_shp_test_() ->
    {foreach,
     fun() ->
             setup
     end,
     fun(_X) ->
             cleanup
     end,
     [
      {"parse_packet valid packet",
       fun() ->
               Packet = <<"1|12\na_label:1|m">>,
               ?assertEqual([#shp_metric{key = <<"a_label">>,
                                       value = 1,
                                       type = m,
                                        sample_rate = undefined}],
                            parse_packet(Packet))
       end
      },

      {"parse_packet bad version",
       fun() ->
               BadVersions = [<<"2|12\na_label:1|m">>,
                              <<"212\na_label:1|m">>,
                              <<"x|12\na_label:1|m">>,
                              <<>>],
               [ ?assertEqual({bad_version, P}, parse_packet(P))
                 || P <- BadVersions ]
       end
      },

      {"parse_packet content length mismatch",
       generator,
       fun() ->
               BadLength = [{<<"1|11\na_label:1|m">>,
                             {11, <<"a_label:1|m">>}},
                             
                            {<<"1|12\nx:1|m">>,
                             {12, <<"x:1|m">>}},
                            
                            {<<"1|11\na_label:1|m">>,
                            {11, <<"a_label:1|m">>}}
                           ],
               [ ?_assertEqual({bad_length, {L, R}}, parse_packet(P))
                 || {P, {L, R}} <- BadLength ]
       end
      },

      {"parse_packet invalid content length",
       generator,
       fun() ->
               Packets = [{<<"1|1.0\nlabel:1|m">>,
                           {"1.0", <<"label:1|m">>}},

                          {<<"1|abc\nlabel:1|m">>,
                           {"abc", <<"label:1|m">>}},

                          {<<"1|label:1|m">>,
                           {"label:1|m", <<>>}}
                          
                         ],

               [ ?_assertEqual({bad_length, {L, R}}, parse_packet(P))
                 || {P, {L, R}} <- Packets ]
       end
      },

      {"parse_metric valid metric tests",
       generator,
       fun() ->
               Tests = [{<<"label:1|m">>,
                         #shp_metric{key = <<"label">>, value = 1, type = 'm'}},

                        {<<"label:123|h">>,
                         #shp_metric{key = <<"label">>, value = 123,
                                     type = 'h'}},

                        {<<"x:-123|g">>,
                         #shp_metric{key = <<"x">>, value = -123, type = 'g'}},


                        {<<"x:123|h">>,
                         #shp_metric{key = <<"x">>, value = 123, type = 'h'}},

                        % sample rate
                        {<<"x:123|h|@0.43">>,
                         #shp_metric{key = <<"x">>, value = 123, type = 'h',
                                     sample_rate = 0.43}}
                       ],
               [ ?_assertEqual(Expect, parse_metric(In)) ||
                  {In, Expect} <- Tests ]
       end
      },

      {"gzip compressed body",
       fun() ->
               Body = <<"a_label:1|m">>,
               GZBody = zlib:gzip(Body),
               % we add one for the '\n'.  Not sure the \n should be
               % included in the length.
               BodySize = integer_to_list(size(GZBody) + 1),
               Packet = iolist_to_binary(["1|", BodySize, "\n",
                                          GZBody]),
               ?assertEqual([#shp_metric{key = <<"a_label">>,
                                         value = 1,
                                         type = m,
                                         sample_rate = undefined}],
                            parse_packet(Packet))
       end
               
      },

      {"parse_metric bad metrics",
       generator,
       fun() ->
               Tests = [
                        % bad type
                        {<<"x:1|q">>, {bad_metric, {unknown_type, <<"q">>}}},
                        % bad value
                        {<<"x:1.0|m">>, {bad_metric, {bad_value, <<"1.0">>}}},
                        % bad parse
                        {<<"x:10m">>, {bad_metric, {parse_error, <<"x:10m">>}}},
                        {<<"x:1|m|a|b">>, {bad_metric,
                                           {bad_sample_rate, [<<"a">>, <<"b">>]}}}
                       ],
               [ ?_assertEqual(Expect, parse_metric(Line)) ||
                   {Line, Expect} <- Tests ]
       end
      },

      {"parse_metric bad sample rate",
       generator,
       fun() ->
               EatAt = fun([$@|S]) ->
                               list_to_binary(S)
                       end,
               SampleRates = ["@0.a", "@01", "@5", "@0.1x"],
               Expects = [ {bad_metric, {bad_sample_rate, EatAt(S)}}
                           || S <- SampleRates ],
               Tests = [ {iolist_to_binary([<<"x:1|m|">>, S]), E}
                         || {S, E} <- lists:zip(SampleRates, Expects) ],
               [ ?_assertEqual(Expect, parse_metric(Line)) ||
                   {Line, Expect} <- Tests ]
       end
      }
      
     ]}.
             

% TODO:
% - put record def into header
