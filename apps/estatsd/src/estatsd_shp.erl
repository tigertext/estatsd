-module(estatsd_shp).

-export([parse_packet/1]).

-include_lib("eunit/include/eunit.hrl").

-define(to_int(Value), list_to_integer(binary_to_list(Value))).
-define(SHP_VERSION, 1).

% -type shp_metric_type() :: (<<"m">> | <<"mr">> | <<"g">> | <<"h">>).
-type shp_metric_type() :: 'm' | 'mr' | 'g' | 'h'.

-record(shp_metric, {key :: binary(),
                     value :: integer(),
                     type :: shp_metric_type(),
                     sample_rate :: float()}).

% 1|26\nmyWebservice.requests:1|m

-spec parse_packet(binary()) ->
        {bad_version, binary()}
        | {bad_length, {integer(), binary()}}
        | {?SHP_VERSION, non_neg_integer(), [#shp_metric{}]}.
                                     
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

parse_body({Length, GZBody = <<31, 139, _Rest/binary>>}) ->
    Body = zlib:gunzip(GZBody),
    parse_body({Length, Body});
parse_body({_Length, Body}) ->
    try
        Lines = binary:split(Body, <<"\n">>, [global]),
        [ parse_line(L) || L <- Lines, L =/= <<>> ]
    catch
        error:Why ->
            error_logger:error_report({bad_metric,
                                       {Body, Why, erlang:get_stacktrace()}}),
            throw({bad_metric_body, Body})
    
    end.

parse_line(Bin) ->
    [Key, Value, Type | Rate] = binary:split(Bin, [<<":">>, <<"|">>], [global]),
    #shp_metric{key = Key, value = ?to_int(Value), type = parse_type(Type),
               sample_rate = parse_sample_rate(Rate)}.

parse_type(<<"m">>) ->
    m;
parse_type(<<"h">>) ->
    h;
parse_type(<<"mr">>) ->
    mr;
parse_type(<<"g">>) ->
    g.

parse_sample_rate([]) ->
    undefined;
parse_sample_rate([<<"@", FloatBin/binary>>]) ->
    list_to_float(binary_to_list(FloatBin)).

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

      {"parse_line valid metric tests",
       generator,
       fun() ->
               Tests = [{<<"label:1|m">>, #shp_metric{key = <<"label">>,
                                                      value = 1,
                                                      type = 'm'}},

                        {<<"label:123|h">>, #shp_metric{key = <<"label">>,
                                                        value = 123,
                                                        type = 'h'}},

                        {<<"x:-123|g">>, #shp_metric{key = <<"x">>,
                                                        value = -123,
                                                        type = 'g'}},


                        {<<"x:123|h">>, #shp_metric{key = <<"x">>,
                                                        value = 123,
                                                        type = 'h'}},

                        % sample rate
                        {<<"x:123|h|@0.43">>, #shp_metric{key = <<"x">>,
                                                          value = 123,
                                                          type = 'h',
                                                          sample_rate = 0.43}}
                        
                       ],
               [ ?_assertEqual(Expect, parse_line(In)) ||
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

      {"parse_line invalid metric tests",
       generator,
       fun() ->
               % TODO
               []
       end
      }
     ]}.
             

