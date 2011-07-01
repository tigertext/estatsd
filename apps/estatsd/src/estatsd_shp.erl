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
            {Length, Rest};
        false ->
            {bad_length, {Length, Rest}}
    end;
parse_packet(<<C:8, Rest/binary>>, Acc) ->
    parse_packet(Rest, [C|Acc]);
parse_packet(<<>>, Acc) ->
    {bad_length, {lists:reverse(Acc), <<>>}}.
    
parse_header(Packet) ->
    [Head|Lines] = binary:split(Packet, <<"\n">>, [global]),
    % verify we are at version 1
    [?SHP_VERSION, Len] = [ ?to_int(X) || X <- binary:split(Head, <<"|">>) ],
    {{?SHP_VERSION, Len}, Lines}.

estatsd_shp_test_() ->
    {foreach,
     fun() ->
             setup
     end,
     fun(_X) ->
             cleanup
     end,
     [
      {"parse_valid header",
       fun() ->
               Packet = <<"1|26\na_label:1|m">>,
               {{Version, Len}, Lines} = parse_header(Packet),
               ?assertEqual(?SHP_VERSION, Version),
               ?assertEqual(26, Len)
       end},
      {"parse_packet valid packet",
       fun() ->
               Packet = <<"1|12\na_label:1|m">>,
               ?assertEqual({12, <<"a_label:1|m">>}, parse_packet(Packet))
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
       fun() ->
               BadLength = [{<<"1|11\na_label:1|m">>,
                             {11, <<"a_label:1|m">>}},
                             
                            {<<"1|12\nx:1|m">>,
                             {12, <<"x:1|m">>}},
                            
                            {<<"1|11\na_label:1|m">>,
                            {11, <<"a_label:1|m">>}}
                           ],
               [ ?assertEqual({bad_length, {L, R}}, parse_packet(P))
                 || {P, {L, R}} <- BadLength ]
       end
      },

      {"parse_packet invalid content length",
       fun() ->
               Packets = [{<<"1|1.0\nlabel:1|m">>,
                           {"1.0", <<"label:1|m">>}},

                          {<<"1|abc\nlabel:1|m">>,
                           {"abc", <<"label:1|m">>}},

                          {<<"1|label:1|m">>,
                           {"label:1|m", <<>>}}
                          
                         ],

               [ ?assertEqual({bad_length, {L, R}}, parse_packet(P))
                 || {P, {L, R}} <- Packets ]
       end
      }
     ]}.
             

