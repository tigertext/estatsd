% These tests expect a working estatsd application listing on UDP 3344.
% 
-module(estatsd_udp_test).

-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

basic() ->
    Port = 3344,
    {ok, S} = gen_udp:open(0),
    ok = gen_udp:send(S, "127.0.0.1", Port, <<"mycounter:10|c">>),
    ok = gen_udp:send(S, "127.0.0.1", Port, <<"mycounter:10|c">>),
    ok = gen_udp:send(S, "127.0.0.1", Port, <<"mycounter:5|d">>),
    Metrics = folsom_metrics:get_metrics(),
    ?assert(lists:member(<<"mycounter">>, Metrics)),
    MyCounter = folsom_metrics:get_metric_value(<<"mycounter">>),
    ?debugVal(MyCounter),
    ok.

multi() ->
    Port = 3344,
    {ok, S} = gen_udp:open(0),
    Self = self(),
    SendUdp = fun() ->
                      Msg = <<"multicounter:10|c">>,
                      ok = gen_udp:send(S, "127.0.0.1", Port, Msg),
                      Self ! self(),
                      ok
              end,
    Pids = [ spawn(SendUdp) || _I <- lists:seq(1, 10) ],
    gather_pids(Pids),
    Metrics = folsom_metrics:get_metrics(),
    ?assertEqual(lists:usort(Metrics), Metrics),
    ?debugVal(Metrics),
    ?assert(lists:member(<<"multicounter">>, Metrics)),
    MultiCounter = folsom_metrics:get_metric_value(<<"multicounter">>),
    ?debugVal(MultiCounter),
    ok.

mass(N) ->
    Port = 3344,
    {ok, S} = gen_udp:open(0),
    Self = self(),
    SendUdp = fun() ->
                      Id = integer_to_list(crypto:rand_uniform(1, 300)),
                      Msg = iolist_to_binary([<<"metric_">>, Id, <<":10|c">>]),
                      ok = gen_udp:send(S, "127.0.0.1", Port, Msg),
                      Self ! self(),
                      ok
              end,
    Pids = [ spawn(SendUdp) || _I <- lists:seq(1, N) ],
    gather_pids(Pids),
    Metrics = folsom_metrics:get_metrics(),
    ?assertEqual(lists:usort(Metrics), lists:sort(Metrics)),
    ?debugVal(Metrics),
    ok.
    

gather_pids([Pid|Rest]) ->
    receive
        Pid ->
            gather_pids(Rest)
    after 2000 ->
            gather_pids(Rest)
    end;
gather_pids([]) ->
    done.

    
            
    

    
