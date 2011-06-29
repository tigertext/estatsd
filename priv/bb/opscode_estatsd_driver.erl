-module(opscode_estatsd_driver).

-export([new/1,
         run/4]).

-record(state, {host,
                port,
                sock}).

new(_Id) ->
    Host = basho_bench_config:get(estatsd_host),
    Port = basho_bench_config:get(estatsd_port),
    {ok, Sock} = gen_udp:open(0, [binary, {active, false}]),
    {ok, #state{host=Host, port=Port, sock=Sock}}.

run(_Op, MetricGen, _ValueGen, #state{sock=Sock, host=Host, port=Port}=State) ->
    ok = gen_udp:send(Sock, Host, Port, MetricGen()),
    {ok, State}.
