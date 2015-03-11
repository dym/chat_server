-module(chat_server_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

-define(TCP_OPTIONS,[binary, {packet, line}, {active, once}, {reuseaddr, true}]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn_link(fun empty_listeners/0),
    Procs = [
             {chat_server_worker, {chat_server_worker, start_link, [ListenSocket]},
              temporary, 1000, worker, [chat_server_worker]}
            ],
    {ok, {{simple_one_for_one, 60, 3600}, Procs}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
