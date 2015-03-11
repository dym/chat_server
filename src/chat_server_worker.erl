%%%-------------------------------------------------------------------
%%% File    : chat_server_worker.erl
%%% Author  : Dmitriy Budashny <dmitriy.budashny@gmail.com>
%%% Description : Chat Server Worker
%%%
%%% Created : 11 Mar 2015 by Dmitriy Budashny <dmitriy.budashny@gmail.com>
%%%-------------------------------------------------------------------
-module(chat_server_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("chat.hrl").

-record(state, {name = <<"anonymous">>,
                socket = undefined}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ListenSocket) ->
    gen_server:start_link(?MODULE, ListenSocket, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(ListenSocket) ->
    gen_server:cast(self(), {accept, ListenSocket}),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({handle_msg, Msg, Username}, State) ->
    NewMsg = <<Username/binary, ": ", Msg/binary>>,
    gproc:send({p, l, ?MODULE}, {broadcast_msg, NewMsg}),
    {noreply, State};
handle_cast({accept, ListenSocket}, State) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    chat_server_sup:start_socket(),
    gproc:reg({p, l, ?MODULE}, self()),
    send(Socket, ?MOTD),
    send(Socket, ?CMD_HELP),
    {noreply, State#state{socket=Socket}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({broadcast_msg, Msg}, #state{socket=Socket} = State) ->
    case Socket of
        undefined -> ok;
        _ -> send(Socket, Msg)
    end,
    {noreply, State};
handle_info({tcp, Socket, <<"/nick ", NewName/binary>>}, State) ->
    NewName1 = re:replace(NewName, "(^\\s+)|(\\s+$)", "", [global, {return, binary}]),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{name=NewName1}};
handle_info({tcp, Socket, <<"/quit", _/binary>>}, State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp, Socket, Msg}, #state{name=Username} = State) ->
    gen_server:cast(self(), {handle_msg, Msg, Username}),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send(Socket, Msg) ->
    ok = gen_tcp:send(Socket, Msg),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.
