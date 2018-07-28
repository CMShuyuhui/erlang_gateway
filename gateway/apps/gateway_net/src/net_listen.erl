%%%-------------------------------------------------------------------
%%% @author cms_huyuhui
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 七月 2018 上午11:52
%%%-------------------------------------------------------------------
-module(net_listen).
-author("cms_huyuhui").

-behavior(gen_server).

%% API
-export([start_link/1]).
-export([accept_ack/1]).
-export([rejected_ack/2]).
-export([get_listen/1]).
-export([listen_close/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% include
-include("net.hrl").
-include_lib("gateway_comm/include/log.hrl").

-type socket_setopt() :: gen_sctp:option() | gen_tcp:option() | gen_udp:option().

-record(state, {
  ip :: inet:ip_address(), %% net listen ip
  listen_fd :: port(), %% listen socket fd
  port :: inet:port_number(), %% net listen port
  listen_opts :: socket_setopt(), %% listen options
  sup_names, %% supervisor names
  mod_sup, %% supervisor module
  nth_sup, %% when have more than one sup_name, the index of sup_name
  max_sup, %% max number of sup_names
  require_size = 0, %% max acceptor pool in config
  active_size = 0, %% current acceptor number
  child_opts = [] %% options of child pid
}).

-define(SERVER, ?MODULE).

%%%=======================================================================
%%% gen_server callbacks
%%%=======================================================================
init([Name, Ip, Port, ListenOpts, SupMod, AcceptorPoolSize, ChildOpts]) ->
  erlang:process_flag(trap_exit, true),
  ?INFO("Listen: ~p, Ip: (~p:~p), size: ~p, mod: ~p",
    [Name, Ip, Port, AcceptorPoolSize, SupMod]),
  {ok, Socket} = listen(Port, ListenOpts),
  SupNames = SupMod:names(),
  State = #state{
    ip = Ip,
    listen_fd = Socket,
    port = Port,
    listen_opts = ListenOpts,
    sup_names = SupNames,
    require_size = AcceptorPoolSize,
    active_size = 0,
    max_sup = erlang:length(SupNames),
    child_opts = ChildOpts
  },
  {ok, new_acceptors(State)}.

handle_call(get_listen, _From, State) ->
  {reply, State#state.listen_fd, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({listen_closed, OldSock},
  State = #state{listen_fd = ListenFd, port = Port, listen_opts = ListenOpts}) ->
  case ListenFd of
    OldSock ->
      net_socket:close(OldSock),
      {ok, NewListenSock} = listen(Port, ListenOpts),
      {noreply, State#state{listen_fd = NewListenSock}};
    _ ->
      ?INFO("listen socket: ~p have restart.", [OldSock]),
      {noreply, State}
  end ;

handle_cast({accepted, _Pid}, State = #state{active_size = ActiveSize}) ->
  {noreply, new_acceptors(State#state{active_size = ActiveSize - 1 })};

handle_cast({rejected, Reason, Time}, State = #state{active_size = ActiveSize}) ->
  ?WARN("accept/2 rejected by ~p after ~p ms, ActivePool: ~p",
    [Reason, Time, ActiveSize]),
  erlang:send_after(Time, self(), {rejected, Reason}),
  {noreply, State#state{active_size = ActiveSize - 1}};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({rejected, Reason}, State) ->
  ErrInfo = inet:format_error(Reason),
  case lists:member(Reason, ?SOCKET_ERROR) of
    true ->
      ?WARN("accept/2 error by ~p(~p) start new one: ~p",
        [Reason, ErrInfo, State]),
      {noreply, new_acceptors(State)};
    false ->
      ?ERROR("accept/2 error by ~p(~p) avtive pool size: ~p, state: ~p",
        [Reason, ErrInfo, State#state.active_size, State]),
      {noreply, State}
  end ;

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVer, State, _Extra) ->
  {ok, State}.

%%%=======================================================================
%%% API
%%%=======================================================================

start_link([Name | _] = Opt) ->
  gen_server:start_link({local, Name}, ?MODULE, Opt, []).

accept_ack(Pid) ->
  gen_server:cast(Pid, {accepted, self()}).

rejected_ack(Pid, Reason) ->
  Time = erlang:phash2(self(), 100) + 11, %% time range from 10ms to 110ms
  gen_server:cast(Pid, {rejected, Reason, Time}).

get_listen(Listener) ->
  gen_server:call(Listener, get_listen).

listen_close(OldListener, Listener) ->
  gen_server:cast(Listener, {listen_closed, OldListener}).


%%%=======================================================================
%%% Internal functions
%%%=======================================================================
listen(Port, Opts) ->
  case net_socket:listen(false, Port, Opts, []) of
    {ok, Socket} ->
      {ok, Socket};
    Reason ->
      ?ERROR("listen port: ~p failed, reason: ~p", [Port, Reason]),
      {init:stop, Port, Reason}
  end .

new_acceptors(State = #state{
  active_size = ActiveSize,
  require_size = RequireSize,
  max_sup = MaxSup,
  nth_sup = Nth,
  sup_names = SupNames,
  listen_fd = ListenFd,
  child_opts = ChildOpts
  }) when ActiveSize < RequireSize->

  case SupNames of
    [SupName] ->
      {ok, _Pid} = supervisor:start_child(SupName, [[self(), ListenFd, ChildOpts]]),
      new_acceptors(State#state{active_size = ActiveSize + 1});
    _ ->
      {SupName, NewNth} =
        case MaxSup >= Nth of
          true ->
            {lists:nth(Nth, SupNames), Nth + 1};
          false ->
            {lists:nth(1, SupNames), 2}
        end,
      {ok, _Pid} = supervisor:start_child(SupName, [[self(), ListenFd, ChildOpts]]),
      new_acceptors(State#state{active_size = ActiveSize + 1, nth_sup = NewNth})
  end;

new_acceptors(State) ->
  State.

