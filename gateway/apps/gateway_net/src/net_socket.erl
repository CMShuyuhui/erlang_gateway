%%%-------------------------------------------------------------------
%%% @author cms_huyuhui
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 七月 2018 下午3:26
%%%-------------------------------------------------------------------
-module(net_socket).
-author("cms_huyuhui").

%% API
-export([listen/4]).
-export([send/2]).
-export([close/1]).
-export([peername/1]).
-export([sockname/1]).
-export([getstat/2]).
-export([getopts/2]).
-export([setopts/2]).
-export([set_active_true/1]).
-export([set_active_false/1]).
-export([set_active_once/1]).
-export([set_active_n/2]).
-export([set_delay_send/2]).
-export([listen_option/1]).
-export([listen_option/2]).
-export([parse_port/1]).

%% include
-include("net.hrl").
-include_lib("gateway_comm/include/log.hrl").

%%%=======================================================================
%%% API
%%%=======================================================================
listen(_Ssl, Port, Opts, _SslOpts) ->
  gen_tcp:listen(Port, Opts).

send(undefined, _Data) ->
  ok;

send(Socket, Data) when ?IS_SSL(Socket) ->
  catch ssl:send(Socket#ssl_socket.ssl, Data);

send(Socket, Data) ->
  catch erlang:port_command(Socket, Data, [force]).


close(undefined) ->
  ok;

close(Socket) when ?IS_SSL(Socket) ->
  catch ssl:close(Socket#ssl_socket.ssl);

close(Socket) ->
  catch gen_tcp:close(Socket).


peername(Socket) when ?IS_SSL(Socket) ->
  ssl:peername(Socket#ssl_socket.ssl);

peername(Socket) ->
  inet:peername(Socket).


sockname(Socket) when ?IS_SSL(Socket) ->
  ssl:sockname(Socket#ssl_socket.ssl);

sockname(Socket) ->
  inet:sockname(Socket).

getstat(Socket, Opts) when ?IS_SSL(Socket) ->
  ssl:getstat(Socket#ssl_socket.ssl, Opts);

getstat(Socket, Opts) ->
  inet:getstat(Socket, Opts).

getopts(Socket, Opts) when ?IS_SSL(Socket) ->
  ssl:getopts(Socket#ssl_socket.ssl, Opts);

getopts(Socket, Opts) ->
  inet:getopts(Socket, Opts).

setopts(Socket, Opts) when ?IS_SSL(Socket) ->
  ssl:setopts(Socket#ssl_socket.ssl, Opts);

setopts(Socket, Opts) ->
  inet:setopts(Socket, Opts).

set_active_true(Socket) ->
  catch setopts(Socket, [{active, true}]).

set_active_false(Socket) ->
  catch setopts(Socket, [{active, false}]).

set_active_once(Socket) ->
  catch setopts(Socket, [{active, once}]).

set_active_n(Socket, ActiveNum) ->
  catch setopts(Socket, [{active, ActiveNum}]).

set_delay_send(Socket, Flag) when is_boolean(Flag) ->
  catch setopts(Socket, [{delay_send, Flag}]).

listen_option(Ip) ->
  listen_option(Ip, []).

listen_option({127, 0, 0, 1}, Opt) ->
  listen_option_inner(Opt);

listen_option(undefined, Opt) ->
  listen_option_inner(Opt);

listen_option(Ip, Opt) ->
  Ip = case is_tuple(Ip) of
         true ->
           Ip;
         _ ->
           {ok, Tmp} = inet:parse_address(Ip),
           Tmp
       end,
  [{ip, Ip} | listen_option_inner(Opt)].

listen_option_inner(Opt) ->
  DefaultOpt = [
    inet,
    binary,
    {reuseaddr, true},
    {backlog, 32768},
    {active, true},
    {packet, 4},
    {keepalive, true},
    {nodelay, true},
    {delay_send, false},
    {low_watermark, ?LOW_WATERMARK},
    {high_watermark, ?HIGH_WATERMARK}
  ],

  MergeFun =
    fun(Item, Acc) ->
      case Item of
        {Key, _Val} ->
          case lists:keymember(Key, 1, Acc) of
            false ->
              [Item|Acc];
            true ->
              lists:keyreplace(Key, 1, Acc, Item)
          end;
        _ when is_atom(Item) ->
          case lists:member(Item, Acc) of
            true ->
              Acc;
            false ->
              [Item|Acc]
          end
      end
    end,
  lists:foldl(MergeFun, DefaultOpt, Opt).

parse_port(Port) when is_list(Port) ->
  list_to_integer(Port);

parse_port(Port) when is_integer(Port) andalso Port > 0 ->
  Port.




