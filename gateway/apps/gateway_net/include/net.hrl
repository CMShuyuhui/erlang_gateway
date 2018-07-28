%%%-------------------------------------------------------------------
%%% @author cms_huyuhui
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 七月 2018 下午3:28
%%%-------------------------------------------------------------------
-author("cms_huyuhui").

-define(LOW_WATERMARK, 65536).
-define(HIGH_WATERMARK, 131072).

-define(SOCKET_ERROR, [
  eagain, %% Resource temporarily unavailable
  ealready, %% Operation already in progress
  ebusy, %% File busy
  econnreset, %% Connection reset by peer
  einprogress, %% Operation now in progress
  emfile %% Too many open files reducing accept rate: out of file descriptors
]).

-record(ssl_socket, {tcp, ssl}).
-define(IS_SSL(Sock), is_record((Sock), ssl_socket)).

