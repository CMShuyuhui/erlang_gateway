%%%-------------------------------------------------------------------
%%% @author cms_huyuhui
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 七月 2018 下午3:23
%%%-------------------------------------------------------------------
-author("cms_huyuhui").


-ifndef(UAP_LOG).
-define(UAP_LOG, true).
-compile([{parse_transform, lager_transform}]).

-define(DEBUG(Str), lager:debug(Str)).
-define(DEBUG(Str, Args), lager:debug(Str, Args)).

-define(INFO(Str), lager:info(Str)).
-define(INFO(Str, Args), lager:info(Str, Args)).

-define(WARN(Str), lager:warning(Str)).
-define(WARN(Str, Args), lager:warning(Str, Args)).

-define(NOTICE(Str), lager:notice(Str)).
-define(NOTICE(Str, Args), lager:notice(Str, Args)).

-define(ERROR(Str), lager:error(Str)).
-define(ERROR(Str, Args), lager:error(Str, Args)).

-endif.
