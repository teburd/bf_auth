%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Time functions.

-module(bf_time).

-export([timestamp/0]).


%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

%% @doc Generated a simple integer timestamp representing
%% seconds from year 0 to the current universal time.
%% @end
-spec timestamp() -> integer().
timestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
