%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Session Database.

-module(bf_session).

%% api
-export([new/0,
         delete/1,
         find/1,
         store/1,
         key/1,
         timestamp/1,
         touch/1,
         store_value/3,
         find_value/2,
         erase_value/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(bucket, <<"session">>).

-type session_key() :: binary().
-record(session, {key=bf_uuid:v4(), version=1, timestamp=erlang:now(),
        data=orddict:new()}).


%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

%% @doc Create a session.
-spec new() -> Session::#session{}.
new() ->
    #session{}.

%% @doc Delete a session.
-spec delete(SessionKey::session_key()) -> ok.
delete(SessionKey) ->
    riakc_pool:delete(?bucket, SessionKey),
    ok.

%% @doc Find and load a session.
-spec find(SessionKey::session_key()) -> Session::#session{} | {error, Reason::term()}.
find(SessionKey) ->
    case riakc_pool:get(?bucket, SessionKey) of
        {error, Reason} -> {error, Reason};
        Value -> binary_to_term(Value)
    end.

%% @doc Save a session
-spec store(Session::#session{}) -> ok | {error, Reason::term()}.
store(Session) ->
    riakc_pool:put(?bucket, Session#session.key, term_to_binary(Session)).

%% @doc Session key attribute.
-spec key(Session::#session{}) -> SessionKey::session_key().
key(Session) ->
    Session#session.key.

%% @doc Session timestamp attribute.
-spec timestamp(Session::#session{}) -> {integer(), integer(), integer()}.
timestamp(Session) ->
    Session#session.timestamp.

%% @doc Set the timestamp attribute of a session to the current time.
-spec touch(Session::#session{}) -> Session::#session{}.
touch(Session) ->
    Session#session{timestamp=erlang:now()}.

%% @doc Store a value associated with the session.
-spec store_value(Session::#session{}, Key::term(), Value::term()) -> Session::#session{}.
store_value(Session, Key, Value) ->
    Data = orddict:store(Key, Value, Session#session.data),
    Session#session{data=Data}.

%% @doc Find a value associated with the session.
-spec find_value(Session::#session{}, Key::term()) -> {ok, Value::term()} | error.
find_value(Session, Key) ->
    orddict:find(Key, Session#session.data).

%% @doc Erase a value associated with the session.
-spec erase_value(Session::#session{}, Key::term()) -> Session::#session{}.
erase_value(Session, Key) ->
    Data = orddict:erase(Key, Session#session.data),
    Session#session{data=Data}.


%% ------------------------------------------------------------------
%% unit tests
%% ------------------------------------------------------------------

-ifdef(TEST).

%% @doc Ensure an application has started.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

session_test_() ->
    {setup,
        fun() -> ensure_started(riakc_pool), ok end,
        [fun crud/0, fun update_values/0]}.

crud() ->
    Session = new(),
    ?assertEqual(ok, store(Session)),
    ?assertEqual(Session, find(Session#session.key)),
    ?assertEqual(ok, delete(Session#session.key)),
    ?assertMatch({error, _}, find(Session#session.key)).

update_values() ->
    Session = new(),
    ?assertEqual(ok, store(Session)),
    ?assertEqual(error, find_value(Session, dog)),
    Session2 = store_value(Session, dog, eats),
    ?assertEqual({ok, eats}, find_value(Session2, dog)),
    ?assertEqual(ok, store(Session2)),
    ?assertMatch(Session2, find(Session2#session.key)),
    Session3 = erase_value(Session2, dog),
    ?assertEqual(error, find_value(Session3, dog)),
    ?assertEqual(ok, store(Session2)),
    ?assertMatch(Session2, find(Session2#session.key)),
    ?assertEqual(ok, delete(Session#session.key)),
    ?assertMatch({error, _}, find(Session#session.key)).

-endif.
