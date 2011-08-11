%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Session Database.

-module(bf_session).

%% api
-export([new/0,
         delete/2,
         find/2,
         store/2,
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
-spec delete(Db::atom(), SessionKey::session_key()) -> ok.
delete(Db, SessionKey) ->
    bf_riakc:delete(Db, ?bucket, SessionKey),
    ok.

%% @doc Find and load a session.
-spec find(Db::atom(), SessionKey::session_key()) -> Session::#session{} | {error, Reason::term()}.
find(Db, SessionKey) ->
    case bf_riakc:get(Db, ?bucket, SessionKey) of
        {error, Reason} -> {error, Reason};
        Value -> binary_to_term(Value)
    end.

%% @doc Save a session
-spec store(Db::atom(), Session::#session{}) -> ok | {error, Reason::term()}.
store(Db, Session) ->
    bf_riakc:put(Db, ?bucket, Session#session.key, term_to_binary(Session)).

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

-define(db, bf_session_db).

session_test_() ->
    {setup,
        fun() -> bf_riakc:start_link(?db, 8, "localhost", 8081), ok end,
        fun(_) -> bf_riakc:stop(?db), ok end,
        [fun crud/0, fun update_values/0]}.

crud() ->
    Session = new(),
    ?assertEqual(ok, store(?db, Session)),
    ?assertEqual(Session, find(?db, Session#session.key)),
    ?assertEqual(ok, delete(?db, Session#session.key)),
    ?assertMatch({error, _}, find(?db, Session#session.key)).

update_values() ->
    Session = new(),
    ?assertEqual(ok, store(?db, Session)),
    ?assertEqual(error, find_value(Session, dog)),
    Session2 = store_value(Session, dog, eats),
    ?assertEqual({ok, eats}, find_value(Session2, dog)),
    ?assertEqual(ok, store(?db, Session2)),
    ?assertMatch(Session2, find(?db, Session2#session.key)),
    Session3 = erase_value(Session2, dog),
    ?assertEqual(error, find_value(Session3, dog)),
    ?assertEqual(ok, store(?db, Session2)),
    ?assertMatch(Session2, find(?db, Session2#session.key)),
    ?assertEqual(ok, delete(?db, Session#session.key)),
    ?assertMatch({error, _}, find(?db, Session#session.key)).

-endif.
