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
         set_user/2,
         user/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(bucket, <<"sessions">>).

-type session_key() :: binary().
-type session_user() :: bf_user:email().
-record(session, {robj, attrs}).


%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

%% @doc Create a session.
-spec new() -> Session::#session{}.
new() ->
    Key = bf_uuid:to_formatted(bf_uuid:v4()),
    RiakObj = riakc_obj:new(?bucket, Key),
    Attrs = orddict:from_list([{<<"key">>, Key},
            {<<"timestamp">>, bf_time:timestamp()},
            {<<"user">>, <<"anonymous">>}]),
    #session{robj=RiakObj, attrs=Attrs}.

%% @doc Delete a session.
-spec delete(Db::any(), SessionKey::session_key()) -> ok.
delete(Db, SessionKey) ->
    riakc_pb_socket:delete(Db, ?bucket, SessionKey).

%% @doc Find and load a session.
-spec find(Db::any(), SessionKey::session_key()) -> Session::#session{} | {error, Reason::term()}.
find(Db, SessionKey) ->
    case riakc_pb_socket:get(Db, ?bucket, SessionKey) of
        {error, Reason} -> {error, Reason};
        {ok, RiakObj} ->
            {Attrs} = jiffy:decode(riakc_obj:get_value(RiakObj)),
            #session{robj=RiakObj, attrs=Attrs}
    end.

%% @doc Save a session
-spec store(Db::any(), Session::#session{}) -> ok | {error, Reason::term()}.
store(Db, Session) ->
    Key = key(Session),
    Value = jiffy:encode({Session#session.attrs}),
    RiakObj = riakc_obj:update_value(Session#session.robj, Value),
    case riakc_pb_socket:put(Db, RiakObj) of
        ok ->
            Session#session{robj=RiakObj};
        {ok, Key} ->
            Session#session{robj=RiakObj};
        {ok, RiakObj2} ->
            Session#session{robj=RiakObj2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Session key attribute.
-spec key(Session::#session{}) -> SessionKey::session_key().
key(Session) ->
    orddict:fetch(<<"key">>, Session#session.attrs).

%% @doc Session timestamp attribute.
-spec timestamp(Session::#session{}) -> {integer(), integer(), integer()}.
timestamp(Session) ->
    orddict:fetch(<<"timestamp">>, Session#session.attrs).

%% @doc Set the timestamp attribute of a session to the current time.
-spec touch(Session::#session{}) -> Session::#session{}.
touch(Session) ->
    TStamp = bf_time:timestamp(),
    Attrs = orddict:store(<<"timestamp">>, TStamp, Session#session.attrs),
    Session#session{attrs=Attrs}.

%% @doc Set the current user attribute of a session.
-spec set_user(Session::#session{}, User::session_user()) -> #session{}.
set_user(Session, User) ->
    Attrs = orddict:store(<<"user">>, User, Session#session.attrs),
    Session#session{attrs=Attrs}.

%% @doc Get the current user attribute of a session.
-spec user(Session::#session{}) -> User::session_user().
user(Session) ->
    orddict:fetch(<<"user">>, Session#session.attrs).

%% ------------------------------------------------------------------
%% unit tests
%% ------------------------------------------------------------------

-ifdef(TEST).

-define(db, bf_session_db).

session_test_() ->
    [{setup,
        fun() ->
            {ok, Db} = riakc_pb_socket:start_link("localhost", 8081),
            pong = riakc_pb_socket:ping(Db),
            Db
        end,
        fun(Db) ->
            riakc_pb_socket:stop(Db),
            ok
        end,
        {with, [fun crud/1]}},
     fun attributes/0].

crud(Db) ->
    Session = new(),
    Key = key(Session),
    Session2 = store(Db, Session),
    ?assertEqual(Key, key(Session2)),
    ?assertEqual(key(Session2), key(find(Db, Key))),
    ?assertEqual(ok, delete(Db, Key)),
    ?assertMatch({error, _}, find(Db, Key)).

attributes() ->
    Session = new(),
    TStamp = timestamp(Session),
    ?assert(is_integer(TStamp)),
    ?assertEqual(<<"anonymous">>, user(Session)),
    User = <<"test@test.com">>,
    Session2 = set_user(Session, User),
    ?assertEqual(User, user(Session2)).

-endif.
