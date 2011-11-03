%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc User Database.

-module(bf_user).

%% api
-export([new/3,
         delete/2,
         find/2,
         store/2,
         email/1,
         name/1,
         timestamp/1,
         authenticate/2,
         set_password/2
         ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("riakc/include/riakc_obj.hrl").

-define(bucket, <<"users">>).
-define(rounds, 10).

-type email() :: binary().
-record(user, {robj, attrs}).

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

%% @doc Create a user.
-spec new(Email::email(), Name::binary(), Password::binary()) -> User::#user{}.
new(Email, Name, Password) ->
    PWHash = hashpw(Password),
    TStamp =  bf_time:timestamp(),
    RiakObj = riakc_obj:new(?bucket, Email),
    Attrs = orddict:from_list([{<<"email">>, Email}, {<<"name">>, Name},
            {<<"timestamp">>, TStamp}, {<<"password">>, PWHash}]),
    #user{robj=RiakObj, attrs=Attrs}.

%% @doc Delete a user.
-spec delete(Db::any(), Email::email()) -> ok.
delete(Db, Email) ->
    riakc_pb_socket:delete(Db, ?bucket, Email).

%% @doc Find and load a user.
-spec find(Db::any(), Email::email()) -> User::#user{} | {error, Reason::term()}.
find(Db, Email) ->
    case riakc_pb_socket:get(Db, ?bucket, Email) of
        {error, Reason} -> {error, Reason};
        {ok, RiakObj} ->
            {Attrs} = jiffy:decode(riakc_obj:get_value(RiakObj)),
            #user{robj=RiakObj, attrs=Attrs}
    end.

%% @doc Save a user
-spec store(Db::any(), User::#user{}) -> User::#user{} | {error, Reason::term()}.
store(Db, User) ->
    Email = email(User),
    Value = jiffy:encode({User#user.attrs}),
    RiakObj = riakc_obj:update_value(User#user.robj, Value, <<"application/json">>),
    case riakc_pb_socket:put(Db, RiakObj) of
        ok ->
            User#user{robj=RiakObj};
        {ok, Email} ->
            User#user{robj=RiakObj};
        {ok, RiakObj2} ->
            User#user{robj=RiakObj2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc User email attribute.
-spec email(User::#user{}) -> Email::email().
email(User) ->
    orddict:fetch(<<"email">>, User#user.attrs).

%% @doc User name attribute.
-spec name(User::#user{}) -> Name::binary().
name(User) ->
    orddict:fetch(<<"name">>, User#user.attrs).

%% @doc User timestamp attribute. Gregorian seconds since year 0 to UTC time.
-spec timestamp(User::#user{}) -> integer().
timestamp(User) ->
    orddict:fetch(<<"timestamp">>, User#user.attrs).

%% @doc Authenticate a password with a users password.
-spec authenticate(User::#user{}, Password::binary()) -> true | false.
authenticate(User, Password) ->
    Hash = orddict:fetch(<<"password">>, User#user.attrs),
    verifypw(Password, Hash).

%% @doc Set password attribute.
-spec set_password(User::#user{}, Password::binary()) -> User::#user{}.
set_password(User, Password) ->
    Hash = hashpw(Password),
    Attrs = orddict:store(<<"password">>, Hash, User#user.attrs),
    User#user{attrs=Attrs}.


%% ------------------------------------------------------------------
%% private api
%% ------------------------------------------------------------------

hashpw(Password) ->
    PasswordStr = binary_to_list(Password),
    Hash = bcrypt:hashpw(PasswordStr, bcrypt:gen_salt(?rounds)),
    list_to_binary(Hash).

verifypw(Password, Hash) ->
    PasswordStr = binary_to_list(Password),
    HashStr = binary_to_list(Hash),
    HashStr =:= bcrypt:hashpw(PasswordStr, HashStr).


%% ------------------------------------------------------------------
%% unit tests
%% ------------------------------------------------------------------

-ifdef(TEST).

-define(db, bf_user_db).

user_test_() ->
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
    {timeout, 60, fun authentication/0}].

crud(Db) ->
    Email = <<"bogus@bogus.com">>,
    Name = <<"Bogus">>,
    Password = <<"bogus123">>,
    User = new(Email, Name, Password),
    ?assertEqual(Email, email(User)),
    ?assertEqual(Name, name(User)),
    TStamp = timestamp(User),
    ?assert(is_integer(TStamp)),
    ?assertMatch({error, _}, find(Db, Email)),
    User2 = store(Db, User),
    ?assertEqual(email(User), email(User2)),
    User3 = find(Db, Email),
    ?assertEqual(User3#user.attrs, User2#user.attrs),
    ?assertEqual(ok, delete(Db, Email)),
    ?assertMatch({error, _}, find(Db, Email)).

authentication() ->
    User = new(<<"bogus@bogus.com">>, <<"Bogus">>, <<"bogus123">>),
    ?assertEqual(true, authenticate(User, <<"bogus123">>)),
    ?assertEqual(false, authenticate(User, <<"bogus321">>)),
    User1 = set_password(User, <<"topnotch">>),
    ?assertEqual(false, authenticate(User1, <<"bogus123">>)),
    ?assertEqual(true, authenticate(User1, <<"topnotch">>)),
    ?debugTime("bcrypt hashpw time", bcrypt:hashpw("test", bcrypt:gen_salt(?rounds))).

-endif.
