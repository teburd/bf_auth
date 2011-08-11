%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc User Database.

-module(bf_user).

%% api
-export([new/4,
         delete/1,
         find/1,
         store/1,
         email/1,
         name/1,
         timestamp/1,
         authenticate/2,
         set_password/2,
         generate_token/1,
         token/1,
         validate/2,
         valid/1
         ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(bucket, <<"user">>).
-define(rounds, 10).

-type email() :: binary().
-record(user, {email, version=1, timestamp=erlang:now(), password=undefined,
    enabled=true, validated=false, token, token_timeout, firstname, lastname}).


%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

%% @doc Create a user.
-spec new(Email::email(), Password::binary(),
    Firstname::binary(), Lastname::binary()) -> User::#user{}.
new(Email, Password, Firstname, Lastname) ->
    User0 = #user{email=Email, firstname=Firstname, lastname=Lastname},
    User1 = set_password(User0,Password),
    generate_token(User1).

%% @doc Delete a user.
-spec delete(Email::email()) -> ok.
delete(Email) ->
    bf_riakc:delete(?bucket, Email),
    ok.

%% @doc Find and load a user.
-spec find(Email::email()) -> User::#user{} | {error, Reason::term()}.
find(Email) ->
    case bf_riakc:get(?bucket, Email) of
        {error, Reason} -> {error, Reason};
        Value -> binary_to_term(Value)
    end.

%% @doc Save a user
-spec store(User::#user{}) -> ok | {error, Reason::term()}.
store(User) ->
    bf_riakc:put(?bucket, User#user.email, term_to_binary(User)).

%% @doc User email attribute.
-spec email(User::#user{}) -> Email::email().
email(User) ->
    User#user.email.

%% @doc User name attribute.
-spec name(User::#user{}) -> Name::binary().
name(User) ->
    Firstname = User#user.firstname,
    Lastname = User#user.lastname,
    <<Firstname/binary, " ", Lastname/binary>>.

%% @doc User timestamp attribute.
-spec timestamp(User::#user{}) -> {integer(), integer(), integer()}.
timestamp(User) ->
    User#user.timestamp.

%% @doc Authenticate a password with a users password.
-spec authenticate(User::#user{}, Password::binary()) -> true | false.
authenticate(User, Password) ->
    Hash = binary_to_list(User#user.password),
    PasswordLst = binary_to_list(Password),
    Hash =:= bcrypt:hashpw(PasswordLst, Hash).

%% @doc Set password attribute.
-spec set_password(User::#user{}, Password::binary()) -> User::#user{}.
set_password(User, Password) ->
    PasswordLst = binary_to_list(Password),
    Hash = bcrypt:hashpw(PasswordLst, bcrypt:gen_salt(?rounds)),
    User#user{password=list_to_binary(Hash)}.

%% @doc Generate a validation token attribute and set it.
-spec generate_token(User::#user{}) -> User::#user{}.
generate_token(User) ->
    User#user{token=crypto:rand_bytes(16), token_timeout=add_days(30, erlang:now())}.

%% @doc Validation token attribute.
-spec token(User::#user{}) -> Token::binary().
token(User) ->
    User#user.token.

%% @doc Mark user as validated given a validation token.
-spec validate(User::#user{}, Token::binary()) -> User::#user{} |
    {error, expired_token} | {error, invalid_token}.
validate(User, Token) ->
    case timer:now_diff(erlang:now(), User#user.token_timeout) > 0 of
        true ->
            {error, expired_token};
        false ->
            case User#user.token == Token of
                true ->
                    User#user{validated=true};
                false ->
                    {error, invalid_token}
            end
    end.

%% @doc Validity check, checks if a user account can be used. 
-spec valid(User::#user{}) -> true | false.
valid(User) ->
    User#user.validated and User#user.enabled.


%% ------------------------------------------------------------------
%% private api
%% ------------------------------------------------------------------

%% @doc Offset a timestamp by a number of days.
-spec add_days(pos_integer(), {integer(), integer(), integer()}) ->
    {integer(), integer(), integer()}.
add_days(Days, {MegaSecs, Secs, MicroSecs}) ->
    AddTime = Days*24*60*60,
    AddMega = AddTime/1000000,
    AddSecs = AddTime - AddMega,
    MegaSecs2 = MegaSecs + AddMega,
    Secs2 = Secs + AddSecs,
    {MegaSecs2, Secs2, MicroSecs}.


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


%% @doc Offset a timestamp negatively by a number of days, used for testing.
-spec subtract_days(pos_integer(), {integer(), integer(), integer()}) ->
    {integer(), integer(), integer()}.
subtract_days(Days, {MegaSecs, Secs, MicroSecs}) ->
    AddTime = Days*24*60*60,
    AddMega = AddTime/1000000,
    AddSecs = AddTime - AddMega,
    MegaSecs2 = MegaSecs - AddMega,
    Secs2 = Secs - AddSecs,
    {MegaSecs2, Secs2, MicroSecs}.


user_test_() ->
    {setup,
        fun() -> ensure_started(bf_riakc), ok end,
        [fun crud/0, {timeout, 60, fun authentication/0}, fun validation/0]}.

crud() ->
    Email = <<"bogus@bogus.com">>,
    User = new(Email, <<"bogus123">>, <<"Bogus">>,
        <<"Bogus">>),
    ?assertEqual(Email, email(User)),
    ?assertMatch({_, _, _}, timestamp(User)),
    ?assertMatch({error, _}, find(User#user.email)),
    ?assertEqual(ok, store(User)),
    ?assertEqual(User, find(User#user.email)),
    ?assertEqual(ok, delete(User#user.email)),
    ?assertMatch({error, _}, find(User#user.email)).

authentication() ->
    User = new(<<"bogus@bogus.com">>, <<"bogus123">>, <<"Bogus">>,
        <<"Bogus">>),
    ?assertEqual(true, authenticate(User, <<"bogus123">>)),
    ?assertEqual(false, authenticate(User, <<"bogus321">>)),
    User1 = set_password(User, <<"topnotch">>),
    ?assertEqual(false, authenticate(User1, <<"bogus123">>)),
    ?assertEqual(true, authenticate(User1, <<"topnotch">>)),
    ?debugTime("bcrypt hashpw time", bcrypt:hashpw("test", bcrypt:gen_salt(?rounds))).

validation() ->
    User0 = new(<<"bogus@bogus.com">>, <<"bogus123">>, <<"Bogus">>,
        <<"Bogus">>),
    ?assertEqual(false, valid(User0)),
    ?assertEqual({error, invalid_token}, validate(User0, <<"bogus">>)),
    User1 = User0#user{token_timeout=subtract_days(1, erlang:now())},
    ?assertEqual({error, expired_token}, validate(User1, <<"bogus">>)),
    Token = token(User0),
    User2 = validate(User0, Token),
    ?assertEqual(true, valid(User2)).

-endif.
