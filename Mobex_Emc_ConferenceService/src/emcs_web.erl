%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for emcs.

-module(emcs_web).
-author("Mochi Media <dev@mochimedia.com>").

-include("emcs.hrl").

-export([start/1, stop/0, loop/2,pre_loop/1]).

%% MySQL Configuration
-define(MYSQL_SERVER, "192.168.1.143").
-define(MYSQL_USER, "root").
-define(MYSQL_PASSWD, "password").
-define(MYSQL_DB, "emc").
-define(MYSQL_PORT, 3306).

-record(sessions, {id, uid, status,flag,mid}).


%% External API

start(Options) ->
	emcs_controller:start(),
	{{host,Host},{username,Username},{password,Password}, {dbname, Dbname}}=emcs_config:get_config(database),
    application:start(emysql),
    emysql:add_pool(myjqrealtime, 1, Username, Password, Host, ?MYSQL_PORT, Dbname, utf8),
	
    tcp_server:start(?MODULE, 9000, {?MODULE, pre_loop}).

stop() ->
    ok.


pre_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_|Nick]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            io:format("Nick: ~p~n", [Nick]),
            case Command of
                "CONNECT" ->
                    try_connection(clean(Nick), Socket);		
                _ ->
                    gen_tcp:send(Socket, "Unknown command!\n"),
                    ok
            end;
        {error, closed} ->
            ok
    end.

try_connection(Nick, Socket) ->
    Response = gen_server:call(emcs_controller, {connect, Nick, Socket}),
    case Response of
        {ok} ->
            gen_tcp:send(Socket, "CONNECT:OK\n"),
            %%gen_server:cast(emcs_controller, {join, Nick}),
            %%gen_tcp:send(Socket, "{uid,Nick}"++Nick++"\n"),
            loop(Nick, Socket);
        nick_in_use ->
            gen_tcp:send(Socket, "CONNECT:ERROR:Nick in use.\n"),
            ok
    end.

%% Check session util

%% Check session util
check_session(Socket,Uid) ->
	%gen_tcp:send(Socket, "check_session(Socket,Uid)"++Uid++"\n"),
    %% Check session
    CheckSession = emysql:execute(myjqrealtime, 
        lists:concat([
            "SELECT * FROM emc_meeting_user_log WHERE flag=1 and uid = ", 
            emysql_util:quote(getclean(Uid)),
            " order by id desc LIMIT 1"
        ]
    )),

    %% Convert to records
    Records = emysql_util:as_record(CheckSession, sessions, record_info(fields, sessions)),

    %% Check existence & return user_id if possible
    if
        length(Records) == 1 ->
            %% Get UserId of element
            [{_, Id,_, _,_, _}] = [Rec || Rec <- Records],
            {integer_to_list(Id)};
        true ->
            false
    end.

check_have_new_conference(Uid) ->
    %% Check session
    CheckSession = emysql:execute(myjqrealtime, 
        lists:concat([
            "SELECT * FROM emc_meeting_user_log WHERE status=0 and flag=0 and uid = ", 
            emysql_util:quote(getclean(Uid)),
            " LIMIT 1"
        ]
    )),

    %% Convert to records
    Records = emysql_util:as_record(CheckSession, sessions, record_info(fields, sessions)),

    %% Check existence & return true if possible
    if
        length(Records) == 1 ->
            true;
        true ->
            false
    end.


loop(Uid, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            case Command of
                "SAY" ->
					feed(Uid, Socket);
                   % say(Uid, Socket, clean(Content));
                "PVT" ->
                    {ReceiverNick, [_|Msg]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Content),
                    private_message(Uid, Socket, ReceiverNick, clean(Msg));
                "QUIT" ->
                    quit(Uid, Socket)
            end;
        {error, closed} ->
            ok
        end.


feed(Uid, Socket)->
	try	
  					receive
					after 1000->
							%gen_tcp:send(Socket, "feed(Nick, Socket)"++Uid++"\n"),
							case check_session(Socket,Uid) of
								{Rid} ->
									case check_have_new_conference(Uid) of
									true->
										Result = emysql:execute(myjqrealtime,
																lists:concat([
																			  "SELECT mid,status FROM emc_meeting_user_log WHERE flag=0 and  uid = ",
																			  emysql_util:quote(getclean(Uid)),
																			  " and id>",
																			  emysql_util:quote(getNumber(Rid))
																			 ]
																			)),
										Records = emysql_util:as_record(Result, sessions, record_info(fields, sessions)),
                                            if
												length(Records) > 0 ->
													JSON = emysql_util:as_json(Result),
													Myjson = mochijson2:encode({struct,[{<<"isNew">>,1},{"content",JSON}]}),
													gen_tcp:send(Socket, Myjson)
											end,
                                       %% for test
                                        emysql:prepare(my_stmt, <<"delete from emc_meeting_user_log where flag=0 and  uid =?">>),
							            emysql:execute(myjqrealtime, my_stmt, [Uid]);
									_->
										Result = emysql:execute(myjqrealtime,
																lists:concat([
																			  "SELECT mid,status FROM emc_meeting_user_log WHERE flag=0 and   uid = ",
																			  emysql_util:quote(getclean(Uid)),
																			  ""
																			 ]
																			)),
										Records = emysql_util:as_record(Result, sessions, record_info(fields, sessions)),
                                            if
												length(Records) > 0 ->
													JSON = emysql_util:as_json(Result),
													Myjson = mochijson2:encode({struct,[{<<"isNew">>,0},{"content",JSON}]}),
													gen_tcp:send(Socket,Myjson);
												 true ->
                                                   gen_tcp:send(Socket,"")
											end,
                                       %% for test
                                        emysql:prepare(my_stmt, <<"delete from emc_meeting_user_log where flag=0 and  uid =?">>),
							            emysql:execute(myjqrealtime, my_stmt, [Uid]) ,					
								        gen_tcp:send(Socket,"")
								end;
								false ->
									emysql:prepare(my_stmt, <<"delete from emc_meeting_user_log where uid =?">>),
									emysql:execute(myjqrealtime, my_stmt, [Uid]),
									emysql:prepare(my_stmt, <<"INSERT INTO emc_meeting_user_log SET uid =?, flag=?">>),
									emysql:execute(myjqrealtime, my_stmt, [Uid,1])
							end
				%%			gen_tcp:send(Socket, "{\"isNew\":1,\"content\":[{\"mid\":11,\"status\":0}]}\n")
					end,
    feed(Uid, Socket)
    catch
        Type:What ->
			   emysql:prepare(my_stmt, <<"delete from emc_meeting_user_log where uid =?">>),
		       emysql:execute(myjqrealtime, my_stmt, [Uid])
    end.
%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%% Get Value or "" if undefined
getclean(X) when X /= undefined ->
    X;
getclean(_) ->
    "".

getNumber(X) when X /= undefined ->
    X;
getNumber(_) ->
    0.


say(Nick, Socket, Content) ->
    gen_server:cast(emcs_controller, {say, Nick, Content}),
    loop(Nick, Socket).

private_message(Nick, Socket, ReceiverNick, Msg) ->
     gen_server:cast(emcs_controller, {private_message, Nick, ReceiverNick, Msg}),
     loop(Nick, Socket).

quit(Nick, Socket) ->
    Response = gen_server:call(emcs_controller, {disconnect, Nick}),
    case Response of
        ok ->
            gen_tcp:send(Socket, "Bye.\n"),
            gen_server:cast(emcs_controller, {left, Nick}),
            ok;
        user_not_found ->
            gen_tcp:send(Socket, "Bye with errors.\n"),
            ok
    end.

clean(Data) ->
    string:strip(Data, both, $\n).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
