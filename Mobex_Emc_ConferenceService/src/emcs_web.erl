%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for emcs.

-module(emcs_web).
-author("Mochi Media <dev@mochimedia.com>").

-include("emcs.hrl").

-export([start/1, stop/0, loop/2]).

%% MySQL Configuration
-define(MYSQL_SERVER, "192.168.1.143").
-define(MYSQL_USER, "root").
-define(MYSQL_PASSWD, "password").
-define(MYSQL_DB, "emc").
-define(MYSQL_PORT, 3306).

-record(sessions, {id, uid, status,flag,mid}).


%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
	{{host,Host},{username,Username},{password,Password}, {dbname, Dbname}}=emcs_config:get_config(database),
	%%error_logger:info_msg("database config message ~n ~p~n",[Host]),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    % start mysql
    application:start(emysql),
    emysql:add_pool(myjqrealtime, 1, Username, Password, Host, ?MYSQL_PORT, Dbname, utf8),
	
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).


%% Check session util
check_session(Uid) ->

    %% Check session
    CheckSession = emysql:execute(myjqrealtime, 
        lists:concat([
            "SELECT * FROM emc_meeting_user_log WHERE flag=1 and uid = ", 
            emysql_util:quote(getclean(Uid)),
            " LIMIT 1"
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


loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
	%%error_logger:info_msg("Path message ~n ~p~n",[Path]),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                "emcs/" ++ Id ->
                    Response = Req:ok({"text/html; charset=utf-8",
                                      [{"Server","Mochiweb-Test"}],
                                      chunked}),
					feed(Response, Id, 1);
					"hello" ->
						QueryStringData = Req:parse_qs(),
						Username = proplists:get_value("username", QueryStringData, "Anonymous"),
						Req:respond({200, [{"Content-Type", "text/plain"}],
									 "Hello " ++ Username ++ "!\n"});
					_ ->
						Req:serve_file(Path, DocRoot)
				end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
			%% for test
			case Path of
                "emcs/" ++ Uid ->
			   emysql:prepare(my_stmt, <<"delete from emc_meeting_user_log where uid =?">>),
			   emysql:execute(myjqrealtime, my_stmt, [Uid])
			end,

            %%Report = ["web request failed",
            %%          {path, Path},
            %%          {type, Type}, {what, What},
            %%          {trace, erlang:get_stacktrace()}],
            %%error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

feed(Response, Id, N) ->
    receive
        %{router_msg, Msg} ->
        %    Html = io_lib:format("Recvd msg #~w: '~s'<br/>", [N, Msg]),
        %    Response:write_chunk(Html);
    after 5000 ->
					case check_session(Id) of
						{Rid} ->%%login in before
                              
							    case check_have_new_conference(Id) of
									true->
										Result = emysql:execute(myjqrealtime,
																lists:concat([
																			  "SELECT mid,status FROM emc_meeting_user_log WHERE flag=0 and  uid = ",
																			  emysql_util:quote(getclean(Id)),
																			  " and id>",
																			  emysql_util:quote(getNumber(Rid))
																			 ]
																			)),
										JSON = emysql_util:as_json(Result),
										Myjson = mochijson2:encode([<<"new">>,1|JSON]),
                                       %% for test
                                       emysql:prepare(my_stmt, <<"delete from emc_meeting_user_log where uid =?">>),
							           emysql:execute(myjqrealtime, my_stmt, [Id]),										
										Response:write_chunk(Myjson);
									false->
										Result = emysql:execute(myjqrealtime,
																lists:concat([
																			  "SELECT mid,status FROM emc_meeting_user_log WHERE flag=0 and   uid = ",
																			  emysql_util:quote(getclean(Id)),
																			  ""
																			 ]
																			)),
										Records = emysql_util:as_record(Result, sessions, record_info(fields, sessions)),
                                            if
												length(Records) > 0 ->
													JSON = emysql_util:as_json(Result),
													Myjson = mochijson2:encode([<<"new">>,0|JSON]),
													Response:write_chunk(Myjson);
												true ->
                                                  Response:write_chunk("")
											end,
                                       %% for test
                                       emysql:prepare(my_stmt, <<"delete from emc_meeting_user_log where uid =?">>),
							           emysql:execute(myjqrealtime, my_stmt, [Id]),					
								        Response:write_chunk("")
								end;
						false ->%%not login in before
                               %% for test
                               emysql:prepare(my_stmt, <<"delete from emc_meeting_user_log where uid =?">>),
							   emysql:execute(myjqrealtime, my_stmt, [Id]),
							   emysql:prepare(my_stmt, <<"INSERT INTO emc_meeting_user_log SET uid =?, flag=?">>),
							   emysql:execute(myjqrealtime, my_stmt, [Id,1])
					end,
           Response:write_chunk("")
    end,
    feed(Response, Id, N+1).

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
