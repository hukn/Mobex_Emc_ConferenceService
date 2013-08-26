%% @author kangning.hu
%% @doc @todo Add description to emcs_config.


-module(emcs_config).

-include("emcs.hrl").
-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([get_config/1]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
        terminate/2,code_change/3]).


%  api function
start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

get_config(Key)->
    gen_server:call(?MODULE, {get_conf,Key}).


% ------------------callback ---------------------
init([]) ->
    {ok,Terms}=file:consult(?EMCS_CONFIG_PATH),
    {ok,Terms}.

handle_call({get_conf,Key},_From,State) ->
    %%io:format("~w~n",[State]),
    Reply=case Term=lists:keyfind(Key,1,State) of
	    false -> undefined;
	    _     -> element(2,Term)
	end ,
    {reply,Reply,State};

handle_call(_Request,_From,State) ->
    {reply,ok,State}.

handle_cast(_Msg,State) ->    
    {noreply,State}.


handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

