%%% -------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Jesse Watts-Russell, David Scott, Alex Melville
%%% @copyright ChickenFartStory Inc. 2015, All rights reserved
%%%
%%% @doc A Generic Server Implementation of a Player
%%% @end
%%%--------------------------------------------------------------------

-module(yahtzee_player1).
-behavior(gen_server).

%% External exports
-export([main/1
         ]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 code_change/3, 
		 terminate/2]).

-define(PROCNAME, player1).
-define(MANAGER_NAME, yahtzee_manager).

-record(state, {username, 
                ticket, 
                activeTournaments, 
                numPendingTournaments}).


%%%============================================================================
%%% API
%%%============================================================================

main([NodeName, Username, Password, TournamentManagerNames]) ->
    os:cmd("epmd -daemon"),
    net_kernel:start([list_to_atom(NodeName), shortnames]),
    %% I don't think we need to register players at all?
    gen_server:start({local, ?PROCNAME}, ?MODULE, 
		      {Username, Password, TournamentManagerNames}, []).


%%%============================================================================
%%% GenServer Calls/Casts
%%%============================================================================


%%%============================================================================
%%% GenServer Callbacks 
%%%============================================================================

%% @spec init({NodesToConnectTo}) -> {ok, State}.
init({Username, Password, TournamentManagerNames}) ->
    login_to_managers(TournamentManagerNames, Username, Password),
    {ok, #state{username = Username}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% SYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(_, _, S) ->
    {reply, ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%% END SYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% ASYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast(_, S) ->
    {noreply, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%% END ASYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% OUTSIDE ASYNCHRONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%% END OUTSIDE ASYNCHRONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    io:format(timestamp() ++ ": terminate reason: ~p~n", [_Reason]).





%%%============================================================================
%%% Other (possibly) useful functions
%%%============================================================================



%% @spec login_to_managers(List::TournamentManagerNames, string()::Username), string()::Password -> ok
%% @doc Sends a message to all of the nodes in the list of tournament managers asking
%% them each to register the player with the given username and password
login_to_managers([], _, _) -> ok;
login_to_managers(TournamentManagerNames, Username, Password) ->
    io:format(timestamp() ++ ": sending login message to node with name '~p~n",
     [hd(TournamentManagerNames)]),
    {?MANAGER_NAME, hd(TournamentManagerNames)} ! {login, self(), Username, {Username, Password}},
    login_to_managers(tl(TournamentManagerNames), Username, Password).


    

%% @spec timestamp() -> string()
%% @doc Generates a fancy looking timestamp, found on:
%%		http://erlang.2086793.n4.nabble.com/formatting-timestamps-td3594191.html
timestamp() -> 
    Now = now(),
    {_, _, Micros} = Now, 
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now), 
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~p", 
                  [YY, MM, DD, Hour, Min, Sec, Micros]). 
