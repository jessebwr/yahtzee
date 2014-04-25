%%% ---------------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Jesse Watts-Russell, David Scott, and Alex Melville
%%% @copyright ChickenFartStory Inc. 2015, All rights reserved
%%%
%%% @doc A Generic Server implementation of a Tournament Manager according
%%%		 to OTP principles.
%%% @end
%%%----------------------------------------------------------------------------

-module(yahtzee_manager).
-behavior(gen_server).

%% External exports
-export([main/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 code_change/3, 
		 terminate/2]).


-record(match, {p1ListOfDice,
				p2ListOfDice,
				p1ScoreCard = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
				p2ScoreCard = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
				p1ID,
				p2ID,
				p1WinLoss = {0, 0},
				p2WinLoss = {0, 0},
				gid,
				tid,
				numScorecardsChecked = 0,
				seed
				}).

-record(tournament, {listOfPlayersWithSeeds,
					 listOfInProgressMatches,
					 tid,
					 status = inProgress,
					 winner = undefined,
					 numPlayersRegistered = 0
					 }).

-record(tm, {userInfo,
			 tournamentInfo,
			 playerDict}). % A dictionary of Username:{Pid, MonitorRef, LoginTicket}


%%%============================================================================
%%% API
%%%============================================================================

main([StrNodeName]) ->
	NodeName = list_to_atom(StrNodeName),
	os:cmd("epmd -daemon"),
	net_kernel:start([NodeName, shortnames]),
	gen_server:start({local, NodeName}, yahtzee_manager, {}, []).




%%%============================================================================
%%% GenServer Calls/Casts
%%%============================================================================

%ets:insert(TabId, {Key, Value})

%%%============================================================================
%%% GenServer Callbacks
%%%============================================================================

init({}) ->
	% UserInfo will hold {password, match-wins, match-losses, tournaments-played, 
	%					  tournaments-wins}
	UserInfo = ets:new(dontMatter, [set, protected]),

	% TournamentInfo will hold tid : {status, winner}
	TournamentInfo = ets:new(dontMatter, [set, protected]),

	{ok, #tm{userInfo = UserInfo,
		  tournamentInfo = TournamentInfo,
		  playerDict = dict:new()}}.

handle_call(_, _, S) ->
	{reply, ok, S}.


handle_cast(_, S) ->
	{noreply, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% RECEIVING MESSAGES SENT BY PLAYER %%%%%%%%%%%%%%%%%%%%%%%

%% @spec handle_info({login, Pid, Username, {Username, Password}}, TMState)
%%						 									-> {noreply, S};
%% @doc This message is received from a player and is used to log into the 
%% tournament manager
handle_info({login, Pid, Username, {Username, Password}}, TMState) ->
	UserInfo = TMState#tm.userInfo,
	PlayerDict = TMState#tm.playerDict,
	LoginTicket = make_ref(),

	case ets:lookup(UserInfo, Username) of
		[] ->
			ets:insert(UserInfo, {Username, {Password, 0, 0, 0, 0}}),
			MonitorRef = monitor(process, Pid),
			NewPlayerDict = dict:append(Username, {Pid, MonitorRef, LoginTicket}, PlayerDict),
			Pid ! {loggedIn, self(), Username, LoginTicket},
			{noreply, TMState#tm{playerDict = NewPlayerDict}};

		[{Username, {Password, _, _, _, _}}] ->
			MonitorRef = monitor(process, Pid),
			NewPlayerDict = dict:append(Username, {Pid, MonitorRef, LoginTicket}, PlayerDict),
			Pid ! {loggedIn, self(), Username, LoginTicket},
			{noreply, TMState#tm{playerDict = NewPlayerDict}};

		[{Username, {_DiffPassword, _, _, _, _}}] ->
			Pid ! {incorrectPassword, Username},
			{noreply, TMState}
	end;


%% @spec handle_info({logout, Pid, Username, LoginTicket}, TMState)
%%						 									-> {noreply, S};
%% @doc This message is received from a player who wants to log out
%% a player with the username Username
handle_info({logout, Pid, Username, LoginTicket}, TMState) ->
	PlayerDict = TMState#tm.playerDict,
	{_Pid, MonitorRef, TrueLoginTicket} = dict:fetch(Username, PlayerDict),

	case LoginTicket == TrueLoginTicket of
		true ->
			demonitor(MonitorRef,[flush]),
			NewPlayerDict = dict:erase(Username, PlayerDict),
			{noreply, TMState#tm{playerDict = NewPlayerDict}};
		false ->
			Pid ! {incorrectLoginTicket, Username},
			{noreply, TMState}
	end;


%% @spec handle_info({acceptTournament, Pid, Username, {Tid, LoginTicket}}, TMState)
%%						 									-> {noreply, S};
%% @doc This message indicated that the player is willing to play in the
%% specified tournament
%%handle_info({acceptTournament, Pid, Username, {Tid, LoginTicket}}, TMState) ->
	




%%%%%%%%%%%%%%%%%% END RECEIVING MESSAGES SENT BY PLAYER %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%%%%%

% handle_info({requestTournament, Pid, {NumPlayers, GamesPerMatch}}, TMState) ->


%%%%%%%%%%%%%%%% END RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(MSG, S) ->
	io:format("not recognized message: ~p~n", [MSG]),
	{noreply, S}.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    io:format(timestamp() ++ ": terminate reason: ~p~n", [_Reason]).

%%%============================================================================
%%% Other (possibly) useful functions
%%%============================================================================

%% @spec timestamp(Now) -> string()
%% @doc Generates a fancy looking timestamp, found on:
%%		http://erlang.2086793.n4.nabble.com/formatting-timestamps-td3594191.html
timestamp() -> 
	Now = now(),
    {_, _, Micros} = Now, 
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now), 
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~p", 
                  [YY, MM, DD, Hour, Min, Sec, Micros]).

