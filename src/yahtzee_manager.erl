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

define(userInfo, ?UserInfo).
define(currentPlayerInfo, ?CPI).
define(tournamentInfo, ?TournamentInfo).
define(timeOutRefs, ?TimeOutRefs).


-record(user, {password,
							 match_wins = 0,
							 match_losses = 0,
							 tournaments_played = 0,
							 tournaments_won = 0}).

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
				seed,
				gamesPerMatch
				}).

-record(tournament, {dictOfSeedsWithPlayers = dict:new(),
					 listOfInProgressMatches = [],
					 status = in_progress,
					 winner = undefined,
					 numPlayersReplied = 0,
					 numNeededReplies,
					 gamesPerMatch,
					 started = false
					 }).

-record(tm, {playerDict}). % A dictionary of Username:{Pid, MonitorRef, LoginTicket}


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
	% UserInfo will hold 
	% Username: user record (see above)
	ets:new(?UserInfo, [set, protected, named_table]),

	% TournamentInfo will hold tid : #tournament
	ets:new(?TournamentInfo, [set, protected, named_table]),

	ets:new(?TimeOutRefs, [set, protected, named_table]),


	% Username : {[{Tid, [Gids]}]}
	ets:new(?CPI, [set, protected, named_table]),

	{ok, #tm{playerDict = dict:new()}}.

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
	PlayerDict = TMState#tm.playerDict,
	LoginTicket = make_ref(),

	case ets:lookup(?UserInfo, Username) of
		[] ->
			ets:insert(?UserInfo, {Username, #user{password = Password}}),
			MonitorRef = monitor(process, Pid),
			NewPlayerDict = dict:append(Username, {Pid, MonitorRef, LoginTicket}, PlayerDict),
			Pid ! {logged_in, self(), Username, LoginTicket},
			{noreply, TMState#tm{playerDict = NewPlayerDict}};

		[{Username, {Password, _, _, _, _}}] ->
			MonitorRef = monitor(process, Pid),
			NewPlayerDict = dict:append(Username, {Pid, MonitorRef, LoginTicket}, PlayerDict),
			Pid ! {logged_in, self(), Username, LoginTicket},
			{noreply, TMState#tm{playerDict = NewPlayerDict}};

		[{Username, {_DiffPassword, _, _, _, _}}] ->
			Pid ! {incorrect_password, Username},
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
			Pid ! {incorrect_login_ticket, Username},
			{noreply, TMState}
	end;


%% @spec handle_info({acceptTournament, Pid, Username, {Tid, LoginTicket}}, TMState)
%%						 									-> {noreply, S};
%% @doc This message indicated that the player is willing to play in the
%% specified tournament

handle_info({accept_tournament, Pid, Username, {Tid, LoginTicket}}, TMState) ->
	[{_, TimeOutRef}] = ets:lookup(?TimeOutRefs, {Username, Tid}),
	ets:delete(?TimeOutRefs, {Username, Tid}),
	timer:cancel(TimeOutRef),

	[{Tid, T}] = ets:lookup(?TournamentInfo, Tid),
	case T#tournament.started of
		true ->
			{noreply, TMState};
		false ->
			NewDictOfSeedsWithPlayers = dict:append(0, Username, T#tournament.dictOfSeedsWithPlayers), 
			NumPlayersReplied = T#tournament.numPlayersReplied + 1,
			case NumPlayersReplied == T#tournament.numNeededReplies of
				true ->
					NewTM = T#tournament{dictOfSeedsWithPlayers = NewDictOfSeedsWithPlayers,
										 numPlayersReplied = NumPlayersReplied,
										 started = true},
					start_tournament(Tid, NewTM),
					{noreply, TMState};

					

	




%%%%%%%%%%%%%%%%%% END RECEIVING MESSAGES SENT BY PLAYER %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%%%%%

handle_info({request_tournament, Pid, {NumPlayers, GamesPerMatch}}, TMState) ->
	case (NumPlayers rem 2) == 1 of
		true ->
			Tid = make_ref(),
			PlayerDict = TMState#tm.playerDict,
			PlayerList = dict:fetch_keys(PlayerDict),
			{NumChosenPlayers, ChosenPlayers} = shuffle(PlayerList, NumPlayers),

			NewTournament = #tournament{numNeededReplies = NumChosenPlayers,
							 										gamesPerMatch = GamesPerMatch}

			lists:foreach(fun(X) -> 
							handle_ask_player(X, dict:fetch(X), Tid) end,
							ChosenPlayers),
			ets:insert(?TournamentInfo, {Tid, NewTournament}),
			{noreply, TMState};

		false ->
			{noreply, TMState}
	end;



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


shuffle(List, K) ->
	{A, B, C} = now(),
	random:seed(A,B,C),
	Shuffled = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
	Chosen = lists:sublist(Shuffled, K),
	Num = length(Chosen),
	{Num, Chosen}.




start_tournament(Tid, T) ->
	DictOfSeedsWithPlayers = T#tournament.dictOfSeedsWithPlayers,
	ZeroSeedPlayers = dictFetch(0, DictOfSeedsWithPlayers),
	GamesPerMatch = T#tournament.gamesPerMatch,
	case length(ZeroSeedPlayers) == 0 of
		true ->
			ets:insert(?TournamentInfo, {Tid, T#tournament{status = complete});
		
		false ->	
			case length(ZeroSeedPlayers) == 1 of
				true ->
					{Winner, 0} = hd(ZeroSeedPlayers),
					ets:insert(?TournamentInfo, {Tid, T#tournament{ZeroSeedPlayers = dict:new(),
																												 status = complete,
																												 winner = Winner}}),
					[{Winner, UserData}] = ets:lookup(?UserInfo, Winner),
					TournamentWins = UserData#user.tournaments_won + 1,
					TournamentsPlayed = UserData#user.tournaments_played + 1,
					NewUserData = UserData#user{tournaments_played = TournamentsPlayed,
																			tournaments_won = TournamentWins},

					ets:insert(?UserInfo, {Winner, NewUserData});

				false ->
					make_matches

% -record(match, {p1ListOfDice,
% 				p2ListOfDice,
% 				p1ScoreCard = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
% 				p2ScoreCard = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
% 				p1ID,
% 				p2ID,
% 				p1WinLoss = {0, 0},
% 				p2WinLoss = {0, 0},
% 				gid,
% 				tid,
% 				numScorecardsChecked = 0,
% 				seed,
% 				gamesPerMatch
% 				}).


% -record(tournament, {dictOfSeedsWithPlayers,
% 					 listOfInProgressMatches,
% 					 status = in_progress,
% 					 winner = undefined,
% 					 numPlayersReplied = 0,
% 					 numNeededReplies,
% 					 gamesPerMatch,
% 					 started = false
% 					 }).


make_matches(Tid, T) ->
	DictOfSeedsWithPlayers = T#tournament.dictOfSeedsWithPlayers,
	GamesPerMatch = T#tournament.gamesPerMatch,	



handle_ask_player(ChosenPlayer, {Pid, _MonitorRef, LoginTicket}, Tid) ->
	Pid ! {start_tournament, self(), ChosenPlayer, Tid},
	{ok, TimerRef} = timer:send_after(60000, {reject_tournament, Pid, ChosenPlayer, {Tid, LoginTicket}}),
	ets:insert(?TimeOutRefs, {{ChosenPlayer, Tid}, TimerRef}),
	ok.


dictFetch(Key, Dict) ->
	try dict:fetch(Key, Dict) of
		Moo -> Moo
	catch
		_:_ -> []
	end.
	