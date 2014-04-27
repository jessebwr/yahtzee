%%% ---------------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Jesse Watts-Russell, David Scott, and Alex Melville
%%% @copyright ChickenFartStory Inc. 2015, All rights reserved
%%%
%%% @doc A Generic Server implementation of a Tournament Manager according
%%%     to OTP principles.
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

-define(UserInfo, userInfo).
-define(CurrentPlayerTournamentInfo, currentPlayerTournamentInfo).
-define(CurrentPlayerLoginInfo, currentPlayerLoginInfo).
-define(TournamentInfo, tournamentInfo).
-define(TimeOutRefs, timeOutRefs).
-define(MatchTable, matchTable).


-record(user, {password,
               match_wins = 0,
               match_losses = 0,
               tournaments_played = 0,
               tournaments_won = 0}).

-record(match, {p1ListOfDice,
        p2ListOfDice,
        p1ScoreCard = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
        p2ScoreCard = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
        p1,
        p2,
        p1RollNum = 0,
        p2RollNum = 0,
        p1WinLoss = {0, 0},
        p2WinLoss = {0, 0},
        tid,
        numScorecardsChecked = 0,
        seed,
        currentGame,
        gamesPerMatch
        }).

-record(tournament, {dictOfPlayersWithSeeds = ets:new(moo, [bag]),
           listOfInProgressMatches = [],
           status = in_progress,
           winner = undefined,
           numPlayersReplied = 0,
           numNeededReplies,
           gamesPerMatch,
           started = false,
           pidThatRequested
           }).

-record(tm, {}). 


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

  % Just some timeout references
  ets:new(?TimeOutRefs, [set, protected, named_table]),

  % Username : [{Tid, [Gids]}, {Tid, [Gids]}, {Tid, [Gids]}]
  ets:new(?CurrentPlayerTournamentInfo, [set, protected, named_table]),

   % {Tid, Gid} : Match
  ets:new(?MatchTable, [set, protected, named_table]),

  % A dictionary of Username:{Pid, MonitorRef, LoginTicket}
  ets:new(?CurrentPlayerLoginInfo, [set, protected, named_table]),


  {ok, #tm{}}.

handle_call(_, _, S) ->
  {reply, ok, S}.


handle_cast(_, S) ->
  {noreply, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% RECEIVING MESSAGES SENT BY PLAYER %%%%%%%%%%%%%%%%%%%%%%%

%% @spec handle_info({login, Pid, Username, {Username, Password}}, S)
%%                               -> {noreply, S};
%% @doc This message is received from a player and is used to log into the 
%% tournament manager
handle_info({login, Pid, Username, {Username, Password}}, S) ->
  LoginTicket = make_ref(),

  case ets:lookup(?UserInfo, Username) of
    [] ->
      % Initializing the user in our record database
      ets:insert(?UserInfo, {Username, #user{password = Password}}),

      % Monitoring it and setting up its current info
      MonitorRef = monitor(process, Pid),
      ets:insert(?CurrentPlayerLoginInfo, {Username, {Pid, MonitorRef, LoginTicket}}),

      % Messaging that they are logged in.
      Pid ! {logged_in, self(), Username, LoginTicket},
      {noreply, S};

    [{Username, {Password, _, _, _, _}}] ->

      % Monitoring it and stting up its current info
      MonitorRef = monitor(process, Pid),
      ets:insert(?CurrentPlayerLoginInfo, {Username, {Pid, MonitorRef, LoginTicket}}),

      % Messaging that they are logged in
      Pid ! {logged_in, self(), Username, LoginTicket},
      {noreply, S};

    % They didn't give the right password
    [{Username, {_DiffPassword, _, _, _, _}}] ->
      Pid ! {incorrect_password, Username},
      {noreply, S}
  end;


%% @spec handle_info({logout, Pid, Username, LoginTicket}, S)
%%                               -> {noreply, S};
%% @doc This message is received from a player who wants to log out
%% a player with the username Username
handle_info({logout, Pid, Username, LoginTicket}, S) ->
  [{Username, {_Pid, MonitorRef, TrueLoginTicket}}] = ets:lookup(?CurrentPlayerLoginInfo, Username),

  case LoginTicket == TrueLoginTicket of
    true ->
      % Demonitoring
      demonitor(MonitorRef,[flush]),

      % Deleting this user's current session information
      ets:delete(?CurrentPlayerLoginInfo, Username),
      {noreply, S};

    % Someone else was trying to log them out...
    false ->
      Pid ! {incorrect_login_ticket, Username},
      {noreply, S}
  end;


%% @spec handle_info({acceptTournament, Pid, Username, {Tid, LoginTicket}}, S)
%%                               -> {noreply, S};
%% @doc This message indicated that the player is willing to play in the
%% specified tournament

handle_info({accept_tournament, Pid, Username, {Tid, LoginTicket}}, S) ->
  [{Username, {_Pid, _MonitorRef, TrueLoginTicket}}] = ets:lookup(?CurrentPlayerLoginInfo, Username),
  case LoginTicket == TrueLoginTicket of
    true ->
      [{_, TimeOutRef}] = ets:lookup(?TimeOutRefs, {Username, Tid}),
      ets:delete(?TimeOutRefs, {Username, Tid}),
      timer:cancel(TimeOutRef),

      [{Tid, T}] = ets:lookup(?TournamentInfo, Tid),
      case T#tournament.started of
        true ->
          {noreply, S};
        false ->

          % Since they accepted, add them to the dictionary of players
          TournamentEts = T#tournament.dictOfPlayersWithSeeds,
          ets:insert(TournamentEts, {Username, 0}), 

          % If we have enough players, start the tournament
          NumPlayersReplied = T#tournament.numPlayersReplied + 1,
          case NumPlayersReplied == T#tournament.numNeededReplies of
            true ->
              NewTM = T#tournament{numPlayersReplied = NumPlayersReplied,
                                   started = true},
              ets:insert(?TournamentInfo, {Tid, NewTM}),
              start_tournament(Tid, NewTM),
              {noreply, S};
            false ->
              NewTM = T#tournament{numPlayersReplied = NumPlayersReplied},
              ets:insert(?TournamentInfo, {Tid, NewTM}),
              {noreply, S}
          end
      end;
    false -> 
      Pid ! {incorrect_login_ticket, Username},
      {noreply, S}
  end;


          
%% @spec handle_info({rejectTournament, Pid, Username, {Tid, LoginTicket}}, S)
%%                               -> {noreply, S};
%% @doc This message indicated that the player is not willing to play in the
%% specified tournament

handle_info({reject_tournament, Pid, Username, {Tid, LoginTicket}}, S) ->
  [{Username, {_Pid, _MonitorRef, TrueLoginTicket}}] = ets:lookup(?CurrentPlayerLoginInfo, Username),
  case LoginTicket == TrueLoginTicket of
    true ->
      [{_, TimeOutRef}] = ets:lookup(?TimeOutRefs, {Username, Tid}),
      ets:delete(?TimeOutRefs, {Username, Tid}),
      timer:cancel(TimeOutRef),

      [{Tid, T}] = ets:lookup(?TournamentInfo, Tid),
      case T#tournament.started of
        true ->
          {noreply, S};
        false -> 
          NumPlayersReplied = T#tournament.numPlayersReplied + 1,

          % If we have enough replies, start the tournament
          case NumPlayersReplied == T#tournament.numNeededReplies of
            true ->
              NewTM = T#tournament{numPlayersReplied = NumPlayersReplied,
                                   started = true},
              start_tournament(Tid, NewTM),
              {noreply, S};
            false ->
              NewTM = T#tournament{numPlayersReplied = NumPlayersReplied},
              ets:insert(?TournamentInfo, {Tid, NewTM}),
              {noreply, S}
          end
      end;
    false ->
      Pid ! {incorrect_login_ticket, Username},
      {noreply, S}
  end;




%%%%%%%%%%%%%%%%%% END RECEIVING MESSAGES SENT BY PLAYER %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%%%%%

handle_info({request_tournament, Pid, {NumPlayers, GamesPerMatch}}, S) ->
  case (NumPlayers rem 2) == 1 of
    true ->
      Tid = make_ref(),

      % Getting Ets Table Keys i.e. Usernames
      PlayerList = lists:flatten(ets:match(?CurrentPlayerLoginInfo, {'$1', '_'})),

      {NumChosenPlayers, ChosenPlayers} = shuffle(PlayerList, NumPlayers),

      NewTournament = #tournament{numNeededReplies = NumChosenPlayers,
                                   gamesPerMatch = GamesPerMatch,
                                   pidThatRequested = Pid},

      % Ask each of the players
      lists:foreach(fun(X) -> 
                        [{X, XInfo}] = ets:lookup(?CurrentPlayerLoginInfo, X),
                        handle_ask_player(X, XInfo, Tid) 
                  end, ChosenPlayers),

      % inserting the tournament info
      ets:insert(?TournamentInfo, {Tid, NewTournament}),
      {noreply, S};

    false ->
      {noreply, S}
  end;



%%%%%%%%%%%%%%%% END RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(MSG, S) ->
  io:format("not recognized message: ~p~n", [MSG]),
  {noreply, S}.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    io:format(utils:timestamp() ++ ": terminate reason: ~p~n", [_Reason]).

%%%============================================================================
%%% Other (possibly) useful functions
%%%============================================================================




shuffle(List, K) ->
  {A, B, C} = now(),
  random:seed(A,B,C),
  Shuffled = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
  Chosen = lists:sublist(Shuffled, K),
  Num = length(Chosen),
  {Num, Chosen}.


%%%%%%%% Starting up a tournament %%%%%%%%%%%%%%% 

start_tournament(Tid, T) ->
  EtsOfPlayersWithSeeds = T#tournament.dictOfPlayersWithSeeds,
  NumPlayers = ets:info(EtsOfPlayersWithSeeds, size),
  case NumPlayers == 0 of
    true ->
      ets:insert(?TournamentInfo, {Tid, T#tournament{status = complete}}),
      ets:delete(EtsOfPlayersWithSeeds);
    false ->  
      case NumPlayers == 1 of
        true ->
          Winner = ets:first(EtsOfPlayersWithSeeds),
          ets:delete(EtsOfPlayersWithSeeds),
          ets:insert(?TournamentInfo, {Tid, T#tournament{status = complete,
                                                         winner = Winner}}),
          [{Winner, UserData}] = ets:lookup(?UserInfo, Winner),
          TournamentWins = UserData#user.tournaments_won + 1,
          TournamentsPlayed = UserData#user.tournaments_played + 1,
          NewUserData = UserData#user{tournaments_played = TournamentsPlayed,
                                      tournaments_won = TournamentWins},

          ets:insert(?UserInfo, {Winner, NewUserData});

        false ->
          NumByesNeeded = utils:nextPow2(NumPlayers) - NumPlayers,
          PlayerList = lists:flatten(ets:match(EtsOfPlayersWithSeeds, {'$1', 0})),
          % Inserting Extra Byes Players
          ByesList = lists:duplicate(NumByesNeeded, bye),
          lists:foreach(fun(_Bye) -> ets:insert(EtsOfPlayersWithSeeds, {bye, 0}) end, ByesList),

          make_initial_matches_loop(Tid, T),
          T#tournament.pidThatRequested ! {tournament_started, self(), {Tid, PlayerList, blah}}
      end
  end.


make_initial_matches_loop(Tid, T) ->
  EtsOfPlayersWithSeeds = T#tournament.dictOfPlayersWithSeeds,
  case ets:info(EtsOfPlayersWithSeeds, size) == 0 of
    true ->
      ok;
    false ->
      Player1 = ets:first(EtsOfPlayersWithSeeds),
      ets:delete(EtsOfPlayersWithSeeds, Player1),
      Player2 = ets:first(EtsOfPlayersWithSeeds),
      ets:delete(EtsOfPlayersWithSeeds, Player2),
      make_single_match(Tid, T, Player1, Player2, 0),
      make_initial_matches_loop(Tid, T)
  end.


%%%%%%%%%%%%%%%% Making and Handling Matches %%%%%%%%%%%%%%%%%%%%%

make_single_match(Tid, T, InitPlayer1, InitPlayer2, Seed) ->

  % This makes sure that if there is at least one bye, it will
  % be Player1, because atoms are always before strings.
  [Player1, Player2] = lists:sort([InitPlayer1, InitPlayer2]),

  case Player1 == bye of
    % If we are fighting a bye, then just increase our seed.
    % No need to create a match
    true -> 
      EtsOfPlayersWithSeeds = T#tournament.dictOfPlayersWithSeeds,
      ets:insert(EtsOfPlayersWithSeeds, {Player1, Seed + 1});

    % Create a match and send the initial dice
    false ->
      GamesPerMatch = T#tournament.gamesPerMatch,
      Dice = generateDice(),
      Match = #match{p1ListOfDice = Dice,
                     p2ListOfDice = Dice,
                     p1 = Player1,
                     p2 = Player2,
                     tid = Tid,
                     seed = Seed,
                     currentGame = 0,
                     gamesPerMatch = GamesPerMatch},
      Gid = make_ref(),
      sendDice(Tid, Gid, Match, 5, 5)
  end.


make_single_match(Tid, T, Player1, Player2, Seed, identical) ->
  Dice1 = generateDice(),
  Dice2 = generateDice(),
  GamesPerMatch = T#tournament.gamesPerMatch,
  Match = #match{p1ListOfDice = Dice1,
                 p2ListOfDice = Dice2,
                 p1 = Player1,
                 p2 = Player2,
                 tid = Tid,
                 seed = Seed,
                 currentGame = 0,
                 gamesPerMatch = GamesPerMatch},
  Gid = make_ref(),
  sendDice(Tid, Gid, Match, 5, 5).


sendDice(Tid, Gid, M, NumDiceToSendP1, NumDiceToSendP2) ->
	P1 = M#match.p1,
	P2 = M#match.p2,

	[{P1, {Pid1, _MonitorRef1, _LoginTicket1}}] = ets:lookup(?CurrentPlayerLoginInfo, P1),
  [{P1, {Pid2, _MonitorRef2, _LoginTicket2}}] = ets:lookup(?CurrentPlayerLoginInfo, P1),

	P1DiceToSend = lists:sublist(M#match.p1ListOfDice, NumDiceToSendP1),
	P2DiceToSend = lists:sublist(M#match.p2ListOfDice, NumDiceToSendP2),

	P1NewBackups = lists:sublist(M#match.p1ListOfDice, NumDiceToSendP1 + 1, length(M#match.p1ListOfDice)),
	P2NewBackups = lists:sublist(M#match.p2ListOfDice, NumDiceToSendP2 + 1, length(M#match.p2ListOfDice)),

  P1Msg = {make_ref(), Tid, Gid, M#match.p1RollNum, P1DiceToSend, M#match.p1ScoreCard, M#match.p2ScoreCard},
  P2Msg = {make_ref(), Tid, Gid, M#match.p2RollNum, P2DiceToSend, M#match.p2ScoreCard, M#match.p1ScoreCard},

  ets:insert(?MatchTable, {{Tid, Gid}, M#match{p1RollNum = M#match.p1RollNum + 1, 
                                               p2RollNum = M#match.p1RollNum + 1,
                                               p1ListOfDice = P1NewBackups,
                                               p2ListOfDice = P2NewBackups}}),

  Pid1 ! {play_request, self(), P1, P1Msg},
  Pid2 ! {play_request, self(), P2, P2Msg}.


handle_ask_player(ChosenPlayer, {Pid, _MonitorRef, LoginTicket}, Tid) ->
  Pid ! {start_tournament, self(), ChosenPlayer, Tid},
  {ok, TimerRef} = timer:send_after(60000, {reject_tournament, Pid, ChosenPlayer, {Tid, LoginTicket}}),
  ets:insert(?TimeOutRefs, {{ChosenPlayer, Tid}, TimerRef}),
  ok.


generateDice() ->
  {A, B, C} = now(),
  random:seed(A, B, C),
  [random:uniform(6) || _ <- lists:seq(1, 15)].


