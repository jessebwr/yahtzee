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
                p1RollNum = 1,
                p2RollNum = 1,
                p1Win = 0,
                p2Win = 0,
                currentGame = 1
                }).

-record(tournament, {bracket = [],
                     listOfPlayers = [],
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

      handle_gone(Username),

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
      % cancel timer
      [{_, TimeOutRef}] = ets:lookup(?TimeOutRefs, {Username, Tid}),
      ets:delete(?TimeOutRefs, {Username, Tid}),
      timer:cancel(TimeOutRef),

      [{Tid, T}] = ets:lookup(?TournamentInfo, Tid),
      case T#tournament.started of
        true ->
          {noreply, S};
        false ->

          % Since they accepted, add them to the dictionary of players
          NewListOfPlayers = T#tournament.listOfPlayers ++ [Username],

          % If we have enough players, start the tournament
          NumPlayersReplied = T#tournament.numPlayersReplied + 1,
          case NumPlayersReplied == T#tournament.numNeededReplies of
            true ->
              NewTM = T#tournament{listOfPlayers = NewListOfPlayers,
                                   numPlayersReplied = NumPlayersReplied,
                                   started = true},
              ets:insert(?TournamentInfo, {Tid, NewTM}),
              start_tournament(Tid, NewTM),
              {noreply, S};

            false ->
              NewTM = T#tournament{numPlayersReplied = NumPlayersReplied,
                                   listOfPlayers = NewListOfPlayers},
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
      %% Cancel the timeout
      [{_, TimeOutRef}] = ets:lookup(?TimeOutRefs, {Username, Tid}),
      ets:delete(?TimeOutRefs, {Username, Tid}),
      timer:cancel(TimeOutRef),

      %% Get the global tournament
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



handle_info({ play_action, Pid, Username, {Ref, Tid, Gid, RollNum, DiceToKeep, ScorecardLine} }, S) ->
  



%%%%%%%%%%%%%%%%%% END RECEIVING MESSAGES SENT BY PLAYER %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%%%%%

handle_info({request_tournament, Pid, {NumPlayers, GamesPerMatch}}, S)
  when (NumPlayers rem 2) == 1 ->
  Tid = make_ref(),

  % Getting Ets Table Keys i.e. Usernames
  PlayerList = lists:flatten(ets:match(?CurrentPlayerLoginInfo, {'$1', '_'})),

  {NumChosenPlayers, ChosenPlayers} = shuffle(PlayerList, NumPlayers),

  NewTournament = #tournament{ numNeededReplies = NumChosenPlayers,
                               gamesPerMatch = GamesPerMatch,
                               pidThatRequested = Pid },

  % Ask each of the players
  lists:foreach(fun(X) -> 
                    [{X, XInfo}] = ets:lookup(?CurrentPlayerLoginInfo, X),
                    handle_ask_player(X, XInfo, Tid) 
              end, ChosenPlayers),

  % inserting the tournament info
  ets:insert(?TournamentInfo, {Tid, NewTournament}),
  {noreply, S};


handle_info({tournament_info, Pid, Tid}, S) ->
  case ets:lookup(?TournamentInfo, Tid) of
    [{Tid, T}] ->
      Status = T#tournament.status,
      Winner = T#tournament.winner,
      Pid ! {tournament_status, self(), {Tid, Status, Winner, blah}},
      {noreply, S};
    _Else ->
      % Tournament Doesnt exist...
      {noreply, S}
  end;

handle_info({user_info, Pid, Username}, S) ->
  case ets:lookup(?UserInfo, Username) of
    [{Username, User}] ->
      Wins = User#user.match_wins,
      Losses = User#user.match_losses,
      Tournaments_played = User#user.tournaments_played,
      Tournaments_won = User#user.tournaments_won,
      Pid ! {user_status, self(), {Username, Wins, Losses, Tournaments_played, Tournaments_won}},
      {noreply, S}
    _Else ->
      % User isn't registered
      {noreply, S};
  end;

%%%%%%%%%%%%%%%% END RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Handling Players Dying %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({'DOWN', MonitorRef,_,Pid,_}, S) ->
  demonitor(MonitorRef, [flush]),
  % Look up the username by pattern matching on the record stored in the 
  % ets table CurrentPlayerLoginInfo.
  Username = hd( ets:match( ?CurrentPlayerLoginInfo, {'$1', {Pid, '_', '_'} } ) ),
  handle_gone( Username ).


handle_info(MSG, S) ->
  io:format("not recognized message: ~p~n", [MSG]),
  {noreply, S}.

handle_gone(Username) ->
  UserMatchesP1 = ets:match( ?MatchTable, { {'$1', '$2'}, #match{p1 = Username} }),
  UserMatchesP2 = ets:match( ?MatchTable, { {'$1', '$2'}, #match{p2 = Username} }),
  lists:foreach(fun( [Tid, Gid] ) -> handle_gone_game(Tid, Gid, Username, 1) end, UserMatchesP1),
  lists:foreach(fun( [Tid, Gid] ) -> handle_gone_game(Tid, Gid, Username, 2) end, UserMatchesP2).



handle_gone_game(Tid, Gid, Username, 1) ->
  [{_Key, M}] = ets:lookup(?MatchTable, {Tid, Gid}),
  ets:delete(?MatchTable, {Tid, Gid} ),

  P1 = M#match.p1,
  P2 = M#match.p2,
  NewCurrentGame = M#match.currentGame + 1,
  GamesPerMatch = M#match.gamesPerMatch,
  P1Win = M#match.p1Win,
  P2Win = M#match.p2Win + 1,

  case P2Win > (GamesPerMatch / 2) of
    true ->
      % [{Tid, T}] = ets:lookup(?TournamentInfo, Tid),
      % [{Username, PrevUserData}] = ets:lookup(?UserInfo, Username),
      % %% Update that you lost the tournament and the match

      % PrevTournamentsPlayed = PrevUserData#user.tournaments_played,
      % NewUserData = PrevUserData#user{match_losses = PrevMatchLosses + 1,
      %                                 tournaments_played = PrevTournamentsPlayed + 1},
      % ets:insert(?UserInfo, {Username, NewUserData}),

      handle_match_over(Tid, P2, P1);

    false ->
      %% Just make them lose the game
      NewGid = make_ref(),
      NewMatch = #match{p1 = P1,
                        p2 = P2,
                        currentGame = NewCurrentGame,
                        p2Win = P2Win,
                        p1Win = P1Win},

      ets:insert(?MatchTable, {{Tid, NewGid}, NewMatch}),
      {ok, TimerRef} = timer:send_after(60000, {Tid, NewGid, P2, P1}),
      ets:insert(?TimeOutRefs, {P1, TimerRef})
  end;

handle_gone_game(Tid, Gid, Username, 2) ->
  [{_Key, M}] = ets:lookup(?MatchTable, {Tid, Gid}),
  ets:delete(?MatchTable, {Tid, Gid} ),

  P1 = M#match.p1,
  P2 = M#match.p2,
  NewCurrentGame = M#match.currentGame + 1,
  GamesPerMatch = M#match.gamesPerMatch,
  P1Win = M#match.p1Win + 1,
  P2Win = M#match.p2Win,

  case P1Win > (GamesPerMatch / 2) of
    true ->
      handle_match_over(Tid, P1, P2);

    false ->
      %% Just make them lose the game
      NewGid = make_ref(),
      NewMatch = #match{p1 = P1,
                        p2 = P2,
                        currentGame = NewCurrentGame,
                        p2Win = P2Win,
                        p1Win = P1Win},

      ets:insert(?MatchTable, {{Tid, NewGid}, NewMatch}),
      {ok, TimerRef} = timer:send_after(60000, {Tid, NewGid, P2, P1}),
      ets:insert(?TimeOutRefs, {P2, TimerRef})
  end.


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
  ListOfPlayers = T#tournament.listofPlayers,
  NumPlayers = length( ListOfPlayers ),
  NumByesNeeded = utils:nextPow2(NumPlayers) - NumPlayers,
  RoundOne = create_initial_bracket( ListOfPlayers, [], NumByesNeeded ),
  Bracket = initialize_later_rounds( RoundOne, [], 1, utils:log2( length(RoundOne) ) ),
  create_matches( Bracket ),  % Create first round matches and advance players whose
                              % opponent is 'bye'
  create_matches( tl(Bracket) ),  % Create second round matches
  Matches = ets:matches( ?MatchTable, { {Tid, '$1'}, $2 } ),
  NewT = T#tournament{ started = true,
                       bracket = Bracket },
  start_matches( Tid, Matches ),
  T#tournament.pidThatRequested ! {tournament_started, self(), {Tid, PlayerList, blah}}.



create_initial_bracket( [], BracketSoFar, _NumByesNeeded ) ->
  BracketSoFar;

create_initial_bracket( [NextPlayer | RestPlayers], BracketSoFar, 0 ) ->
  create_initial_bracket( RestPlayers, [NextPlayer | BracketSoFar], 0 );

create_initial_bracket( Players, BracketSoFar, NumByesNeeded )
  when length( BracketSoFar ) rem 2 == 1 ->
  create_initial_bracket( Players, [bye | BracketSoFar], NumByesNeeded - 1 );

create_initial_bracket( [NextPlayer | RestPlayers], BracketSoFar, NumByesNeeded )
  when length( BracketSoFar ) rem 2 == 0 ->
  create_initial_bracket( RestPlayers, [NextPlayer | BracketSoFar], NumByesNeeded ).



initialize_later_rounds( Bracket, LaterRounds, NumRounds, NumRounds ) ->
  [ Bracket | LaterRounds ];

initialize_later_rounds( Bracket, LaterRounds, CurrentRoundSize, MaxRounds ) 
  when CurrentRoundSize < MaxRounds ->
  NextRound = lists:duplicate( math:pow(2, CurrentRoundSize), none ),
  initialize_later_rounds( Bracket, [NextRound | LaterRounds], CurrentRoundSize + 1, MaxRounds).


create_matches( [ [] | _Rest ], _CurrMatchInd, _Tid ) ->
  ok;

create_matches( [ [bye, SecondPlayer | RestPlayers], RoundTwo | RestRounds ],
  CurrMatchInd, Tid ) ->
  NewRoundTwo = utils:set_list_index( RoundTwo, CurrMatchInd, SecondPlayer ),
  create_matches( [RestPlayers, NewRoundTwo | RestRounds], CurrMatchInd + 1, Tid );

create_matches( [ [FirstPlayer, SecondPlayer | RestPlayers], RoundTwo | RestRounds ], Tid ) ->
  GameRef = make_ref(),
  ets:insert(?MatchTable, {{Tid, GameRef}, #match{ p1 = FirstPlayer,
                                                   p2 = SecondPlayer,
                                                   }} ),
  create_matches( [RestPlayers, RoundTwo | RestRounds], Tid ).



start_matches( Tid, [] ) ->
  ok;

start_matches( Tid, [ [Gid, NextMatch] | RestMatches ] ) ->
  Dice = generateDice(),
  NewMatch = NextMatch#match{ p1ListOfDice = Dice,
                              p2ListOfDice = Dice },
  ets:insert( ?MatchTable, {{Tid, Gid}, NewMatch} ),
  sendDice( Tid, Gid, NextMatch, 5, 5 ),
  start_matches( Tid, RestMatches ).
  


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

  %{KEY, VALUE}
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


