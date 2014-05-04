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
-export([main/1,
	 shuffle/2]).

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
                p1RollNum = 1, %% Which roll number we are on
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
    io:format(utils:timestamp() ++ ": starting network kernel with node name ~p~n", [node()]),
    gen_server:start({local, ?MODULE}, ?MODULE, {}, []).




%%%============================================================================
%%% GenServer Calls/Casts
%%%============================================================================

%%ets:insert(TabId, {Key, Value})

%%%============================================================================
%%% GenServer Callbacks
%%%============================================================================

init({}) ->
    %% UserInfo will hold 
    %% Username: user record (see above)
    ets:new(?UserInfo, [set, protected, named_table]),

    %% TournamentInfo will hold tid : #tournament
    ets:new(?TournamentInfo, [set, protected, named_table]),

    %% Just some timeout references
    ets:new(?TimeOutRefs, [set, protected, named_table]),

    %% {Tid, Gid} : Match
    ets:new(?MatchTable, [set, protected, named_table]),

    %% A dictionary of Username:{Pid, MonitorRef, LoginTicket}
    ets:new(?CurrentPlayerLoginInfo, [set, protected, named_table]),

    io:format(utils:timestamp() ++ ": all ets tables succesfully initialized~n"),


    {ok, #tm{}}.

handle_call(_, _, S) ->
    {reply, ok, S}.


handle_cast(_, S) ->
    {noreply, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% RECEIVING MESSAGES SENT BY PLAYER %%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec handle_info({login, Pid, Username, {Username, Password}}, S)
%%                               -> {noreply, S};
%% @doc This message is received from a player and is used to log into the 
%% tournament manager
handle_info({login, Pid, Username, {Username, Password}}, S) ->
    io:format(utils:timestamp() ++ ": received login message from ~p~n", [Username]),
    LoginTicket = make_ref(),

    case ets:lookup(?UserInfo, Username) of
	[] ->
	    io:format(utils:timestamp() ++ ": ~p is a new player, now logging them in~n", [Username]),
	    %% Initializing the user in our record database
	    ets:insert(?UserInfo, {Username, #user{password = Password}}),

	    %% Monitoring it and setting up its current info
	    MonitorRef = monitor(process, Pid),
	    ets:insert(?CurrentPlayerLoginInfo, {Username, {Pid, MonitorRef, LoginTicket}}),

	    %% Messaging that they are logged in.
	    Pid ! {logged_in, self(), Username, LoginTicket},
	    {noreply, S};

	[{Username, {Password, _, _, _, _}}] ->
	    io:format(utils:timestamp() ++ ": ~p has logged in before, now logging them back in~n", [Username]),

	    %% Monitoring it and stting up its current info
	    MonitorRef = monitor(process, Pid),
	    ets:insert(?CurrentPlayerLoginInfo, {Username, {Pid, MonitorRef, LoginTicket}}),

	    %% Messaging that they are logged in
	    Pid ! {logged_in, self(), Username, LoginTicket},
	    {noreply, S};

	%% They didn't give the right password
	[{Username, {_DiffPassword, _, _, _, _}}] ->
	    io:format(utils:timestamp() ++ ": Incorrect password entered!~n"),
	    Pid ! {incorrect_password, Username},
	    {noreply, S}
    end;


%% @spec handle_info({logout, Pid, Username, LoginTicket}, S)
%%                               -> {noreply, S};
%% @doc This message is received from a player who wants to log out
%% a player with the username Username
handle_info({logout, Pid, Username, LoginTicket}, S) ->
    io:format(utils:timestamp() ++ ": received logout message from ~p~n", [Username]),
    [{Username, {_Pid, MonitorRef, TrueLoginTicket}}] = ets:lookup(?CurrentPlayerLoginInfo, Username),

    case LoginTicket == TrueLoginTicket of
	true ->
	    %% Demonitoring
	    demonitor(MonitorRef,[flush]),

	    handle_gone(Username),

	    %% Deleting this user's current session information
	    ets:delete(?CurrentPlayerLoginInfo, Username),
	    {noreply, S};

	%% Someone else was trying to log them out...
	false ->
	    io:format(utils:timestamp() ++ ": Warning! Logout message sent from incorrect source~n"),
	    Pid ! {incorrect_login_ticket, Username},
	    {noreply, S}
    end;


%% @doc This message indicated that the player is willing to play in the
%% specified tournament

handle_info({accept_tournament, Pid, Username, {Tid, LoginTicket}}, S) ->
    io:format(utils:timestamp() ++ ": accept_tournament message received from ~p~n", [Username]),
    [{Username, {_Pid, _MonitorRef, TrueLoginTicket}}] = ets:lookup(?CurrentPlayerLoginInfo, Username),
    case LoginTicket == TrueLoginTicket of
	true ->
	    %% cancel timer
	    [{_, TimeOutRef}] = ets:lookup(?TimeOutRefs, {Username, Tid}),
	    ets:delete(?TimeOutRefs, {Username, Tid}),
	    timer:cancel(TimeOutRef),

	    [{Tid, T}] = ets:lookup(?TournamentInfo, Tid),
	    case T#tournament.started of
		true ->
		    {noreply, S};

		false ->
		    %% Since they accepted, add them to the dictionary of players
		    NewListOfPlayers = T#tournament.listOfPlayers ++ [Username],

		    %% If we have enough players, start the tournament
		    NumPlayersReplied = T#tournament.numPlayersReplied + 1,
		    case NumPlayersReplied == T#tournament.numNeededReplies of
			true ->
			    io:format(utils:timestamp() ++ ": enough players have replied... starting the tournament~n"),
			    NewTM = T#tournament{listOfPlayers = NewListOfPlayers,
						 numPlayersReplied = NumPlayersReplied,
						 started = true},
			    ets:insert(?TournamentInfo, {Tid, NewTM}),
			    io:format(utils:timestamp() ++ ": calling start_tournament~n"),
			    start_tournament(Tid, NewTM),
			    {noreply, S};

			false ->
			    io:format(utils:timestamp() ++ ": we still need more players to reply before starting the tournament~n"),
			    NewTM = T#tournament{numPlayersReplied = NumPlayersReplied,
						 listOfPlayers = NewListOfPlayers},
			    ets:insert(?TournamentInfo, {Tid, NewTM}),
			    {noreply, S}
		    end
	    end;
	false -> 
	    io:format(utils:timestamp() ++ ": incorrect login ticket received from ~p~n", [Username]),
	    Pid ! {incorrect_login_ticket, Username},
	    {noreply, S}
    end;



%% @spec handle_info({rejectTournament, Pid, Username, {Tid, LoginTicket}}, S)
%%                               -> {noreply, S};
%% @doc This message indicated that the player is not willing to play in the
%% specified tournament

handle_info({reject_tournament, Pid, Username, {Tid, LoginTicket}}, S) ->
    io:format(utils:timestamp() ++ ": reject_tournament message received from ~p~n", [Username]),
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

		    %% If we have enough replies, start the tournament
		    case NumPlayersReplied == T#tournament.numNeededReplies of
			true ->
			    io:format(utils:timestamp() ++ ": enough players have replied... starting the tournament~n"),
			    NewTM = T#tournament{numPlayersReplied = NumPlayersReplied,
						 started = true},
			    start_tournament(Tid, NewTM),
			    {noreply, S};
			false ->
			    io:format(utils:timestamp() ++ ": we still need more players to reply before starting the tournament~n"),
			    NewTM = T#tournament{numPlayersReplied = NumPlayersReplied},
			    ets:insert(?TournamentInfo, {Tid, NewTM}),
			    {noreply, S}
		    end
	    end;
	false ->
	    Pid ! {incorrect_login_ticket, Username},
	    {noreply, S}
    end;



%% Handle a non-scoring message, where scorecard-line is 0
handle_info({ play_action, Pid, Username, {Ref, Tid, Gid, RollNum, DiceToKeep, 0} }, S) ->
    io:format(utils:timestamp() ++ ": received play_action message from ~p with the following info~nTid: ~p~n Gid: ~p~nRollNum: ~p~n DiceToKeep: ~p~nTid: ~p~n Scorecard Line: 0~n",
	      [Username, Tid, Gid, RollNum, DiceToKeep]),

    case ets:lookup(?MatchTable, {Tid, Gid}) of
	[] ->
	    %% Invalid game that doesn't exist....  Ignore it since the protocol
	    %% doesn't specify any action
	    {noreply, S};
	[{Tid, Gid}, M] ->
	    P1 = M#match.p1,
	    P1RollNum = M#match.p1RollNum,

	    P2 = M#match.p2,
	    P2RollNum = M#match.p2RollNum,
	    case Username of
		P1 ->
		    case RollNum of
			P1RollNum when RollNum =< 3, RollNum > 0 ->
			    {NewDiceList, NewDiceToSend} = 
				updateDiceList( M#match.p1ListOfDice, DiceToKeep ),
			    NewMatch = M#match{p1RollNum = RollNum + 1,
					       p1ListOfDice = NewDiceList},
			    ets:insert(?MatchTable, {{Tid, Gid}, NewMatch}),
			    Pid ! {play_request, self(), Username,
				   {Ref, Tid, Gid, RollNum + 1, NewDiceToSend,
				    M#match.p1ScoreCard, M#match.p2ScoreCard}};
			_ ->
			    kick_out_cheater( Username )
		    end;
		P2 ->
		    case RollNum of
			P2RollNum when RollNum =< 3, RollNum > 0 ->
			    {NewDiceList, NewDiceToSend} = 
				updateDiceList( M#match.p2ListOfDice, DiceToKeep ),
			    NewMatch = M#match{p2RollNum = RollNum + 1,
					       p2ListOfDice = NewDiceList},
			    ets:insert(?MatchTable, {{Tid, Gid}, NewMatch}),
			    Pid ! {play_request, self(), Username,
				   {Ref, Tid, Gid, RollNum + 1, NewDiceToSend,
				    M#match.p2ScoreCard, M#match.p1ScoreCard}};
			_ ->
			    kick_out_cheater( Username )
		    end;
		_ -> %% Ignore this mystery user
		    io:format(utils:timestamp() ++ ": not recognized message: ~p~n")
	    end,
	    {noreply, S}
    end;


%% %% Scoring move!  :(
%% %% We don't care about DiceToKeep, since you can't reroll blindly (that is,
%% %% you can't say "score it in this box after rerolling these dice" as a single
%% %% move) and don't care about RollNum since we rset it here anyway
%% handle_info({ play_action, Pid, Username, {Ref, Tid, Gid, RollNum, DiceToKeep, ScoreCardLine} }, S) ->
%%     io:format(utils:timestamp() ++ ": received play_action message from ~p 
%%                                 with the following info~nTid: ~p~n
%% 	      Gid: ~p~nRollNum: ~p~n
%% 	      DiceToKeep: ~p~nTid: ~p~n
%% 	      Scorecard Line: ~p~n",
%%                         [Username, Tid, Gid, RollNum, DiceToKeep, ScoreCardLine]),
%%     case ets:lookup(?MatchTable, {Tid, Gid}) of
%% 	[] ->
%% 	    %% Invalid game that doesn't exist....  Ignore it since the protocol
%% 	    %% doesn't specify any action
%% 	    {noreply, S};
%% 	[{Tid, Gid}, M] ->
%% 	    P1 = M#match.p1,
%% 	    P2 = M#match.p2,

%% 	    case Username of
%% 		P1 ->
%% 		    NewScoreCard = updateScoreCard( M#match.p1ScoreCard,
%% 						    ScoreCardLine,
%% 						    M#match.p1ListOfDice ),
%% 		    NewMatch = M#match{p1ScoreCard = NewScoreCard,
%% 				       p1RollNum = 0},
%% 		    ets:insert(?MatchTable, {{Tid, Gid}, NewMatch});
%% 		P2 ->
%% 		    NewScoreCard = updateScoreCard( M#match.p2ScoreCard,
%% 						    ScoreCardLine,
%% 						    M#match.p2ListOfDice ),
%% 		    NewMatch = M#match{p2ScoreCard = NewScoreCard,
%% 				       p2RollNum = 0},
%% 		    ets:insert(?MatchTable, {{Tid, Gid}, NewMatch})
%% 	    end
%%     end,
%%     NewMatch = ets:lookup(?MatchTable, {Tid, Gid}),
%%     case NewMatch of
%% 	#match{p1RollNum = 0, p2RollNum = 0, p1Win = P1Win, p2Win = P2Win} ->
%% 	    %% Yup, the game ended.  We should determine the winner, etc.
%% 	    Tournament = ets:lookup(?TournamentInfo, Tid),
%% 	    MaxGames = Tournament#tournament.gamesPerMatch,
%% 	    case NewMatch#match.{p1ScoreCard = P1ScoreCard, 
%% 				 p2ScoreCard = P2ScoreCard} of
%% 		_ when scoreFullCard(P1ScoreCard) > scoreFullCard(P2ScoreCard)
%% 			  -> 
%% 		    %% Now check if Player 1 won the match
%% 		    case P1Win + 1 > MaxGames / 2 of
%% 			true ->
%% 			    % Yup, the match is over, Player 1 won.
%% 			    ;
%% 			false ->
%% 			    % Nope, start the next game.
%% 			    ets:delete(?MatchTable, {Tid, Gid}),
%% 			    NewGid = make_ref(),
%% 			    UpdatedMatch = newGameInMatch( NewMatch, 1 ),
%% 			    ets:insert(?MatchTable, {{Tid, NewGid}, UpdatedMatch}),
%% 			    sendDice( Tid, NewGid, UpdatedMatch, 5, 5 )
%% 		    end;
%% 		_ when scoreFullCard(P2ScoreCard) > scoreFullCard(P1ScoreCard)
%% 		       ->
%% 		    %% Now check if Player 2 won the match
%% 		    case P2Win + 1 > MaxGames / 2 of
%% 			true ->
%% 			    % Yup, the match is over, Player 2 won.
%% 			    ;
%% 			false ->
%% 			    % Nope, start the next game.
%% 			    ets:delete(?MatchTable, {Tid, Gid}),
%% 			    NewGid = make_ref(),
%% 			    UpdatedMatch = newGameInMatch( NewMatch, 2 ),
%% 			    ets:insert(?MatchTable, {{Tid, NewGid}, UpdatedMatch}),
%% 			    sendDice( Tid, NewGid, UpdatedMatch, 5, 5 )
%% 		    end;    
%% 		_ ->
%% 		    %% OH NO A TIE
%% 	    end;
%% 	_ ->
%% 	    %% Nope, haven't heard back from both players yet
%% 	    {noreply, S}
%%     end;
	



%%%%%%%%%%%%%%%% END RECEIVING MESSAGES SENT BY PLAYER %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%

handle_info({request_tournament, Pid, {NumPlayers, GamesPerMatch}}, S) when (GamesPerMatch rem 2) == 1,
                                                                        NumPlayers > 0 ->
    io:format(utils:timestamp() ++ ": received request_tournament message from ~p
                                    for a tournament with ~p players and ~p games~n",
                                     [Pid, NumPlayers, GamesPerMatch]),
    Tid = make_ref(),

    %% Getting Ets Table Keys i.e. Usernames
    PlayerList = ets:match(?CurrentPlayerLoginInfo, {'$1', '_'}),

    {NumChosenPlayers, ChosenPlayers} = shuffle(PlayerList, NumPlayers),

    NewTournament = #tournament{ numNeededReplies = NumChosenPlayers,
				 gamesPerMatch = GamesPerMatch,
				 pidThatRequested = Pid },

    %% Ask each of the players
    io:format(utils:timestamp() ++ ": asking players if they want to join the tournament~n"),
    lists:foreach(fun(X) -> 
        Player = hd(X),
        io:format("~p~n", [Player]),
			  [{PlayerName, PlayerInfo}] = ets:lookup(?CurrentPlayerLoginInfo, Player),
			  handle_ask_player(PlayerName, PlayerInfo, Tid) 
		  end, ChosenPlayers),

    %% inserting the tournament info
    ets:insert(?TournamentInfo, {Tid, NewTournament}),
    {noreply, S};


handle_info({tournament_info, Pid, Tid}, S) ->
    io:format(utils:timestamp() ++ ": received tournament_info message from ~p~n", [Pid]),

    case ets:lookup(?TournamentInfo, Tid) of
	[{Tid, T}] ->
	    Status = T#tournament.status,
	    Winner = T#tournament.winner,
	    Pid ! {tournament_status, self(), {Tid, Status, Winner, blah}},
	    {noreply, S};
	_Else ->
	    %% Tournament Doesnt exist...
	    {noreply, S}
    end;

handle_info({user_info, Pid, Username}, S) ->
    io:format(utils:timestamp() ++ ": received user_info message from ~p~n", [Pid]),
    case ets:lookup(?UserInfo, Username) of
	[{Username, User}] ->
	    Wins = User#user.match_wins,
	    Losses = User#user.match_losses,
	    Tournaments_played = User#user.tournaments_played,
	    Tournaments_won = User#user.tournaments_won,
	    Pid ! {user_status, self(), {Username, Wins, Losses, Tournaments_played, Tournaments_won}},
	    {noreply, S};
	_Else ->
	    %% User isn't registered
	    {noreply, S}
    end;

%%%%%%%%%%%%%%% END RECIEVING MESSAGES FROM THE OUTSIDE WORLD %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Handling Players Dying %%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({'DOWN', MonitorRef,_,Pid,_}, S) ->
    demonitor(MonitorRef, [flush]),
    %% Look up the username by pattern matching on the record stored in the 
    %% ets table CurrentPlayerLoginInfo.
    Username = hd( ets:match( ?CurrentPlayerLoginInfo, {'$1', {Pid, '_', '_'} } ) ),
    io:format(utils:timestamp() ++ ": received DOWN message from ~p" ++
		  "with Pid ~p~n", [Username, Pid]),
    handle_gone( Username ),
    {noreply, S};


handle_info(MSG, S) ->
    io:format(utils:timestamp() ++ ": not recognized message: ~p~n", [MSG]),
    {noreply, S}.

%% TODO: actually implement
kick_out_cheater( Username ) ->
    %% Kick them out of all active matches
    ok.

handle_gone(Username) ->
    UserMatchesP1 = ets:match( ?MatchTable, { {'$1', '$2'}, #match{p1 = Username} }),
    UserMatchesP2 = ets:match( ?MatchTable, { {'$1', '$2'}, #match{p2 = Username} }),
    lists:foreach(fun( [Tid, Gid] ) -> handle_gone_game(Tid, Gid, Username, 1) end, UserMatchesP1),
    lists:foreach(fun( [Tid, Gid] ) -> handle_gone_game(Tid, Gid, Username, 2) end, UserMatchesP2).


handle_gone_game(Tid, Gid, _Username, 1) ->
    [{_Key, M}] = ets:lookup(?MatchTable, {Tid, Gid}),
    ets:delete(?MatchTable, {Tid, Gid} ),
    [{_, T}] = ets:lookup(?TournamentInfo, Tid),

    P1 = M#match.p1,
    P2 = M#match.p2,
    NewCurrentGame = M#match.currentGame + 1,
    GamesPerMatch = T#tournament.gamesPerMatch,
    P1Win = M#match.p1Win,
    P2Win = M#match.p2Win + 1,

    case P2Win > (GamesPerMatch / 2) of
    	true ->
    	    handle_match_over(Tid, Gid, P2, P1);

    	false ->
    	    %% Just make them lose the game
    	    NewGid = make_ref(),
    	    NewMatch = #match{p1 = P1,
    			      p2 = P2,
    			      currentGame = NewCurrentGame,
    			      p2Win = P2Win,
    			      p1Win = P1Win},

    	    ets:insert(?MatchTable, {{Tid, NewGid}, NewMatch}),
    	    {ok, TimerRef} = timer:send_after(60000, {Tid, NewGid, P2, P1}), %Make Handler for this
    	    ets:insert(?TimeOutRefs, {P1, TimerRef})
    end;

handle_gone_game(Tid, Gid, _Username, 2) ->
    [{_Key, M}] = ets:lookup(?MatchTable, {Tid, Gid}),
    ets:delete(?MatchTable, {Tid, Gid} ),
    [{_, T}] = ets:lookup(?TournamentInfo, Tid),

    P1 = M#match.p1,
    P2 = M#match.p2,
    NewCurrentGame = M#match.currentGame + 1,
    GamesPerMatch = T#tournament.gamesPerMatch,
    P1Win = M#match.p1Win + 1,
    P2Win = M#match.p2Win,

    case P1Win > (GamesPerMatch / 2) of
	true ->
	    handle_match_over(Tid, Gid, P1, P2);

	false ->
	    %% Just make them lose the game
	    NewGid = make_ref(),
	    NewMatch = #match{p1 = P1,
			      p2 = P2,
			      currentGame = NewCurrentGame,
			      p2Win = P2Win,
			      p1Win = P1Win},

	    ets:insert(?MatchTable, {{Tid, NewGid}, NewMatch}),
	    {ok, TimerRef} = timer:send_after(60000, {Tid, NewGid, P2, P1}), % If it times out, we need P2 to win the match
	    ets:insert(?TimeOutRefs, {P2, TimerRef})
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    io:format(utils:timestamp() ++ ": terminate reason: ~p~n", [Reason]).






%%%============================================================================
%%% Other (possibly) useful functions
%%%============================================================================



%% @spec shuffle(List, K) -> {Num, Chosen}
%% @doc Given a list List and an integer K, return a tuple
%% whose second value is a list Chosen with the shuffled values of
%% those in the original list, and the length Num of this list which is
%% K if Num <= the size of the original list, or else the size of the
%% original list
shuffle(List, K) ->
    {A, B, C} = now(),
    random:seed(A,B,C),
    Shuffled = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
    Chosen = lists:sublist(Shuffled, K),
    Num = length(Chosen),
    {Num, Chosen}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Tournament Bracket Code %%%%%%%%%%%%%%%%%%%%%%%%%%

start_tournament(Tid, T) ->
    ListOfPlayers = T#tournament.listOfPlayers,
    NumPlayers = length( ListOfPlayers ),
    NumByesNeeded = utils:nextPow2(NumPlayers) - NumPlayers,

    %% Pair off players according to a fixed single-elimination tournament
    %% bracket.
    RoundOne = create_initial_bracket( ListOfPlayers, [], NumByesNeeded ),    

    %% Once the players are paired off and the entire tournament bracket is
    %% created, we can handle bye-paired matches by advancing the real player
    %% to the next round, and starting matches between actual players.
    %% However, if there are two or fewer total players in the tournament,
    %% there exists only one round so we have to handle that separately.
    OnlyOneRound = NumPlayers =< 2,
    io:format( utils:timestamp() ++ ": Starting to make UpdatedBracket; OnlyOneRound is ~p~n", [OnlyOneRound] ),
    UpdatedBracket = if 
			 OnlyOneRound ->
			     create_single_round_match([RoundOne], Tid);
			 not OnlyOneRound ->
			     Bracket = initialize_later_rounds( RoundOne, [], 0, utils:log2( length(RoundOne) ) ),
			     create_matches( Bracket, 0, Tid, RoundOne)
		     end,
    
    %% Regardless of the number of players in the tournament, every real match
    %% got added to the match table, so now we can start those matches properly.
    io:format("UpdatedBracket: ~p~n", [UpdatedBracket]),
    Matches = ets:match( ?MatchTable, { {Tid, '$1'}, '$2' } ),
    io:format( utils:timestamp() ++ ": Matches are: ~p~n", [Matches] ),
    NewT = T#tournament{ started = true,
			 bracket = UpdatedBracket },
    ets:insert(?TournamentInfo, {Tid, NewT}),
    io:format( utils:timestamp() ++ ": Calling start_matches~n" ),
    start_matches( Tid, Matches ),
    T#tournament.pidThatRequested ! {tournament_started, self(), {Tid, ListOfPlayers, blah}}.



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
    NextRound = lists:duplicate( round(math:pow(2, CurrentRoundSize)), none ),
    initialize_later_rounds( Bracket, [NextRound | LaterRounds], CurrentRoundSize + 1, MaxRounds).

%% @spec handle_match_over(Tid, Gid, Winner, Loser) -> none()
%% @doc Winner and Loser are both players. 
%% Once a player has won, or because the other player crashed and
%% didn't log back in in time, update each player's wins and losses,
%% delete the old match, update the tournament bracket, and
%% create and send out the messages for a new match
handle_match_over(Tid, Gid, Winner, Loser) ->
    % update wins and losses
    [{Winner, WinnerInfo}] = ets:lookup(?UserInfo, Winner),
    [{Loser, LoserInfo}] = ets:lookup(?UserInfo, Loser),
    ets:insert(?UserInfo, {Winner, WinnerInfo#user{match_wins =
                                           WinnerInfo#user.match_wins + 1}}),
    ets:insert(?UserInfo, {Loser, LoserInfo#user{match_losses =
                                           LoserInfo#user.match_losses + 1}}),

    % delete the old match
    ets:delete(?MatchTable, {Tid, Gid}),

    % update tournament bracket with new winner
    NewBracket = updateTournamentBracket(Tid, Winner),

    % make new match and send dice
    NewGid = make_ref(),
    ets:insert(?MatchTable, {{Tid, NewGid}, #match{ }}). % shit, we need the new player from updating the brackets...

%% @spec updateTournamentBracket(Tid, Winner) -> NewBracket
%% @doc updates the structure of the tournament bracket
%% as a result of a match ending. The old round must have
%% the two player's positions set to none, and the
%% new round must have the new player's username in the
%% correct position
updateTournamentBracket(Tid, Winner) ->
    [].


create_matches( [ [] | Rest ], _CurrMatchInd, _Tid, RoundOne) ->
    [RoundOne | Rest];

create_matches( [ [bye, SecondPlayer | RestPlayers], RoundTwo | RestRounds ],
		CurrMatchInd, Tid, RoundOne) ->
    NewRoundTwo = utils:set_list_index( RoundTwo, CurrMatchInd, SecondPlayer ),
    create_matches( [RestPlayers, NewRoundTwo | RestRounds], CurrMatchInd + 1, Tid, RoundOne);

create_matches( [ [FirstPlayer, SecondPlayer | RestPlayers], RoundTwo | RestRounds ],
		CurrMatchInd, Tid, RoundOne) ->
    GameRef = make_ref(),
    ets:insert(?MatchTable, {{Tid, GameRef}, #match{ p1 = FirstPlayer,
						     p2 = SecondPlayer}}),
    create_matches( [RestPlayers, RoundTwo | RestRounds], CurrMatchInd + 1, Tid, RoundOne).


create_single_round_match( Bracket = [[bye, _PlayerTwo]], Tid ) ->
    Bracket;

create_single_round_match( Bracket = [[_PlayerOne, bye]], Tid ) ->
    Bracket;

create_single_round_match( [[PlayerOne, PlayerTwo]], Tid ) ->
    GameRef = make_ref(),
    ets:insert(?MatchTable, {{Tid, GameRef}, #match{ p1 = PlayerOne,
						     p2 = PlayerTwo }}),
    [[PlayerOne, PlayerTwo]].
    
    
    


start_matches( _, [] ) ->
    ok;

start_matches( Tid, [ [Gid, NextMatch] | RestMatches ] ) ->
    Dice = generateDice(),
    NewMatch = NextMatch#match{ p1ListOfDice = Dice,
				p2ListOfDice = Dice },
    ets:insert( ?MatchTable, {{Tid, Gid}, NewMatch} ),
    sendDice( Tid, Gid, NewMatch, 5, 5 ),
    start_matches( Tid, RestMatches ).



sendDice(Tid, Gid, M, NumDiceToSendP1, NumDiceToSendP2) ->
    P1 = M#match.p1,
    P2 = M#match.p2,

    [{P1, {Pid1, _MonitorRef1, _LoginTicket1}}] = ets:lookup(?CurrentPlayerLoginInfo, P1),
    [{P2, {Pid2, _MonitorRef2, _LoginTicket2}}] = ets:lookup(?CurrentPlayerLoginInfo, P2),

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

    %%{KEY, VALUE}
    Pid1 ! {play_request, self(), P1, P1Msg},
    Pid2 ! {play_request, self(), P2, P2Msg}.





handle_ask_player(ChosenPlayer, {Pid, _MonitorRef, LoginTicket}, Tid) ->
    Pid ! {start_tournament, self(), ChosenPlayer, Tid},
    {ok, TimerRef} = timer:send_after(60000, {reject_tournament, Pid, ChosenPlayer, {Tid, LoginTicket}}),
    ets:insert(?TimeOutRefs, {{ChosenPlayer, Tid}, TimerRef}),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actually score a turn! %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a wrapper that checks for a Yahtzee Bonus and scores it, regardless
%% of what row the player wanted the turn scored in.
updateScoreCard( ScoreCard, Row, ListOfDice ) ->
  SortedDice = lists:sort( ListOfDice ),
  YahtzeeScore = lists:nth( 12, ScoreCard ),
  case isYahtzee(ListOfDice) of
    true ->
      case YahtzeeScore of
        0 ->
          updateScoreCard2( ScoreCard, Row, SortedDice );
        -1 ->
          updateScoreCard2( ScoreCard, Row, SortedDice );
        50  ->
          YahtzBonus = lists:nth( 14, ScoreCard ),
          NewCard = utils:set_list_index( ScoreCard, 14, YahtzBonus + 100 ),
          updateScoreCard2( NewCard, Row, SortedDice )
      end;
    false -> updateScoreCard2( ScoreCard, Row, SortedDice )
  end.


%% For these delegated calls, we assume that the list of dice is given
%% in sorted order.  This helps a lot...    

isThreeOfAKind( [N, N, N, _, _] ) ->
    true;
isThreeOfAKind( [_, N, N, N, _] ) ->
    true;
isThreeOfAKind( [_, _, N, N, N] ) ->
    true;
isThreeOfAKind( _ ) ->
    false.

isFourOfAKind( [N, N, N, N, _] ) ->
    true;
isFourOfAKind( [_, N, N, N, N] ) ->
    true;
isFourOfAKind( _ ) ->
    false.

isFullHouse( [N, N, N, M, M] ) ->
    true;
isFullHouse( [N, N, M, M, M] ) ->
    true;
isFullHouse( _ ) ->
    false.

isSmallStraight( [1, 2, 3, 4, _] ) -> 
    true;
isSmallStraight( [_, 1, 2, 3, 4] ) -> 
    true;
isSmallStraight( [2, 3, 4, 5, _] ) -> 
    true;
isSmallStraight( [_, 2, 3, 4, 5] ) -> 
    true;
isSmallStraight( [3, 4, 5, 6, _] ) -> 
    true;
isSmallStraight( [_, 3, 4, 5, 6] ) -> 
    true;
isSmallStraight( _ ) ->
    false.

isLargeStraight( [1, 2, 3, 4, 5] ) ->
    true;
isLargeStraight( [2, 3, 4, 5, 6] ) ->
    true;
isLargeStraight( _ ) ->
    false.

isYahtzee( [N, N, N, N, N] ) ->
    true;
isYahtzee( _ ) ->
    false.




%% For the top half (i.e. the first 6 boxes), just count
updateScoreCard2( ScoreCard, Row, ListOfDice ) 
  when Row > 0, Row =< 6 ->
    utils:set_list_index(ScoreCard, Row, Row*utils:count(ListOfDice, Row) );

%% Three of a kind
updateScoreCard2( ScoreCard, 7, ListOfDice ) ->
  case isThreeOfAKind( ListOfDice ) of
    true ->
      utils:set_list_index( ScoreCard, 7, lists:sum( ListOfDice ) );
    false ->
      utils:set_list_index( ScoreCard, 7, 0 )
  end;

%% Four of a kind
updateScoreCard2( ScoreCard, 8, ListOfDice ) ->
    case isFourOfAKind( ListOfDice ) of
  true ->
      utils:set_list_index( ScoreCard, 8, lists:sum( ListOfDice ) );
  false ->
      utils:set_list_index( ScoreCard, 8, 0 )
    end;

%% Full House
updateScoreCard2( ScoreCard, 9, ListOfDice ) ->
    case isFullHouse( ListOfDice ) of
  true ->
      utils:set_list_index( ScoreCard, 9, 25 );
  false ->
      utils:set_list_index( ScoreCard, 9, 0 )
    end;

%% Small Straight
updateScoreCard2( ScoreCard, 10, ListOfDice ) ->
    case isSmallStraight( ListOfDice ) of
  true ->
      utils:set_list_index( ScoreCard, 10, 30 );
  false ->
      utils:set_list_index( ScoreCard, 10, 0 )
    end;

%% Large Straight
updateScoreCard2( ScoreCard, 11, ListOfDice ) ->
    case isLargeStraight( ListOfDice ) of
  true ->
      utils:set_list_index( ScoreCard, 11, 40 );
  false ->
      utils:set_list_index( ScoreCard, 11, 0 )
    end;

%% Yahtzee
updateScoreCard2( ScoreCard, 12, ListOfDice ) ->
    case isYahtzee( ListOfDice ) of
  true ->
      utils:set_list_index( ScoreCard, 12, 50 );
  false ->
      utils:set_list_index( ScoreCard, 12, 0 )
    end;

%% Chance
updateScoreCard2( ScoreCard, 13, ListOfDice ) ->
    utils:set_list_index( ScoreCard, 13, lists:sum( ListOfDice ) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% END SCORING FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



updateDiceList( DiceList, DiceToKeep ) ->
    updateDiceList( DiceList, DiceToKeep, 0, [] ).

updateDiceList( DiceList, [], 0, DiceToSend ) ->
    {DiceList, DiceToSend};

updateDiceList( [FirstDie|RestDice], [FirstToKeep|RestToKeep], NumDiceToSend, [] ) ->
    case FirstToKeep of
	true ->
	    {DiceList, NewDiceToSend} = updateDiceList( RestDice, RestToKeep, NumDiceToSend, [] ),
	    {[FirstDie | DiceList], NewDiceToSend};
	false ->
	    updateDiceList( RestDice, RestToKeep, NumDiceToSend + 1, [] )
    end;

updateDiceList( [FirstDie|RestDice], [], NumDiceToSend, DiceToSend ) ->
    updateDiceList( RestDice, [], NumDiceToSend - 1, [FirstDie|DiceToSend] ).


newGameInMatch( M, 1 ) ->
    #match{ p1 = M#match.p1, p2 = M#match.p2,
	    p1Win = M#match.p1Win + 1,
	    currentGame = M#match.currentGame + 1 };

newGameInMatch( M, 2 ) ->
    #match{ p1 = M#match.p1, p2 = M#match.p2,
	    p2Win = M#match.p2Win + 1,
	    currentGame = M#match.currentGame + 1 }.


generateDice() ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    [random:uniform(6) || _ <- lists:seq(1, 15)].


