%%% -------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Jesse Watts-Russell, David Scott, Alex Melville
%%% @copyright ChickenFartStory Inc. 2015, All rights reserved
%%%
%%% @doc A Generic Server Implementation of a Player
%%% @end
%%%--------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Scorecard Layout %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The patterns on the scorecard, and their scoring rules, are shown in the 
% following table. a and b denote distinct values shown on dice (e.g., the
% sequence a,a,a,b,b means three dice with value a and two dice with value
% b ≠ a). x, y and z denote values shown on dice that may not be distinct
% (e.g., the sequence x, x, x, y, z means at least three, but possibly four
% or five, dice with value x).





%_________________________________________________________________________ |
%                             UPPER SECTION                                |
%_________________________________________________________________________ |
%|PATTERN                   | CONDITION         | SCORE CALCULATION        |
%|__________________________|___________________|_________________________ |
%| Aces                     | none              | total value of all ones  |
%| Twoes                    | none              | total value of all twoes |
%| Threes                   | none              | total value of all threes|
%| Fours                    | none              | total value of all fours |
%| Fives                    | none              | total value of all fives |
%| Sixes                    | none              | total value of all sixes |
%_________________________________________________________________________ |
%                             LOWER SECTION                                |
%_________________________________________________________________________ |
%|PATTERN                   | CONDITION         | SCORE CALCULATION        |
%|__________________________|___________________|_________________________ |
%| Three of a kind          | x, x, x, y, z     | total value of all dice  |
%| Four of a kind           | x, x, x, x, z     | total value of all dice  |
%| Full House               | a, a, a, b, b     | total value of all dice  |
%| Small Straight           | x,x+1,x+2,x+3,y   | 25                       |
%| Large Straight           | x,x+1,x+2,x+3,x+4 | 30                       |
%| Yahtzee                  | x, x, x, x, x     | 40                       |
%| Chance                   | none              | total value of all dice  |
%_________________________________________________________________________ |





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
-define(AUTOMATIC_START, true).

-record(state, {username, 
                ticketDict}). %% Dictionary with manager PID as key and login ticket as value
                %%activeTournaments}). %% Dict of active tournament records

%% Note: tournament records are meant to be stored in a dictionary with tid as the key
%% and the record as the value
% -record(tournament, {gid,
%                      scorecard,
%                      opponentScorecard}).


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
  {ok, #state{username = Username,
                ticketDict = dict:new()}}.


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

%% @spec handle_info({logged_in, Pid, _Username LoginTicket}, 
%%                            State) -> {noreply, State}
%% @doc Message received from the system telling the player it has successfully
%%      registered with the tournament manager
handle_info({logged_in, Pid, _Username, LoginTicket}, State) ->
  io:format(timestamp() ++ ": received logged_in message from Pid: ~p~n", [Pid]),
  {noreply, dict:append(Pid, LoginTicket, State#state.ticketDict)};

%% @spec handle_info({start_tournament, Pid, _Username, LoginTicket}, State) -> none()
%% @doc Message received from the system asking the player if it would like to enter
%%      a tournament
handle_info({start_tournament, Pid, Username, Tid}, State) ->
  io:format(timestamp() ++ ": received start_tournament message from Pid: ~p~n",
                                [Pid]),
  TrueUsername = State#state.username,
  case Username of
    TrueUsername ->
      case ?AUTOMATIC_START of
        true ->
          io:format(timestamp() ++ ": accepting request to start a tournament~n"),
          Pid ! {accept_tournament, self(), State#state.username, {Tid, 
                                              dict:fetch(Pid, State#state.ticketDict)}},
          {noreply, State};
        false ->
          PlayerResponse = io:get_line("Would you like to enter a tournment? (y/n): "),
          case PlayerResponse of
            "y\n" ->
              io:format(timestamp() ++ ": accepting request to start a tournament~n"),
              Pid ! {accept_tournament, self(), State#state.username, {Tid, 
                                          dict:fetch(Pid, State#state.ticketDict)}},
              {noreply, State};
            "n\n" ->
              io:format(timestamp() ++ ": rejecting request to start a tournament~n"),
              Pid ! {reject_tournament, self(), State#state.username, {Tid, 
                                            dict:fetch(Pid, State#state.ticketDict)}},
              {noreply, State};
            true ->
              io:format(timestamp() ++ ": input error! User needs to enter either 'y' or 'n'."
                                    ++ " Please try again~n"),
              handle_info({start_tournament, Pid, Username, Tid}, State)
          end
      end;
    true ->
      io:format(timestamp() ++ ": incorrect username received from tournament manager with pid ~p! 
                                        Trickery is afoot~n", [Pid]),
      {noreply, State}
  end;

%% @spec handle_info({play_request, Pid, _Username, {Ref, Tid, Gid, RollNumber, Dice,
%%                        Scorecard, OpponentsScorecard}}, State) -> {noreply, State}
%% @doc Message received from the rest of the system asking the player to make a play,
%% based on which roll number this play is, the dice, and both player's scorecards
handle_info({play_request, Pid, Username, {Ref, Tid, Gid, RollNumber, Dice,
                        Scorecard, OpponentsScorecard}}, State) ->
  io:format(timestamp() ++ ": received a request to play from pid; ~p
                        with Tid: ~p and Gid: ~p~n", [Pid, Tid, Gid]),
  io:format(timestamp() ++ ": it is roll number ~p and the dice are ~p~n", [RollNumber, Dice]),
  TrueUsername = State#state.username,
  case Username of
    TrueUsername ->
      {DiceToKeep, ScorecardLine} = playerAI(RollNumber, Dice, Scorecard, OpponentsScorecard),
      Pid ! {play_action, self(), State#state.username, {Ref, Tid, Gid, RollNumber,
                                                        DiceToKeep, ScorecardLine}},
      {noreply, State};
    true ->
      io:format(timestamp() ++ ": incorrect username received from tournament manager with pid ~p! 
                                        Trickery is afoot~n", [Pid]),
      {noreply, State}
  end.







        



%%%%%%%%%%%%%%%%%%%%%% END OUTSIDE ASYNCHRONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  io:format(timestamp() ++ ": terminate reason: ~p~n", [_Reason]).




%%%============================================================================
%%% PLAYER AI FUNCTIONS
%%%============================================================================

%% REALLY SILLY AI IMPLEMENTATION, MAKE THIS BETTER!
playerAI(RollNumber, Dice, Scorecard, OpponentsScorecard) ->
  {[true, true, true, true, true], 1}.
    


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
