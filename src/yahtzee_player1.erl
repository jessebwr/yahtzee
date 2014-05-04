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
-export([main/1,
         decide_choice/4,
         playerAI/4,
         init/0
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
-define(AUTOMATIC_START, true). %% When true, a player will not ask for command-line
%%                              input on whether it should accept a tournament request

-record(state, {username, 
                tournamentDict, %% Dictionary with manager PID as key and 
                                %% a value of a tuple of the form  {LoginTicket, Loggedin},
                                %% where LoginTicket is the login ticket received
                                %% from a logged_in message, and LoggedIn is a
                                %% boolean describing whether the player is logged
                                %% in to the tournament with the key as a Pidlogin
                loggedIn}). 
                %%activeTournaments}). %% Dict of active tournament records

%% Note: tournament records are meant to be stored in a dictionary with tid as the key
%% and the record as the value
% -record(tournament, {gid,
%                      scorecard,
%                      opponentScorecard}).


%%%============================================================================
%%% API
%%%============================================================================

main(Params) ->
  NodeName = hd(Params),
  Username = hd(tl(Params)),
  Password = hd(tl(tl(Params))),
  Managers = tl(tl(tl(Params))),
  io:format("Managers: ~p~n", [Managers]),
  AtomizedManagers = lists:map(fun(X) -> list_to_atom(X) end, Managers),
  os:cmd("epmd -daemon"),
  net_kernel:start([list_to_atom(NodeName), shortnames]),
  yahtzee_chooser:init(),
  gen_server:start({local, ?PROCNAME}, ?MODULE, 
            {Username, Password, AtomizedManagers}, []).
init() -> 
% filename:join ([filename:dirname (code:which (?MODULE)),"..","bin"] ),
% erlang:load_nif (filename:join (PrivDir, "chooser"), 0).
erlang:load_nif("./yahtzee_chooser", 0).



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
              tournamentDict = dict:new(),
              loggedIn = true}}.


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
  io:format(utils:timestamp() ++ ": received logged_in message from Pid: ~p~n", [Pid]),
  NewDict = dict:store(Pid, {LoginTicket, true}, State#state.tournamentDict),
  {noreply, State#state{tournamentDict = NewDict}};

%% @spec handle_info(logout, State) -> {noreply, State}
%% @doc Message callback when a user calls the logout helper function.
%%      The player then updates its state record and sends a logout message
%%      to the tournament
handle_info({logout, TournamentPid}, State) ->
  {LoginTicket, _} = dict:fetch(TournamentPid, State#state.tournamentDict),
  io:format(utils:timestamp() ++ ": send logout message to tournament manager with PID ~p~n",
                              [TournamentPid]),
  TournamentPid ! {logout, self(), State#state.username, LoginTicket},
  NewDict = dict:store(TournamentPid, {LoginTicket, false}, State#state.tournamentDict),
  {noreply, State#state{tournamentDict = NewDict}};

%% @spec handle_info({start_tournament, Pid, _Username, LoginTicket}, State) -> none()
%% @doc Message received from the system asking the player if it would like to enter
%%      a tournament
handle_info({start_tournament, Pid, Username, Tid}, State) ->
  io:format(utils:timestamp() ++ ": received start_tournament message from Pid: ~p~n",
                                [Pid]),
  TrueUsername = State#state.username,
  case Username of
    TrueUsername ->
      case ?AUTOMATIC_START of
        true ->
          io:format(utils:timestamp() ++ ": accepting request to start a tournament~n"),
          {LoginTicket, _} = dict:fetch(Pid, State#state.tournamentDict),
          Pid ! {accept_tournament, self(), State#state.username, {Tid, 
                                              LoginTicket}},
          {noreply, State};
        false ->
          PlayerResponse = io:get_line("Would you like to enter a tournment? (y/n): "),
          case PlayerResponse of
            "y\n" ->
              io:format(utils:timestamp() ++ ": accepting request to start a tournament~n"),
              {LoginTicket, _} = dict:fetch(Pid, State#state.tournamentDict),
              Pid ! {accept_tournament, self(), State#state.username, {Tid, 
                                          LoginTicket}},
              {noreply, State};
            "n\n" ->
              io:format(utils:timestamp() ++ ": rejecting request to start a tournament~n"),
              {LoginTicket, _} = dict:fetch(Pid, State#state.tournamentDict),
              Pid ! {reject_tournament, self(), State#state.username, {Tid, 
                                            LoginTicket}},
              {noreply, State};
            true ->
              io:format(utils:timestamp() ++ ": input error! User needs to enter either 'y' or 'n'."
                                    ++ " Please try again~n"),
              handle_info({start_tournament, Pid, Username, Tid}, State)
          end
      end;
    true ->
      io:format(utils:timestamp() ++ ": incorrect username received from tournament manager with pid ~p! " ++ 
                                        "Trickery is afoot~n", [Pid]),
      {noreply, State}
  end;

%% @spec handle_info({play_request, Pid, _Username, {Ref, Tid, Gid, RollNumber, Dice,
%%                        Scorecard, OpponentsScorecard}}, State) -> {noreply, State}
%% @doc Message received from the rest of the system asking the player to make a play,
%% based on which roll number this play is, the dice, and both player's scorecards
handle_info({play_request, Pid, Username, {Ref, Tid, Gid, RollNumber, Dice,
                        Scorecard, OpponentsScorecard}}, State) ->
  io:format(utils:timestamp() ++ ": received a request to play from pid; ~p " ++
                        "with Tid: ~p and Gid: ~p~n", [Pid, Tid, Gid]),
  io:format(utils:timestamp() ++ ": it is roll number ~p and the dice are ~p~n", [RollNumber, Dice]),
  TrueUsername = State#state.username,
  case Username of
    TrueUsername ->
      {DiceToKeep, ScorecardLine} = playerAI(RollNumber, Dice, Scorecard, OpponentsScorecard),
      io:format(utils:timestamp() ++ ": sending play_action message with the scorecard " ++
                                      "line ~p and dice to keep ~p~n", [ScorecardLine, DiceToKeep]),
      Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber,
                                                        DiceToKeep, ScorecardLine}},
      {noreply, State};
    true ->
      io:format(utils:timestamp() ++ ": incorrect username received from tournament manager with pid ~p! 
                                        Trickery is afoot~n", [Pid]),
      {noreply, State}
  end.







        



%%%%%%%%%%%%%%%%%%%%%% END OUTSIDE ASYNCHRONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  io:format(utils:timestamp() ++ ": terminate reason: ~p~n", [_Reason]).




%%%============================================================================
%%% PLAYER AI FUNCTIONS
%%%============================================================================

%% REALLY SILLY AI IMPLEMENTATION, MAKE THIS BETTER!
playerAI(RollNumber, Dice, Scorecard, OpponentsScorecard) ->
  case RollNumber of
    3 ->
      {_EV, Choice} = yahtzee_chooser:decide_choice(Scorecard, OpponentsScorecard, 3, Dice),
      {[true, true, true, true, true], Choice};
    _ ->
      {_EV, KeepDice, _ReturnDice} = yahtzee_chooser:decide_choice(Scorecard, OpponentsScorecard, RollNumber, Dice),
      BoolList = get_bool_list(KeepDice, Dice),
      {BoolList, 0}
  end.


get_bool_list([], Dice) ->
  finalize(Dice);

get_bool_list([KeptDie | Rest], Dice) ->
  Index = string:str(Dice, [KeptDie]),
  NewDice = utils:set_list_index(Dice, Index, true),
  get_bool_list(Rest, NewDice).

finalize([]) ->
  [];

finalize([H | T]) ->
  case H == true of
    true ->
      [H] ++ finalize(T);
    false ->
      [false] ++ finalize(T)
  end.


decide_choice(_MyCard, _OpponentCard, _RollNumber, _CurrentDice) ->
  "NIF library not loaded".


%%%============================================================================
%%% Other (possibly) useful functions
%%%============================================================================


%% @spec logout(TournamentPid) -> none()
%% @doc Preferred method to call for a player to log out from the 
%% tournament with the inputted Pid, so the state record can be
%% updated correctly. A player sends a message to itself so in
%% handle_info it can access the state record
logout(TournamentPid) ->
  self() ! {logout, TournamentPid}.

%% @spec login_to_managers(List::TournamentManagerNames, string()::Username), string()::Password -> ok
%% @doc Sends a message to all of the nodes in the list of tournament managers asking
%% them each to register the player with the given username and password
login_to_managers([], _, _) -> ok;
login_to_managers(Managers, Username, Password) ->
  io:format(utils:timestamp() ++ ": sending login message to node with name ~p~n",
    [hd(Managers)]),
  {?MANAGER_NAME, hd(Managers)} ! {login, self(), Username, {Username, Password}},
  login_to_managers(tl(Managers), Username, Password).


