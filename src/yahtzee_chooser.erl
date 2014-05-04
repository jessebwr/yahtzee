-module(yahtzee_chooser).

-export([decide_choice/4,
         run_one_game/0,
         get_avg/0,
         init/0]).

init() -> 
  % filename:join ([filename:dirname (code:which (?MODULE)),"..","bin"] ),
  % erlang:load_nif (filename:join (PrivDir, "chooser"), 0).
  erlang:load_nif("./yahtzee_chooser", 0).


generateDice() ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    [random:uniform(6) || _ <- lists:seq(1, 15)].


get_avg()->
  A = lists:seq(1, 400),
  B = lists:map(fun(_X) -> run_one_game() end, A),
  lists:sum(B)/length(B).



run_one_game() ->
  Dice = generateDice(),
  ScoreCard = [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0],
  run_one_game(Dice, 1, ScoreCard, 1).

run_one_game(Dice, RollNumber, ScoreCard, _TurnNumber) when 
  (RollNumber == 1) or (RollNumber ==2) ->
  DiceToGive = lists:sublist(Dice, 5),
  DiceLeftOver = lists:subtract(Dice, DiceToGive),
  {_EV, KeepDice, _RieturnDice} = decide_choice(ScoreCard, ScoreCard, RollNumber, DiceToGive),
  NextDice = KeepDice ++ DiceLeftOver,
  %io:format("Dice Given: ~p, DiceKept: ~p, DiceReturned: ~p~n", [DiceToGive, KeepDice, _ReturnDice]),
  run_one_game(NextDice, RollNumber + 1, ScoreCard, _TurnNumber);

run_one_game(Dice, 3, ScoreCard, 13) ->
  FinalDice = lists:sublist(Dice, 5),
  {_EV, Choice} = decide_choice(ScoreCard, ScoreCard, 3, FinalDice),
  RealChoice = Choice,
  NewScoreCard = updateScoreCard(ScoreCard, RealChoice, FinalDice),
  io:format("Dice Given: ~p, ChoiceMade: ~p, Final ScoreCard: ~p, Final Score: ~p~n", [FinalDice, RealChoice, NewScoreCard, score(NewScoreCard)]),
  score(NewScoreCard);
  %io:format("Score: ~p, ScoreCard: ~p~n", [score(NewScoreCard), NewScoreCard]);

run_one_game(Dice, 3, ScoreCard, TurnNumber) ->
  FinalDice = lists:sublist(Dice, 5),
  {_EV, Choice} = decide_choice(ScoreCard, ScoreCard, 3, FinalDice),
  RealChoice = Choice,
  NewScoreCard = updateScoreCard(ScoreCard, RealChoice, FinalDice),
  %io:format("Dice Given: ~p, ChoiceMade: ~p, NewScoreCard: ~p~n", [FinalDice, RealChoice, NewScoreCard]),
  run_one_game(generateDice(), 1, NewScoreCard, TurnNumber + 1).

score(ScoreCard) ->
  TopSum = lists:sum(lists:sublist(ScoreCard, 6)),
  BottomSum = lists:sum(lists:sublist(ScoreCard, 7, 7)),
  YahtzeeSum = lists:nth(14, ScoreCard),
  TopSum + topBonus(TopSum) + BottomSum + YahtzeeSum.

topBonus(Score) ->
  case Score >= 63 of
    true ->
      35;
    false ->
      0
  end.

decide_choice(_MyCard, _OpponentCard, _RollNumber, _CurrentDice) ->
  "NIF library not loaded".


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

