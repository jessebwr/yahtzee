-module(yahtzee_chooser).

-export([decide_choice/4,
         init/0]).

init() -> 
  % filename:join ([filename:dirname (code:which (?MODULE)),"..","bin"] ),
  % erlang:load_nif (filename:join (PrivDir, "chooser"), 0).
  erlang:load_nif("./yahtzee_chooser", 0).


decide_choice(MyCard, OpponentCard, RollNumber, CurrentDice) ->
  "NIF library not loaded".