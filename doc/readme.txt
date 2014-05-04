Names: Alex Melville, Jesse Watts-Russell and David Scott
Date: May 3, 2014
CS182E- Distributed Systems - Prof. Zimmerman
Assignment 6 - Distributed Yahtzee

-------------readme.txt for assignment 6------------------------

General Notes:
Our tournament managers and players make use of the OTP gen_server
module in order to make the passing of state and the sending and
receiving of messages easier to implement. We make heavy use of
ets tables to store the state variables for things lke the
tournament, players, matches, and timeout information. We also
use records a lot, which are glorified tuples but are also similar
to structs in C.
Please use the yahtzee_player1.erl module when trying out our
player AI, because player2 is a very poor player.
Also, you'll not that at the end of the tournaments our players
print out

"Got a message... Derp?"

This comes from the end_tournament message which our players are not
required to take care of, so it gets caught by our catch-all case.



How to run our code:

First, start a tournament manager by typing in the Terminal
erl -noshell -run yahtzee_manager main <MANAGERNAME>

Then start as many players as you want by typing
erl -noshell -run -yahtzee_player1/2 main <PLAYERNAME> <USERNAME> <PASSWORD> <MANAGERNAME>

Finally, once you have made all of the players that you'd like to join, create an Erlang
node with the following line of Terminal code

erl -sname <NODENAME>

and then from within this Erlang node send a request_tournament message to the tournament manager
{yahtzee_manager, <MANAGERNAME} ! {request_tournament, self(), {<NUMPLAYERS>, <NUMGAMES>}}.



----utils.erl----

A module that holds functions for finding elements within lists, doing
simple mathematical operations, and finding the timestamp. These are
called mainly by the players and the manager.

----yahtzee_manager.erl----

The workhorse of this assignment. The manager initializes and keeps
track of the brackets, which consist of matches, for multiple
tournaments. Matches consist of Yahtzee games involving players
that connect to the tournament manager. The manager can be queried
for information regarding players, matches, and games. The tournament
bracket is updated everytime a match ends, until only a single player
(the winner) is left int he bracket. Along the way if two players
make the exact same decisions, a tie break game begins and the
players are given difference dice rolls as explained in the assignment.
The manager handles players quitting when they're not in any matches.
See the Yahtzee Protocol page for a more detailed explanation of the
different messages the manager listens for and sends out. 



----yahtzee_player1/yahtzee_player2.erl----

The players connects to a tournament manager, and then is placed in
a tournament when the outside world requests a tournament to start.
Note that players are designed to start immediately when they receive
a request to enter a tournament from the tournament manager.
Players receive dice from the tournament manager, make their
decisions, and then send a list of which dice they would like to
keep and which ones they want to trade in for more dice.
The players have no knowledge of the tournament when it's in progress;
they only know about the tournament their competing in when they first
accept an invitation to join one, and when they happen to be the
winners of the tournament. We have created two players (yahtzee_player1
and yahtzee_player2) The first has an AI that use depth-one search
algorithms implemented natively in C++ to find the optimal choice of which
dice to keep and which to give away. The player yahtzee_player2 is a dummy
player that the team made two test against our smarter AI.




----yahtzee_chooser.erl----

The erlang file to interface between the AI player and the native C++ code.



----test.txt----
A text file that contains the output of succesful runs using our player1,
player2, and our tournament manager. We use this file to show that we tested
our app and it performs well.
