#Distributed Yahtzee
=======

## The tournament manager
A distributed yahtzee tournament manager than communicates with multiple players, sets up tournament brackets, and runs tournaments.

Records for individual players are kept for scoring purposes. These records will be written to files via ETS tables.

## The player
Implemented using AI heuristics. Note that the AI is implemented in C++ and used in our Erlang code via NIFs (Natively Implemented Fuctions). Has a calculated expected score every iteration and trys to maximize total score (player1). player2 just chooses yahtzee boxes sequentially.
