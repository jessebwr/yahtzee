%%% -------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Jesse Watts-Russell, David Scott, Alex Melville
%%% @copyright ChickenFartStory Inc. 2015, All rights reserved
%%%
%%% @doc A Generic Server Implementation of a Player
%%% @end
%%%--------------------------------------------------------------------

-module(yahtzee_player1).
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

-define(PROCNAME, player).

-record(state, {username, ticket, activeTournaments, numPendingTournaments
				}).


%%%============================================================================
%%% API
%%%============================================================================

main( [NodeName | Username | Password | TournamentManagerNames] ) ->
    os:cmd("epmd -daemon"),
    net_kernel:start([list_to_atom(NodeName), shortnames]),
    io:format( timestamp(now()) ++ ": Initializing The Player " ++ 
		   atom_to_list( node() ) ++ ".~n" ),
    %% I don't think we need to register players at all?
    gen_server:start( yahtzee_player, 
		      {Username, Password, TournamentManagerNames}, []).


%%%============================================================================
%%% GenServer Calls/Casts
%%%============================================================================


%%%============================================================================
%%% GenServer Callbacks (For the rest of the code)
%%%============================================================================

%% @spec init({NodesToConnectTo}) -> {ok, State}.
%% @doc Starts in joining state and sends connectToMePLZ requests to specified
%%		nodes.
init({Username, Password, TournamentManagerNames}) ->
    {ok, #state{ username = Username, ticket = Ticket, }}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% SYNCRHONOUS MESSAGES FROM OTHER PHILOSOPHERS %%%%%%%%%%%%%%%%%%


% Handling Errors and unused callbacks

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    io:format(timestamp (now()) ++ ": terminate reason: ~p~n", [Reason]).



%% @spec timestamp(Now) -> string()
%% @doc Generates a fancy looking timestamp, found on:
%%		http://erlang.2086793.n4.nabble.com/formatting-timestamps-td3594191.html
timestamp(Now) -> 
    {_, _, Micros} = Now, 
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now), 
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~p", 
                  [YY, MM, DD, Hour, Min, Sec, Micros]). 
