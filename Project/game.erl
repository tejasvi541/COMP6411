-module(game).
-export([start/1]).

start(Args) ->
    % Read the player file
    PlayerFile = lists:nth(1, Args),
    {ok, PlayerInfo} = file:consult(PlayerFile),
    io:format("** Rock, Paper Scissors World Championship ** \n \n ~n"),
    io:format("Starting game log... \n \n ~n"),

    % Spawn the master process
    Master = master:start(PlayerInfo),
    
    % Extract player names for random selection
    Players = lists:map(fun({Name, _Credits}) -> Name end, PlayerInfo),
    
    % Spawn player processes with the complete list of players
    lists:foreach(fun({Name, Credits}) -> 
        % io:format("Spawning player process for ~p with ~p credits~n", [Name, Credits]),
        player:start(Name, Credits, Master, Players) 
    end, PlayerInfo).
