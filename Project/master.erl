-module(master).
-export([start/1]).

start(PlayerInfo) ->
    Players = lists:map(fun({Name, _Credits}) -> {Name, self(), _Credits} end, PlayerInfo),
    spawn(fun() -> loop(PlayerInfo, Players, [], 1, #{}, #{}) end).

loop([], _Players, ActivePlayers, _GameID, _GameStates, _Results) ->
    % Tournament over
    io:format("Tournament over. Stopping all active players.~n"),
    lists:foreach(fun({_, Pid, _}) -> Pid ! stop end, ActivePlayers),
    io:format("Tournament summary:~n"),
    lists:foreach(fun({Name, _, Credits}) ->
        io:format("Player ~p - Credits remaining: ~p~n", [Name, Credits])
    end, ActivePlayers),
    io:format("Tournament is over~n"),
    ok;

loop(PlayerInfo, Players, ActivePlayers, GameID, GameStates, Results) ->
    receive
        {new_game, Pid, Name, Opponent} ->
            io:format("Received new game request from ~p to ~p~n", [Name, Opponent]),
            NewGameID = GameID + 1,
            Pid ! {game_request, NewGameID, Opponent},
            % Store the game state
            GameStates1 = maps:put(NewGameID, {Name, Opponent, undefined, undefined}, GameStates),
            loop(PlayerInfo, Players, ActivePlayers, NewGameID, GameStates1, Results);
        {move, GameID, Name, Move, Opponent, Opp_Move} ->
            io:format("Received move ~p from ~p for game ~p~n", [Move, Name, GameID]),
            io:format("VVVVVVVVVVV : ~p", [GameStates]),
            case maps:get(GameID, GameStates) of
                {P1, P2, undefined, undefined} ->
                    % First move
                    io:format("first : ~p ~p~n", [Name, Move]),
                    GameStates1 = maps:put(GameID, {P1, P2, Name, Move}, GameStates),
                    
                    % Second move
                    io:format("second : ~p~n", [ActivePlayers]),
                    Winner = determine_winner(Opp_Move, Move),
                    io:format("$ (~p) ~p:~p -> ~p:~p = Winner: ~p~n", [GameID, Opponent, Opp_Move, Name, Move, Winner]),
                    {WinnerName, LoserName} = if Winner == tie ->
                            {tie, tie};
                        Winner == Opp_Move ->
                            {Opponent, Name};
                        Winner == Move ->
                            {Name, Opponent}
                    end,
                    Results1 = case Winner of
                        tie ->
                            io:format("Game ~p is a tie. Replaying...~n", [GameID]),
                            Pid1 = lists:keyfind(Opponent, 1, ActivePlayers),
                            Pid2 = lists:keyfind(Name, 1, ActivePlayers),
                            element(2, Pid1) ! {move_result, GameID, tie},
                            element(2, Pid2) ! {move_result, GameID, tie},
                            Results;
                        _ ->
                            Pid1 = lists:keyfind(Opponent, 1, ActivePlayers),
                            Pid2 = lists:keyfind(Name, 1, ActivePlayers),
                            WinnerPid = if Winner == Opp_Move -> element(2, Pid1); Winner == Move -> element(2, Pid2) end,
                            LoserPid = if Winner == Opp_Move -> element(2, Pid2); Winner == Move -> element(2, Pid1) end,
                            WinnerPid ! {move_result, GameID, win},
                            LoserPid ! {move_result, GameID, lose},
                            update_results(WinnerName, LoserName, Results)
                    end,
                    ActivePlayers1 = if Winner == tie ->
                            ActivePlayers;
                        true ->
                            lists:keyreplace(LoserName, 1, ActivePlayers, {LoserName, self(), credits(LoserName, ActivePlayers) - 1})
                    end,
                    loop(PlayerInfo, Players, ActivePlayers1, GameID, maps:remove(GameID, GameStates), Results1)
            end;
        {disqualified, Player} ->
            io:format("Player ~p is disqualified~n", [Player]),
            NewActivePlayers = lists:map(
                fun({Name, Pid, Credits}) ->
                    case Name == Player of
                        true ->
                            NewCredits = Credits - 1,
                            {Name, Pid, NewCredits}; % Reduce credits by 1 for the disqualified player
                        false ->
                            {Name, Pid, Credits} % Keep credits unchanged for other players
                    end
                end, ActivePlayers),
            UpdatedActivePlayers = lists:filter(fun({_Name, _Pid, Credits}) -> Credits > 0 end, NewActivePlayers),
            io:format("Updated active players after disqualification: ~p~n", [UpdatedActivePlayers]),
            
            % Check if only one player is left
            case UpdatedActivePlayers of
                [LastPlayer] ->
                    io:format("Tournament over. Stopping all active players.~n"),
                    {Name, Pid, Credits} = LastPlayer,
                    Pid ! stop,
                    io:format("Tournament summary:~n"),
                    io:format("Player ~p - Credits remaining: ~p~n", [Name, Credits]),
                    io:format("Tournament is over~n"),
                    ok;
                _ ->
                    loop(PlayerInfo, Players, UpdatedActivePlayers, GameID, GameStates, Results)
            end;

        {register, Pid, Name, Credits} ->
            case lists:keyfind(Name, 1, ActivePlayers) of
                false ->
                    % Player not found, register new
                    io:format("Registering player ~p with ~p credits~n", [Name, Credits]),
                    loop(PlayerInfo, Players, [{Name, Pid, Credits} | ActivePlayers], GameID, GameStates, Results);
                {_, PidOld, _} ->
                    % Player found, update credits
                    io:format("Updating credits for player ~p to ~p~n", [Name, Credits]),
                    NewActivePlayers = lists:keyreplace(Name, 1, ActivePlayers, {Name, Pid, Credits}),
                    loop(PlayerInfo, Players, NewActivePlayers, GameID, GameStates, Results)
            end
    after 100 ->
        % Check if only one player is left
        case lists:filter(fun({_Name, _Pid, Credits}) -> Credits > 0 end, ActivePlayers) of
            [LastPlayer] ->
                io:format("Only one player left: ~p~n", [LastPlayer]),
                loop([], Players, ActivePlayers, GameID, GameStates, Results);
            _ ->
                loop(PlayerInfo, Players, ActivePlayers, GameID, GameStates, Results)
        end
    end.

determine_winner(rock, scissors) -> rock;
determine_winner(scissors, paper) -> scissors;
determine_winner(paper, rock) -> paper;
determine_winner(Move, Move) -> tie;
determine_winner(_, _) -> invalid.

update_results(Winner, Loser, Results) ->
    Results1 = maps:update_with(Winner, fun({Wins, Losses}) -> {Wins + 1, Losses} end, {1, 0}, Results),
    maps:update_with(Loser, fun({Wins, Losses}) -> {Wins, Losses + 1} end, {0, 1}, Results1).

credits(Name, Players) ->
    case lists:keyfind(Name, 1, Players) of
        {_, _, Credits} -> Credits;
        _ -> 0
    end.
