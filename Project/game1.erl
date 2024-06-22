-module(game1).
-export([start/1, loop/6]).

start(PlayerFile) ->
    {ok, PlayerInfo} = file:consult(PlayerFile),
    PlayerPIDs = spawn_players(PlayerInfo),
    Pid = spawn_link(fun() -> loop(PlayerInfo, PlayerPIDs, [], 1, #{}, #{}) end),
    register(master, Pid),
    lists:foreach(fun({Name, _PID}) -> Name ! {request_game, Name} end, PlayerPIDs),
    receive
        end_tournament ->
            io:format("Hello, world!n", []),
            exit(normal)
    after 5000 ->
        io:format("Forcing termination after timeout...n", []),
        exit(Pid, kill)
    end.

spawn_players([]) -> [];
spawn_players([{Name, Credits} | Rest]) ->
    PlayerPID = spawn(player1, init, [self(), Name, Credits]),
    register(Name, PlayerPID),
    [{Name, PlayerPID} | spawn_players(Rest)].

loop(PlayerInfo, PlayerPIDs, CompletedGames, GameID, GamesInProgress, PlayerStates) ->
    receive
        {request_game, PlayerName} ->
            case find_opponent(PlayerName, PlayerPIDs) of
                none ->
                    loop(PlayerInfo, PlayerPIDs, CompletedGames, GameID, GamesInProgress, PlayerStates);
                Opponent ->
                    Player1PID = get_pid(PlayerName),
                    Player2PID = get_pid(Opponent),
                    NewGameID = GameID,
                    io:format("+ [~p] New Game between between ~s and ~sn", [NewGameID, PlayerName, Opponent]),
                    Player1PID ! {play, NewGameID, Opponent},
                    Player2PID ! {play, NewGameID, PlayerName},
                    loop(PlayerInfo, PlayerPIDs, CompletedGames, GameID + 1, GamesInProgress#{NewGameID => {Player1PID, Player2PID}}, PlayerStates)
            end;
        {move, GameID, Player, Move} ->
            case maps:get(GameID, GamesInProgress, undefined) of
                {Player1PID, Player2PID} ->
                    NewPlayerStates = maps:put(GameID, {Player, Move}, PlayerStates),
                    case maps:find(GameID, NewPlayerStates) of
                        {ok, {Opponent, OpponentMove}} when Opponent =/= Player ->
                            Result = determine_winner(Move, OpponentMove),
                            case Result of
                                win ->
                                    io:format("$ (~p) ~p:~p -> ~p:~p = Winner: ~p~n", [GameID, Player, Move, Opponent, OpponentMove, Player]),
                                    Player1PID ! {game_result, GameID, Player, Opponent, win},
                                    Player2PID ! {game_result, GameID, Opponent, Player, lose};
                                tie ->
                                    io:format("$ (~p) ~p:~p -> ~p:~p = Tie~n", [GameID, Player, Move, Opponent, OpponentMove]),
                                    loop(PlayerInfo, PlayerPIDs, CompletedGames, GameID, GamesInProgress, NewPlayerStates);
                                lose ->
                                    io:format("$ (~p) ~p:~p -> ~p:~p = Winner: ~p~n", [GameID, Player, Move, Opponent, OpponentMove, Opponent]),
                                    Player1PID ! {game_result, GameID, Player, Opponent, lose},
                                    Player2PID ! {game_result, GameID, Opponent, Player, win};
                            end;    
                            loop(PlayerInfo, PlayerPIDs, [{GameID, Player, Opponent, Result} | CompletedGames], GameID, maps:remove(GameID, GamesInProgress), maps:remove(GameID, NewPlayerStates));
                        _ ->
                            loop(PlayerInfo, PlayerPIDs, CompletedGames, GameID, GamesInProgress, NewPlayerStates);
                    end,
                undefined ->
                    loop(PlayerInfo, PlayerPIDs, CompletedGames, GameID, GamesInProgress, PlayerStates)
            end;
        {disqualified, Player} ->
            NewPlayerPIDs = lists:keydelete(Player, 1, PlayerPIDs),
            loop(PlayerInfo, NewPlayerPIDs, CompletedGames, GameID, GamesInProgress, PlayerStates);
        end_tournament ->
            display_report(PlayerPIDs, CompletedGames),
            exit(normal)
    end.

find_opponent(PlayerName, PlayerPIDs) ->
    OtherPlayers = [Name || {Name, _} <- PlayerPIDs, Name =/= PlayerName],
    case OtherPlayers of
        [] -> none;
        _ -> lists:nth(rand:uniform(length(OtherPlayers)), OtherPlayers)
    end.

get_pid(Player) ->
    case whereis(Player) of
        undefined -> exit({badarg, {unknown_player, Player}});
        PID -> PID
    end.

determine_winner(Move1, Move2) ->
    case {Move1, Move2} of
        {rock, scissors} -> win;
        {scissors, paper} -> win;
        {paper, rock} -> win;
        {rock, rock} -> tie;
        {scissors, scissors} -> tie;
        {paper, paper} -> tie;
        _ -> lose
    end.

display_report(PlayerPIDs, CompletedGames) ->
    io:format("** Tournament Report **n"),
    lists:foreach(fun({Name, PID}) ->
        {_, Credits} = player1:get_info(PID),
        io:format("Player: ~s, Credits: ~pn", [Name, Credits])
    end, PlayerPIDs),
    lists:foreach(fun({GameID, Player1, Player2, Result}) ->
        io:format("Game ~p: ~s vs ~s, Winner: ~pn", [GameID, Player1, Player2, Result])
    end, CompletedGames),
    io:format("See you next year...~n").