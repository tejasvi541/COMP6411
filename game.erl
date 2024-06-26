-module(game).
-export([start/1]).

start([PlayerFile]) ->
    {ok, PlayerInfo} = file:consult(PlayerFile),
    io:format("** Rock, Paper, Scissors World Championship **~n"),
    io:format("Starting the game log...~n"),
    MasterPid = self(),
    Players = lists:map(fun({Name, _Credits}) -> Name end, PlayerInfo),
    spawn_player_processes(PlayerInfo, MasterPid,Players),
    game_engine(PlayerInfo, MasterPid, 0, []).

spawn_player_processes([], _MasterPid,Players) -> ok;
spawn_player_processes([{Name, Credits} | Rest], MasterPid,Players) ->
    TempPid = spawn(fun() -> player:init(Name, Credits, MasterPid,Players) end),
    spawn_player_processes(Rest, MasterPid,Players).

game_engine(Players, MasterPid, GameId, GameResults) ->
    receive
        {game_request, P1Pid, P1Name, P2Pid, P2Name} ->
            handle_game_request(P1Pid, P1Name, P2Pid, P2Name, GameId, Players, MasterPid, GameResults);
        {game_move, Gid, PlayerPid, Move} ->
            handle_game_move(Gid, PlayerPid, Move, Players, MasterPid, GameResults);
        {player_disqualified, Name} ->
            handle_player_disqualification(Name, Players, MasterPid, GameId, GameResults);
        {game_result, GameId, Result} ->
            handle_game_result(GameId, Result, Players, MasterPid, GameResults);
        {update_players, UpdatedPlayers} ->
            game_engine(UpdatedPlayers, MasterPid, GameId, GameResults);
        {game_tied, GameId, P1Pid, P2Pid} ->
            handle_game_tied(GameId, P1Pid, P2Pid, Players, MasterPid, GameResults)
    end.

handle_game_request(P1Pid, P1Name, P2Pid, P2Name, GameId, Players, MasterPid, GameResults) ->
    NewGameId = GameId + 1,
    io:format("+ [~p] new game for ~p -> ~p~n", [NewGameId, P2Name, P1Name]),
    P1Pid ! {game_move, NewGameId},
    P2Pid ! {game_move, NewGameId},
    game_engine(Players, MasterPid, NewGameId, GameResults).

handle_player_disqualification(Name, Players, MasterPid, GameId, GameResults) ->
    NewPlayers = lists:filter(fun({PlayerName, _Credits}) -> PlayerName =/= Name end, Players),
    if length(NewPlayers) =:= 1 ->
        [Winner] = NewPlayers,
        io:format("Tournament over! The winner is: ~p~n", [Winner]),
        report_printing(GameResults),
        halt();
    true ->
        game_engine(NewPlayers, MasterPid, GameId, GameResults)
    end.

handle_game_result(GameId, Result, Players, MasterPid, GameResults) ->
    NewGameResults = [{GameId, Result} | GameResults],
    game_engine(Players, MasterPid, GameId, NewGameResults).

handle_game_tied(GameId, P1Pid, P2Pid, Players, MasterPid, GameResults) ->
    P1Pid ! {game_tied, GameId},
    P2Pid ! {game_tied, GameId},
    game_engine(Players, MasterPid, GameId, GameResults).


handle_game_move(GameId, PlayerPid, Move, Players, MasterPid, GameResults) ->
    case lists:keyfind(GameId, 1, GameResults) of
        false ->
            NewGameResults = [{GameId, [{PlayerPid, Move}]} | GameResults],
            game_engine(Players, MasterPid, GameId, NewGameResults);
        {GameId, Moves} ->
            UpdatedMoves = [lists:nth(1, Moves), {PlayerPid, Move}],
            handle_moves(GameId, UpdatedMoves, GameResults, Players, MasterPid)
    end.

handle_moves(GameId, [{P1Pid, Move1}, {PlayerPid, Move}], GameResults, Players, MasterPid) ->
    Result = determine_winner(Move1, Move),
    {_, Player1Name} = lookup_name_by_process_id(P1Pid),
    {_, Player2Name} = lookup_name_by_process_id(PlayerPid),

    send_result(Result, GameId, P1Pid, PlayerPid, Player1Name, Move1, Player2Name, Move),

    NewGameResults = lists:keydelete(GameId, 1, GameResults),
    game_engine(Players, MasterPid, GameId, NewGameResults).

send_result(tie, GameId, P1Pid, P2Pid, Player1Name, Move1, Player2Name, Move2) ->
    io:format("$ (~p) ~p:~p -> ~p:~p,", [GameId, Player2Name, Move2, Player1Name, Move1]),
    self() ! {game_tied, GameId, P1Pid, P2Pid};
send_result(lost, GameId, P1Pid, P2Pid, Player1Name, Move1, Player2Name, Move2) ->
    P1Pid ! {game_result, GameId, lost, Player1Name, Move1, Player2Name, Move2, Player1Name};
send_result(_, GameId, P1Pid, P2Pid, Player1Name, Move1, Player2Name, Move2) ->
    P2Pid ! {game_result, GameId, lost, Player1Name, Move1, Player2Name, Move2, Player2Name}.


determine_winner(rock, scissors) -> win;
determine_winner(scissors, paper) -> win;
determine_winner(paper, rock) -> win;
determine_winner(scissors, rock) -> lost;
determine_winner(paper, scissors) -> lost;
determine_winner(rock, paper) -> lost;
determine_winner(_, _) -> tie.

report_printing(Message) ->
    io:format("** Tournament Report **~n~p~n", [Message]).

lookup_name_by_process_id(Pid) ->
    RegisteredNames = erlang:registered(),
    lists:foldl(
        fun(Name, Acc) ->
            case whereis(Name) of
                Pid ->
                    {ok, Name};
                _ ->
                    Acc
            end
        end,
        {error, not_found},
        RegisteredNames
    ).
