-module(game).
-export([start/1]).

start([PlayerFile]) ->
    {ok, PlayerInfo} = file:consult(PlayerFile),
    io:format("** Rock, Paper Scissors World Championship ** \n \n \n"),
    io:format("Starting game log... \n \n"),
    MasterProcess = self(),
    Players = lists:map(fun({Name, _Credits}) -> Name end, PlayerInfo),
    spawn_player_processes(PlayerInfo, MasterProcess,Players),
    game_engine(PlayerInfo,PlayerInfo, MasterProcess, 0, [],[],[]).

spawn_player_processes([], _MasterPid,Players) -> ok;
spawn_player_processes([{Name, Credits} | Rest], MasterProcess,Players) ->
    spawn(fun() -> player:start(Name, Credits, MasterProcess,Players) end),
    spawn_player_processes(Rest, MasterProcess,Players).

%% Main game engine loop, which handles various game-related events.
game_engine(PlayerData, Players, MasterProcess, GameID, GameResults, DisqualifiedBacklog, MovesRecord) ->
    receive
        {game_request, Player1ProcessID, Player1Name, Player2ProcessID, Player2Name} ->
            handle_game_request(Player1ProcessID, Player1Name, Player2ProcessID, Player2Name, PlayerData, Players, MasterProcess, GameID, GameResults, DisqualifiedBacklog, MovesRecord);
        
        {match_move, GameId, PlayerPid, Move} ->
            strategy_handler(PlayerData, GameId, PlayerPid, Move, Players, MasterProcess, GameResults, DisqualifiedBacklog, MovesRecord);

        {player_disqualified, Name} ->
            handle_player_disqualification(Name, PlayerData, Players, MasterProcess, GameID, GameResults, DisqualifiedBacklog, MovesRecord);

        {main_result, GameId, Result} ->
            handle_game_result(GameId, Result, PlayerData, Players, MasterProcess, GameResults, DisqualifiedBacklog, MovesRecord);

        {update_players, LostPlayerName, NewCredits} ->
            update_player_credits(LostPlayerName, NewCredits, PlayerData, Players, MasterProcess, GameID, GameResults, DisqualifiedBacklog, MovesRecord);

        {game_tied, GameId, Player1ProcessID, Player2ProcessID} ->
            handle_game_tied(GameId, Player1ProcessID, Player2ProcessID, PlayerData, Players, MasterProcess, GameResults, DisqualifiedBacklog, MovesRecord)
    end.

%% Handles the initiation of a new game.
handle_game_request(Player1ProcessID, Player1Name, Player2ProcessID, Player2Name, PlayerData, Players, MasterProcess, GameID, GameResults, DisqualifiedBacklog, MovesRecord) ->
    NewGameId = GameID + 1,
    io:format("+ [~p] new game for ~p -> ~p~n", [NewGameId, Player2Name, Player1Name]),
    Player1ProcessID ! {play_game_with_gameid, NewGameId},
    Player2ProcessID ! {play_game_with_gameid, NewGameId},
    game_engine(PlayerData, Players, MasterProcess, NewGameId, GameResults, DisqualifiedBacklog, MovesRecord).

%% Handles the disqualification of a player.
handle_player_disqualification(Name, PlayerData, Players, MasterProcess, GameID, GameResults, DisqualifiedBacklog, MovesRecord) ->
    NewPlayers = lists:filter(fun({PlayerName, _}) -> PlayerName =/= Name end, Players),
    NewPlayersRemoved = 
        case lists:member(Name, DisqualifiedBacklog) of
            true -> DisqualifiedBacklog;
            false -> [Name | DisqualifiedBacklog]
        end,
    if length(NewPlayers) =:= 1 ->
        declare_winner(NewPlayers, PlayerData, DisqualifiedBacklog, GameID);
    true ->
        game_engine(PlayerData, NewPlayers, MasterProcess, GameID, GameResults, NewPlayersRemoved, MovesRecord)
    end.

%% Declares the winner and ends the tournament.
declare_winner([Winner], PlayerData, DisqualifiedBacklog, GameID) ->
    {TournamentWinner, TournamentWinnerCredits} = Winner,
    io:format("\n\nWe have a winner!\n\n"),
    io:format("** Tournament Over **\n\n"),
    io:format("Players~n"),
    WinnerCreditsUsed = credit_check(TournamentWinner, PlayerData) - TournamentWinnerCredits,
    io:format("~s: credits used: ~p, credits remaining: ~p~n", [TournamentWinner, WinnerCreditsUsed, TournamentWinnerCredits]),
    lists:foreach(
    fun(Name) ->
        Credit = credit_check(Name, PlayerData),
        FormattedString = io_lib:format("~s: credits used: ~p, credits remaining: 0~n", [Name, Credit]),
        io:format("~s", [lists:flatten(FormattedString)])
    end,
    DisqualifiedBacklog
    ),
    io:format("\n-------------------~n"),
    io:format("Total Games: ~p~n", [GameID]),
    io:format("\n\nWinner: ~p~n~n\n", [TournamentWinner]),
    halt().

%% Handles the result of a game.
handle_game_result(GameId, Result, PlayerData, Players, MasterProcess, GameResults, DisqualifiedBacklog, MovesRecord) ->
    NewGameResults = [{GameId, Result} | GameResults],
    game_engine(PlayerData, Players, MasterProcess, GameId, NewGameResults, DisqualifiedBacklog, MovesRecord).

%% Updates the credits of a player.
update_player_credits(LostPlayerName, NewCredits, PlayerData, Players, MasterProcess, GameID, GameResults, DisqualifiedBacklog, MovesRecord) ->
    UpdatedPlayers = lists:map(
        fun({Name, Credits}) ->
            if Name =:= LostPlayerName -> {Name, NewCredits};
               true -> {Name, Credits}
            end
        end, Players),
    game_engine(PlayerData, UpdatedPlayers, MasterProcess, GameID, GameResults, DisqualifiedBacklog, MovesRecord).

%% Handles a game that ends in a tie.
handle_game_tied(GameId, Player1ProcessID, Player2ProcessID, PlayerData, Players, MasterProcess, GameResults, DisqualifiedBacklog, MovesRecord) ->
    Player1ProcessID ! {game_tied, GameId},
    Player2ProcessID ! {game_tied, GameId},
    game_engine(PlayerData, Players, MasterProcess, GameId, GameResults, DisqualifiedBacklog, MovesRecord).


%% Handles the strategy for a game based on the player's move.
%% Updates game results and records moves.
strategy_handler(PlayerData, GameID, PlayerPid, Move, Players, MasterProcess, GameResults, DisqualifiedBacklog, MovesRecord) ->
    case lists:keyfind(GameID, 1, GameResults) of
        false ->
            % First move in a new game
            NewGameResults = [{GameID, [{PlayerPid, Move}]} | GameResults],
            game_engine(PlayerData, Players, MasterProcess, GameID, NewGameResults, DisqualifiedBacklog, MovesRecord);
        {GameID, Moves} ->
            % Retrieve existing moves and add the new move
            [{Player1ProcessID, Move1}, {Player2ProcessID, Move2}] = [lists:nth(1, Moves), {PlayerPid, Move}],
            NewMovesRecord = [{GameID, {Player1ProcessID, Move1}, {Player2ProcessID, Move2}} | MovesRecord],
            
            % Determine the winner based on the moves
            Result = determine_winner(Move1, Move2),
            
            % Lookup player names by their process IDs
            {_, Player1Name} = lookup_name_by_process_id(Player1ProcessID),
            {_, Player2Name} = lookup_name_by_process_id(Player2ProcessID),
            
           % Handle the result of the game
            case Result of
                tie ->
                    self() ! {game_tied, GameID, Player1ProcessID, Player2ProcessID};
                lost ->
                    Player1ProcessID ! {main_result, GameID, Result, Player1Name, Move1, Player2Name, Move2, Player1Name, NewMovesRecord};
                _ ->
                    Player2ProcessID ! {main_result, GameID, lost, Player1Name, Move1, Player2Name, Move2, Player2Name, NewMovesRecord}
            end,
            NewGameResults = lists:keydelete(GameID, 1, GameResults),
            game_engine(PlayerData, Players, MasterProcess, GameID, NewGameResults, DisqualifiedBacklog, NewMovesRecord)
    end.


determine_winner(Player1Move, Player2Move) ->
    case {Player1Move, Player2Move} of
        {rock, scissors} -> win;
        {scissors, paper} -> win;
        {paper, rock} -> win;
        {scissors, rock} -> lost;
        {paper, scissors} -> lost;
        {rock, paper} -> lost;
        _ -> tie
    end.

credit_check(Name, PlayerData) ->
    case lists:keyfind(Name, 1, PlayerData) of
        {Name, Credit} -> Credit;
        false -> undefined
    end.

lookup_name_by_process_id(Pid) ->
    lists:foldl(
        fun(Name, Acc) -> case whereis(Name) of Pid -> {ok, Name}; _ -> Acc end end,
        {error, not_found},
        erlang:registered()
    ).



