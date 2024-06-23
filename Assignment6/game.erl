-module(game).
-export([start/1]).

-define(TIMEOUT, 5000). % Timeout value in milliseconds for receives.

start(Args) ->
    PlayerFile = lists:nth(1, Args),
    {ok, PlayerInfo} = file:consult(PlayerFile),
    io:format("** Rock, Paper Scissors World Championship ** \n \n ~n"),
    io:format("Starting game log... \n \n ~n"),
    Master = master_function(PlayerInfo),
    lists:foreach(
        fun({Name, Credits}) ->
            create_players(Name, Credits, Master, PlayerInfo)
        end,
        PlayerInfo
    ),
    tournament_loop(PlayerInfo, Master).

create_players(Name, Credits, Master, PlayerInfo) ->
    PlayerPid = spawn(fun() -> player(Name, Credits, PlayerInfo, Master) end),
    register(Name, PlayerPid).

player(Name, Credits, PlayerInfo, Master) ->
    Name ! {request_new_player, Name},
    game_handler(PlayerInfo, Master, Name).

game_handler(PlayerInfo, Master, Name) ->
    receive
        {request_new_player, Name} ->
            RandomPlayer = choose_random_player(PlayerInfo, Name),
            Name ! {player_ready, RandomPlayer},
            start_game(Name, PlayerInfo, Master),
            game_handler(PlayerInfo, Master, Name);
        {timeout, _Ref, {request_new_player, Name}} ->
            io:format("Timeout waiting for request_new_player message for ~p~n", [Name]),
            game_handler(PlayerInfo, Master, Name)
    end.

start_game(Name, PlayerInfo, Master) ->
    receive
        {player_ready, Opponent} ->
            {OpponentName, _} = Opponent,
            Master ! {new_game, Name, OpponentName},
            loop(PlayerInfo, Master)
    after ?TIMEOUT ->
            io:format("Timeout waiting for player_ready message for ~p~n", [Name]),
            start_game(Name, PlayerInfo, Master)
    end.

tournament_loop(PlayerInfo, Master) ->
    Players = lists:map(
        fun({Name, _Credits}) -> {Name, self(), _Credits} end,
        PlayerInfo
    ),
    loop(PlayerInfo, Players, 0, Master).

loop(PlayerInfo, Players, GameID, Master) ->
    receive
        {new_game, Name, Opponent} ->
            NewGameID = GameID + 1,
            CurrentPlayerInfo = find_player(Name, Players),
            case CurrentPlayerInfo of
                {Name, Pid, Credits} when Credits =< 0 ->
                    io:format("~w Disqualified and game ended ~p ~n", [Credits, Name]),
                    loop(PlayerInfo, Players, NewGameID, Master);
                {Name, Pid, Credits} ->
                    OpponentInfo = find_player(Opponent, Players),
                    case OpponentInfo of
                        {OpponentName, OpponentPid, OpponentCredits} when OpponentCredits =< 0 ->
                            io:format("~w Disqualified ~p ~n", [OpponentCredits, OpponentName]),
                            Master ! {reselect_player, Name, NewGameID},
                            loop(PlayerInfo, Players, NewGameID, Master);
                        {OpponentName, OpponentPid, OpponentCredits} ->
                            io:format("+ [~p] New game for ~p -> ~p, ~p ~n", [NewGameID, Name, OpponentName, self()]),
                            Master ! {play_game, Name, OpponentName, NewGameID},
                            loop(PlayerInfo, Players, NewGameID, Master)
                    end
            end;
        {play_game, Name, Opponent, GameID} ->
            {Move1, Move2, PlayerLooses} = decide_winner(Name, Opponent),
            PlayerLoosesInfo = find_player(PlayerLooses, Players),
            case PlayerLoosesInfo of
                {PlayerLoosesName, PlayerLoosesPid, PlayerLoosesCredits} when PlayerLoosesCredits =< 0 ->
                    io:format("$ (~p) ~p:~p -> ~p:~p = ~p loses (~p credits left)~n", [GameID, Name, Move1, Opponent, Move2, PlayerLoosesName, PlayerLoosesCredits - 1]),
                    NewPlayers = [Player || Player <- Players, Player /= PlayerLoosesInfo],
                    loop(PlayerInfo, NewPlayers, GameID, Master);
                {PlayerLoosesName, PlayerLoosesPid, PlayerLoosesCredits} ->
                    io:format("$ (~p) ~p:~p -> ~p:~p = ~p loses (~p credits left)~n", [GameID, Name, Move1, Opponent, Move2, PlayerLoosesName, PlayerLoosesCredits - 1]),
                    loop(PlayerInfo, Players, GameID, Master)
            end;
        {reselect_player, Name, GameID} ->
            RandomPlayerInfo = choose_random_player_from_players(Players, Name),
            case RandomPlayerInfo of
                {OpponentName, OpponentPid, OpponentCredits} when OpponentCredits =< 0 ->
                    FilteredPlayers = [Player || Player <- Players, Player /= RandomPlayerInfo],
                    io:format("~w is less than 10 for opponent ~p ~n", [OpponentCredits, OpponentName]),
                    Master ! {reselect_player, Name, GameID},
                    loop(PlayerInfo, FilteredPlayers, GameID, Master);
                {OpponentName, OpponentPid, OpponentCredits} ->
                    io:format("+ [~p] New game for ~p -> ~p~n", [GameID, Name, OpponentName]),
                    Master ! {play_game, Name, OpponentName, GameID},
                    loop(PlayerInfo, Players, GameID, Master)
            end
    end.

find_player(Name, Players) ->
    case lists:keyfind(Name, 1, Players) of
        false ->
            not_found;
        {_, Pid, Credits} ->
            {Name, Pid, Credits}
    end.

choose_random_player(PlayerInfo, Name) ->
    RandomIndex = rand:uniform(length(PlayerInfo)),
    RandomPlayer = lists:nth(RandomIndex, PlayerInfo),
    case RandomPlayer of
        {Name, _} -> choose_random_player(PlayerInfo, Name);
        _ -> RandomPlayer
    end.

choose_random_player_from_players(Players, Name) ->
    RandomIndex = rand:uniform(length(Players)),
    RandomPlayer = lists:nth(RandomIndex, Players),
    case RandomPlayer of
        {Name, _, _} -> choose_random_player_from_players(Players, Name);
        _ -> RandomPlayer
    end.

decide_winner(Player1, Player2) ->
    Move1 = random_move(),
    Move2 = random_move(),
    case {Move1, Move2} of
        {"rock", "scissors"} -> {Move1, Move2, Player2};
        {"scissors", "rock"} -> {Move1, Move2, Player1};
        {"scissors", "paper"} -> {Move1, Move2, Player2};
        {"paper", "scissors"} -> {Move1, Move2, Player1};
        {"paper", "rock"} -> {Move1, Move2, Player2};
        {"rock", "paper"} -> {Move1, Move2, Player1};
        {_, _} -> decide_winner(Player1, Player2)
    end.

random_move() ->
    Moves = ["rock", "paper", "scissors"],
    lists:nth(rand:uniform(3), Moves).
