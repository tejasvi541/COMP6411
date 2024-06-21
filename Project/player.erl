-module(player).
-export([start/4]).

start(Name, Credits, Master, Players) ->
    spawn(fun() -> loop(Name, Credits, Master, Players) end).

loop(Name, 0, Master, _Players) ->
    Master ! {disqualified, Name},
    io:format("Player ~p has been disqualified~n", [Name]),
    receive stop -> ok end;

loop(Name, Credits, Master, Players) ->
    Master ! {register, self(), Name, Credits},
    receive
        {game_request, GameID, Opponent} ->
            % io:format("Player ~p received game request ~p from ~p~n", [Name, GameID, Opponent]),
            if Credits > 0 ->
                self() ! {game_confirmed, GameID, Opponent}
            end,
            loop(Name, Credits, Master, Players);
        {game_confirmed, GameID, Opponent} ->
            io:format("+ [~p] new game for ~p -> ~p ~n", [GameID, Name, Opponent]),
            Move = random_move(),
            Opp_Move = random_move(),
            Master ! {move, GameID, Name, Move, Opponent, Opp_Move},
            loop(Name, Credits - 1, Master, Players);
        {move_result, GameID, Result} ->
            case Result of
                lose ->
                    loop(Name, Credits - 1, Master, Players);
                tie ->
                    Move = random_move(),
                    Master ! {move, GameID, Name, Move},
                    loop(Name, Credits, Master, Players);
                win ->
                    loop(Name, Credits, Master, Players)
            end;
        stop ->
            io:format("Player ~p stopping~n", [Name]),
            ok
    after 10 + rand:uniform(91) ->
            % Make a new game request
            RandomPlayer = random_player(Name, Players),
            io:format("Player ~p requesting new game with ~p~n", [Name, RandomPlayer]),
            Master ! {new_game, self(), Name, RandomPlayer},
            loop(Name, Credits, Master, Players)
    end.

random_move() ->
    case random:uniform(3) of
        1 -> rock;
        2 -> paper;
        3 -> scissors
    end.

random_player(Name, Players) ->
    FilteredPlayers = lists:filter(fun(Player) -> Player =/= Name end, Players),
    RandomIndex = rand:uniform(length(FilteredPlayers)),
    lists:nth(RandomIndex, FilteredPlayers).
