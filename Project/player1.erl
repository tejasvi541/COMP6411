-module(player1).
-export([init/3, get_credits/1, get_info/1]).

init(Parent, Name, Credits) ->
    loop(Name, Credits, Parent).

loop(Name, Credits, Parent) ->
    receive
        {play, GameID, Opponent} ->
            if Credits > 0 ->
                timer:sleep(rand:uniform(90) + 10),
                Parent ! {request_game, Name},
                OpponentPID = whereis(Opponent),
                case Credits > 0 of
                    true ->
                        % OpponentPID ! {request_game, Name},
                        Parent ! {move, GameID, Name, random_move()};
                    false ->
                        Parent ! {disqualified, Name}
                end,
                loop(Name, Credits - 1, Parent);
            true ->
                Parent ! {disqualified, Name},
                loop(Name, Credits, Parent)
            end;
        {game_result, _GameID, _Player, _Opponent, Result} ->
            NewCredits = case Result of
                win -> Credits;
                tie -> Credits;
                lose -> Credits - 1
            end,
            loop(Name, NewCredits, Parent);
        {request_game, _Opponent} ->
            loop(Name, Credits, Parent);
        {disqualified, _} ->
            Parent ! {disqualified, Name},
            loop(Name, Credits, Parent);
        stop -> ok
    end.

get_credits(PID) ->
    PID ! {get_credits, self()},
    receive
        {credits, Credits} -> Credits
    end.

get_info(PID) ->
    PID ! {get_info, self()},
    receive
        {info, Name, Credits} -> {Name, Credits}
    end.

random_move() ->
    Moves = [rock, paper, scissors],
    lists:nth(rand:uniform(3), Moves).