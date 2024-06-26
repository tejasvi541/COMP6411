-module(player).
-export([init/4, player_engine/4]).

init(Name, Credits, Master,Players) ->
    register(Name, self()),
    player_engine(Name, Credits, Master, Players).

player_engine(Name, Credits, Master, Players) ->
    receive
        {play_game, Opponent} ->
            if Credits =< 0 ->
                Master ! {player_disqualified, Name};
            true ->
                Opponent ! {opponent_invitation, self(), Name},
                player_engine(Name, Credits, Master,Players)
            end;

        {opponent_invitation, OpponentPid, OpponentName} ->
            if Credits =< 0 ->
                OpponentPid ! {game_rejected, self(), Name};
            true ->
                Master ! {game_request, self(), Name, OpponentPid, OpponentName}
            end,
            player_engine(Name, Credits, Master,Players);

        {game_move, GameId} ->
            Move = random_move(),
            Master ! {game_move, GameId, self(), Move},
            player_engine(Name, Credits, Master,Players);

        {game_result, GameId, Result,Player1Name,Move1,Player2Name,Move2,LostPlayerName} ->
            NewCredits = if Result =:= lost -> Credits - 1; true -> Credits end,
            io:format("$ (~p) ~p:~p -> ~p:~p = ~p ~p [~p Credits Left]~n", [GameId, Player2Name, Move2,Player1Name,Move1,  LostPlayerName,Result,NewCredits]),
            player_engine(Name, NewCredits, Master,Players);

        {game_tied, GameId} ->
            Move = random_move(),
            Master ! {game_move, GameId, self(), Move},
            player_engine(Name, Credits, Master,Players);

        {deny_request} ->
            player_engine(Name, Credits, Master,Players)
    after 10 + random:uniform(101)->
        OtherPlayers = lists:filter(fun(P) -> P =/= Name end,Players),
        if OtherPlayers =/= [] ->
            RandomOpponent = lists:nth(rand:uniform(length(OtherPlayers)), OtherPlayers),
            self() ! {play_game, RandomOpponent},
            player_engine(Name, Credits, Master,Players);

        true ->
            timer:sleep(100)
        end,
        player_engine(Name, Credits, Master,Players)
    end.

random_move() ->
    Moves = [rock, paper, scissors],
    lists:nth(rand:uniform(3), Moves).


