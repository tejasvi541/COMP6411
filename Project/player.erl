-module(player).
-export([start/4, player_engine/4]).

start(Name, Credits, MasterProcess,Players) ->
    register(Name, self()),
    player_engine(Name, Credits, MasterProcess,Players).

player_engine(Name, Credits, MasterProcess,Players) ->
    receive
        {play_game, Opponent} ->
            if Credits =< 0 ->
                MasterProcess ! {player_disqualified, Name};
            true ->
                Opponent ! {invitation, self(), Name},
                player_engine(Name, Credits, MasterProcess,Players)
            end;

        {invitation, OpponentPid, OpponentName} ->
            if Credits =< 0 ->
                OpponentPid ! {game_rejected, self(), Name};
            true ->
                MasterProcess ! {game_request, self(), Name, OpponentPid, OpponentName}
            end,
            player_engine(Name, Credits, MasterProcess,Players);

        {play_game_with_gameid, GameId} ->
            Move = lists:nth(rand:uniform(3), [rock, paper, scissors]),            
            MasterProcess ! {match_move, GameId, self(), Move},
            player_engine(Name, Credits, MasterProcess,Players);

        {main_result, GameId, Result, P1Name, M1, P2Name, M2, LostPName, MoveRecords} ->
            UpdatedCredits = if Result =:= lost -> Credits - 1; true -> Credits end,
            RelevantGames = lists:filter(fun({GID, {_,_}, {_,_}}) -> GID == GameId end, MoveRecords),
            NumOfGames = length(RelevantGames),

            case NumOfGames > 1 of
                true ->
                    GameDetailsList = lists:foldl(
                        fun({_, {P1, P1M}, {P2, P2M}}, Acc) ->
                            {_, Name1} = lookup_name_by_process_id(P1),
                            {_, Name2} = lookup_name_by_process_id(P2),
                            GameDetailString = io_lib:format("~p:~p -> ~p:~p, ", [Name2, P2M, Name1, P1M]),
                            [GameDetailString | Acc]
                        end,
                        [],
                        RelevantGames
                    ),
                    FlattenedGameDetails = lists:flatten(GameDetailsList),
                    io:format("$ (~p) ~s = ~p ~p [~p Credits Remaining]~n", [GameId, FlattenedGameDetails, LostPName, Result, Credits]);
                false ->
                    io:format("$ (~p) ~p:~p -> ~p:~p = ~p ~p [~p Credits Left]~n", [GameId, P2Name, M2, P1Name, M1, LostPName, Result, UpdatedCredits])
            end,

            case UpdatedCredits =< 0 of
                true -> io:format("- (~p) Player ~p has been removed for insufficient credits~n", [GameId, Name]);
                false -> ok
            end,

            MasterProcess ! {update_players, LostPName, UpdatedCredits},
            player_engine(Name, UpdatedCredits, MasterProcess, Players);


        {game_tied, GameId} ->
            Move = lists:nth(rand:uniform(3), [rock, paper, scissors]),
            MasterProcess ! {match_move, GameId, self(), Move},
            player_engine(Name, Credits, MasterProcess,Players)

    after 10 + random:uniform(101)->
        OtherPlayers = lists:filter(fun(P) -> P =/= Name end,Players),
        if OtherPlayers =/= [] ->
            RandomOpponent = lists:nth(rand:uniform(length(OtherPlayers)), OtherPlayers),
            self() ! {play_game, RandomOpponent},
            player_engine(Name, Credits, MasterProcess,Players);

        true ->
            timer:sleep(101)
        end,
        player_engine(Name, Credits, MasterProcess,Players)
    end.

lookup_name_by_process_id(Pid) ->
    NameList = erlang:registered(),
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
        NameList
    ).
