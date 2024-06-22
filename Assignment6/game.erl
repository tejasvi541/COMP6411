-module(game).
-export([start/1]).

start(Args) ->
    PlayerFile = lists:nth(1, Args),
    {ok, PlayerInfo} = file:consult(PlayerFile),
    io:format("** Rock, Paper Scissors World Championship ** \n \n ~n"),
    io:format("Starting game log... \n \n ~n"),
    Master = master_function(PlayerInfo),

    lists:foreach(fun({Name, Credits}) -> 
        % io:format("Spawning player process for ~p with ~p credits~n", [Name, Credits]),
        timer:sleep(200),
        create_players(Name, Credits, Master, PlayerInfo) 
    end, PlayerInfo).

create_players(Name,Credits,Master,PlayerInfo)->
    PlayerPid = spawn(fun() -> player(Name, Credits,PlayerInfo,Master) end),
    register(Name, PlayerPid).

player(Name, Credits,PlayerInfo,Master) ->
    Name!{request_new_player,Name},
    game_handler(PlayerInfo,Master,Name).


start_game(Name,PlayerInfo,Master) ->
    receive
        {player_ready,Opponent} ->
            {OpponentName, _} = Opponent,
            Master!{new_game,Name,OpponentName}
    end.          

choose_random_player(PlayerInfo, Name) ->
    RandomIndex = rand:uniform(length(PlayerInfo)),
    RandomPlayer = lists:nth(RandomIndex, PlayerInfo),
    case RandomPlayer of
        {Name, _} -> 
            choose_random_player(PlayerInfo, Name);
        _ -> 
            RandomPlayer
    end.

game_handler(PlayerInfo,Master,Name)->
    receive
        {request_new_player,Name} ->
            RandomPlayer = choose_random_player(PlayerInfo, Name),
            Name! {player_ready,RandomPlayer},
            start_game(Name,PlayerInfo,Master),
            game_handler(PlayerInfo,Master,Name)
         after 10 + random:uniform(69) ->
            Name!{request_new_player,Name},
            game_handler(PlayerInfo,Master,Name)
    end.


% -------------------MASTER PROCESS CAN BE SEPERATED INTO DIFFERENT FILE ---------------------------------------------------------------------------------------------------

master_function(PlayerInfo) ->
    Players = lists:map(fun({Name, _Credits}) -> {Name, self(), _Credits} end, PlayerInfo),
    spawn(fun() -> loop(PlayerInfo, Players,0) end).

loop(_PlayerInfo, [PlayerName],_GameID) ->
    io:format("Tournament is over~n"),
    ok;
loop(PlayerInfo, Players,GameID) ->
    receive
        {new_game, Name, Opponent} ->
            NewGameID = GameID + 1, 
            CurrentPlayerInfo = find_player(Name, Players),
            case CurrentPlayerInfo of
                {Name, Pid, Credits} ->
                    if(Credits =< 0) ->
                        io:format("~w Disqualified and game endded~p ~n", [Credits,Name]),
                        loop(PlayerInfo, Players, NewGameID);
                    true ->
                        OpponentInfo = find_player(Opponent, Players),
                        case OpponentInfo of
                            {OpponentName, OpponentPid, OpponentCredits} ->
                                if
                                    OpponentCredits =< 0 ->
                                        io:format("~w Disqualified ~p ~n", [OpponentCredits,OpponentName]),
                                        self()!{reselect_player,Name,NewGameID},
                                        loop(PlayerInfo, Players, NewGameID);
                                    true ->
                                        io:format("+ [~p] New game for ~p -> ~p, ~p ~n", [NewGameID,Name, OpponentName,self()]),
                                        self()!{play_game, Name, Opponent, NewGameID},
                                        loop(PlayerInfo, Players, NewGameID)
                                end;
                            not_found ->
                                io:format("Opponent ~p not found~n", [Opponent]),
                                self()!{reselect_player,Name,NewGameID},
                                loop(PlayerInfo, Players, NewGameID)
                        end
                    end;
                not_found ->
                    io:format("Player ~p not found~n", [Name]),
                    loop(PlayerInfo, Players, NewGameID-1)
            end;
        
        {play_game, Name, Opponent, GameID} ->

            {Move1,Move2,PlayerLooses} = decide_winner(Name, Opponent),

            PlayerLoosesInfo = find_player(PlayerLooses, Players),

            case PlayerLoosesInfo of
                {PlayerLoosesName, PlayerLoosesPid, PlayerLoosesCredits} ->
                    io:format("$ (~p) ~p:~p -> ~p:~p = ~p loses (~p credits left)~n", [GameID, Name, Move1, Opponent, Move2, PlayerLoosesName, PlayerLoosesCredits-1]),
                    NewPlayers = update_players_list(Players, PlayerLoosesName, PlayerLoosesCredits-1),
                    if(PlayerLoosesCredits-1 =< 0) ->
                        FilteredPlayers = [Player || Player <- Players, Player /= PlayerLoosesInfo],
                        io:format("New Players ~p with looser name ~p ~n", [FilteredPlayers,PlayerLoosesInfo]),
                        loop(PlayerInfo, FilteredPlayers, GameID);
                    true ->
                        loop(PlayerInfo, NewPlayers, GameID)
                    end;


                not_found ->
                    io:format("PlayerLoosesName ~p not found~n", [Opponent]),
                    loop(PlayerInfo, Players, GameID)
            end;

        {reselect_player,Name,GameID} ->
            RandomPlayerInfo = choose_random_player_from_players(Players, Name),
            case RandomPlayerInfo of
                {OpponentName,OpponentPid, OpponentCredits} ->
                    if
                        OpponentCredits =< 0 ->
                            FilteredPlayers = [Player || Player <- Players, Player /= RandomPlayerInfo],
                            io:format("~w is less than 10. for opponemt ~p ~n", [OpponentCredits,OpponentName]),
                            self()!{reselect_player,Name,GameID},
                            loop(PlayerInfo, FilteredPlayers, GameID);
                        true ->
                            io:format("+ [~p] New game1 for ~p -> ~p~n", [GameID,Name, OpponentName]),
                            self()!{play_game, Name, OpponentName, GameID},
                            loop(PlayerInfo, Players, GameID)
                    end;
                not_found ->
                    loop(PlayerInfo, Players, GameID)
            end
    end.



find_player(Name, Players) ->
    case lists:filter(fun({PlayerName, _, _}) -> PlayerName =:= Name end, Players) of
        [PlayerInfo] -> PlayerInfo;
        [] -> not_found
    end.


update_players_list(Players, Name, Credits) ->
    case Name of
        undefined -> Players;  
        _ ->update_credit_helper(Players,Name,Credits)
    end.

update_credit_helper(Players, NewName, NewCredits) ->
    lists:map(
        fun({Name, Pid, Credits}) ->
            case Name == NewName of
                true ->
                    {NewName, Pid, NewCredits};
                false ->
                    {Name, Pid, Credits}
            end
        end,
        Players
    ).




% -----------ABOUT GAME DESICISIONS--------------------------------------
random_move() ->
    Moves = ["rock", "paper", "scissors"],
    lists:nth(rand:uniform(3), Moves).


decide_winner(Player1,Player2) ->
    Move1 = random_move(),
    Move2 = random_move(),

    case {Move1, Move2} of
        {"rock", "scissors"} -> {Move1,Move2,Player2};
        {"scissors", "rock"} -> {Move1,Move2,Player1};
        {"scissors", "paper"} -> {Move1,Move2,Player2};
        {"paper", "scissors"} -> {Move1,Move2,Player1};
        {"paper", "rock"} -> {Move1,Move2,Player2};
        {"rock", "paper"} -> {Move1,Move2,Player1};
        {_, _} -> % If it's a draw, recursively call decide_winner
            decide_winner(Player1, Player2)
    end.


choose_random_player_from_players(Players, Name) ->
    RandomIndex = rand:uniform(length(Players)),
    RandomPlayer = lists:nth(RandomIndex, Players),
    case RandomPlayer of
        {Name, _,_} -> % 
            choose_random_player_from_players(Players, Name);
        _ -> 
            RandomPlayer
    end.