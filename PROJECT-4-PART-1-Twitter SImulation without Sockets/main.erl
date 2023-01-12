-module(main).
-import(client, [start_client/4]).
-import(server, [start_server/0]).
-export([start/0, start/3, main_listener/7]).

start() ->
  server:start_server(),
  receive
    _ -> ok
  end.

start(NumClients, MaxSubscribers, DisconnectClients) ->
  ClientsToDisconnect = DisconnectClients * (0.01) * NumClients,
  ets:new(mainregistry, [set, public, named_table]),

  ListenerPID = spawn(main, main_listener, [NumClients, NumClients, 0, 0, 0, 0, 0]),
  global:register_name(mainproc, ListenerPID),
  register(delegator, self()),
  StartTime = os:system_time(millisecond),
  io:format("~n start time is: ~w", [StartTime]),

  create_users(1, NumClients, MaxSubscribers),

  receive

    done -> ok
  end,
  TotalTime = StartTime - os:system_time(millisecond),
  io:format("~nTime taken for initial simulation to complete: ~p milliseconds", [TotalTime]),

  simulate_disconnection(NumClients, ClientsToDisconnect).


main_listener(0, TotalClients, Tweet_time_diff, Queries_subscribed_to_time_diff, Queries_hashtag_time_diff, Queries_mention_time_diff, Queries_get_tweets_time_diff) ->
  io:format( "~nAvg. time to tweet: ~p milliseconds", [Tweet_time_diff/TotalClients]),
  io:format( "~nAvg. time to query tweets subscribe to: ~p milliseconds", [Queries_subscribed_to_time_diff/TotalClients]),
  io:format( "~nAvg. time to query tweets by hashtag: ~p milliseconds", [Queries_hashtag_time_diff/TotalClients]),
  io:format( "~nAvg. time to query tweets by mention: ~p milliseconds", [Queries_mention_time_diff/TotalClients]),
  io:format( "~nAvg. time to query all relevant tweets: ~p milliseconds", [Queries_get_tweets_time_diff/TotalClients]),

  delegator ! done;


main_listener(NumClients, TotalClients, Tweet_time_diff, Queries_subscribed_to_time_diff, Queries_hashtag_time_diff, Queries_mention_time_diff, Queries_get_tweets_time_diff) ->
  receive
    {perfmetrics,A, B, C, D, E} ->
      io:format("~nPerformance metrics received for numclient : ~p ~n", [NumClients]),
      main_listener(NumClients - 1, TotalClients, Tweet_time_diff + A, Queries_subscribed_to_time_diff + B, Queries_hashtag_time_diff + C, Queries_mention_time_diff + D, Queries_get_tweets_time_diff + E)
  end.

create_users(Count, NoOfClients, TotalSubscribers) ->
  UserName = integer_to_list(Count),
  NumTweets = round(math:floor(TotalSubscribers/Count)),
  NoToSubscribe = round(math:floor(TotalSubscribers/(NoOfClients-Count+1))) - 1,
  PID = spawn(client, start_client, [UserName, NumTweets, NoToSubscribe, false]),
  ets:insert(mainregistry, {UserName, PID}),
  io:format("~nUser with ~w PID created",[PID]),
  if
    Count /= NoOfClients -> create_users(Count + 1, NoOfClients, TotalSubscribers);
    true -> ok
  end.

where_is(UserId) ->
  [{_,PID} | _ ] = ets:lookup(mainregistry, UserId),
  PID.

restart_client([]) ->
  ok;

restart_client(List) ->
  [CurrentUser | Remaining] = List,
  PID = spawn(client, start_client, [CurrentUser, -1, -1, true]),
  ets:insert(mainregistry, {CurrentUser, PID}),
  restart_client(Remaining).

simulate_disconnection(NumClients, ClientsToDisconnect) ->
  timer:sleep(1000),
  DisconnectList = handle_disconnection(NumClients, ClientsToDisconnect, 0, []),
  timer:sleep(1000),
  restart_client(DisconnectList),
  simulate_disconnection(NumClients, ClientsToDisconnect).

handle_disconnection(NumClients, ClientsToDisconnect, ClientsDisconnected, DisconnectList) ->
  if
    ClientsDisconnected < ClientsToDisconnect ->
      ClientID = integer_to_list(rand:uniform(NumClients)),
      ClientPID = where_is(ClientID),
      if
        ClientPID /= none ->
          global:whereis_name(twitterServer) ! {disconnectUser, ClientID},
          ets:insert(mainregistry, {ClientID, none}),
          exit(ClientPID, kill),
          io:format("Simulator :- User ~p has been disconnected~n",[ClientID]),
          handle_disconnection(NumClients, ClientsToDisconnect, ClientsDisconnected + 1, [ClientID | DisconnectList]);
        true ->
          handle_disconnection(NumClients, ClientsToDisconnect, ClientsDisconnected, DisconnectList)
      end;
    true ->
      DisconnectList
  end.

