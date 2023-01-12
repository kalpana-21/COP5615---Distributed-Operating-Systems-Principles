-module(client).
-export([start_client/4]).


twitter_server() ->
  global:whereis_name(twitterServer).

start_client(UserID, NoOfTweets, NoToSubscribe, ExistingUser) ->
  if ExistingUser ->
    io:format("~nUser #~p :- reconnected", [UserID]),
    login_handler(UserID);
    true ->
      ok
  end,
  global:send(twitterServer, {registerUser, UserID, self()}),
  receive
    {registerConfirmation} ->
      io:format("~nUser #~p :- registered on server", [UserID])
  end,
  client_handler(UserID, NoOfTweets, NoToSubscribe),

  receive
    _ -> ok
  end.


send_tweets(UserID, TweetCount) ->
  if
    TweetCount > 0 ->
      global:send(twitterServer, {tweet, "user " ++ UserID ++ " tweeting that " ++ randomizer(8) ++ " does not make sense", UserID}),
      send_tweets(UserID, TweetCount - 1);
    true ->
      ok
  end.

generate_subList(Count, SubCount, List) ->
  if
    Count == SubCount ->
      [Count | List];
    true ->
      generate_subList(Count + 1, SubCount, List)
  end.

current_time() ->
  os:system_time(millisecond).

login_handler(UserID) ->
  twitter_server() ! {loginUser, UserID, self()},
  send_tweets(UserID, 5),
  handle_liveView(UserID).

client_handler(UserID, NoOfTweets, NoToSubscribe) ->
  if
    NoToSubscribe > 0 ->
      SubList = generate_subList(1, NoToSubscribe, []),
      handle_zipf_subscribe(UserID, SubList);
    true ->
      ok
  end,

  Tweet_start_time = current_time(),
  io:format("Tweet Start time~p~n",[Tweet_start_time]),

  %Mentions
  UserToMention = randomizer(list_to_integer(UserID)),
  twitter_server() ! {tweet, "User " ++ UserID ++ " tweeting @" ++ UserToMention, UserID},

  %Hashtags
  twitter_server() ! {tweet, "User " ++ UserID ++ " tweeting that #COP5615isgreat", UserID},

  %Send Tweets
  send_tweets(UserID, NoOfTweets),
  %Retweets
  handle_retweet(UserID),
  Tweet_time_diff = current_time() - Tweet_start_time,

  %Queries
  Queries_subscribed_to_start_time = current_time(),
  handle_queries_subscribed_to(UserID),
  Queries_subscribed_to_time_diff = current_time() - Queries_subscribed_to_start_time,


  Queries_hashtag_time_start = current_time(),
  handle_queries_hashtag("#COP5615isgreat",UserID),
  Queries_hashtag_time_diff = current_time() - Queries_hashtag_time_start,

  Queries_mention_time_start = current_time(),
  handle_queries_mention(UserID),
  Queries_mention_time_diff = current_time() - Queries_mention_time_start,

  Queries_get_tweets_time_start = current_time(),
  %Get All Tweets
  handle_get_my_tweets(UserID),
  Queries_get_tweets_time_diff = current_time() - Queries_get_tweets_time_start,

  io:format("Handle queries for my tweets finished~n"),


  Final_Tweet_time_diff = Tweet_time_diff/(NoOfTweets+3),
  io:format("sending performance metrics : ~p~n", [{perfmetrics, Final_Tweet_time_diff, Queries_subscribed_to_time_diff, Queries_hashtag_time_diff, Queries_mention_time_diff, Queries_get_tweets_time_diff}]),
  global:whereis_name(mainproc) ! {perfmetrics, Final_Tweet_time_diff, Queries_subscribed_to_time_diff, Queries_hashtag_time_diff, Queries_mention_time_diff, Queries_get_tweets_time_diff},

  %Live View
  handle_liveView(UserID).

handle_zipf_subscribe(UserID, SubList) ->
  if
    SubList == [] ->
      ok;
    true ->
      [Current | Remaining] = SubList,
      twitter_server() ! {addSubscriber, UserID, integer_to_list(Current)},
      handle_zipf_subscribe(UserID, Remaining)
  end.

handle_retweet(UserID) ->
  global:send(twitterServer, {tweetsSubscribedTo, UserID}),
  TweetList = receive
                {repTweetsSubscribedTo, List} ->
                  io:format("Recieved retweet to response as ~p~n", [List]),
                  List
              end,


  if
    TweetList /= [] ->
      [ReTweet | _] = lists:nth(1,TweetList),
      twitter_server() ! {tweet, ReTweet ++ " -RT", UserID};
    true ->
      nothing
  end.

handle_liveView(UserID) ->
  receive
    {live, TweetString} ->
      io:format("~nUser " ++ UserID ++ " :- Live View -----"),
      io:format(TweetString)
  end,
  handle_liveView(UserID).

print_tweets(TweetList) ->
  if
    TweetList == [] ->
      ok;
    true ->
      [CurrentTweet | Remaining] = TweetList,
      io:format(CurrentTweet),
      print_tweets(Remaining)
  end.

handle_get_my_tweets(UserID) ->
  twitter_server() ! {getMyTweets, UserID},
  receive
    {repGetMyTweets, List} ->
      io:format("~nUser " ++ UserID ++ " :- All my tweets"),
      print_tweets(List)
  end.

handle_queries_subscribed_to(UserID) ->
  global:send(twitterServer, {tweetsSubscribedTo, UserID}),
  receive
    {repTweetsSubscribedTo, List} ->
      io:format("~nUser " ++ UserID ++ " :- Tweets Subscribed To"),
      io:format("received the ~p list~n",[List]),
      print_tweets(List)
  end.

handle_queries_hashtag(Tag, UserID) ->
  global:send(twitterServer, {tweetsWithHashtag, Tag, UserID}),
  receive
    {repTweetsWithHashTag, List} ->
      io:format("~nUser " ++ UserID ++ " :- Tweets With " ++ Tag),
      print_tweets(List)
  end.

handle_queries_mention(UserID) ->
  twitter_server() ! {tweetsWithMention, UserID},
  receive
    {repTweetsWithMention, List} ->
      io:format("~nUser " ++ UserID ++ " :- Tweets with @" ++ UserID),
      print_tweets(List)
  end.

randomizer(Num) ->
  integer_to_list(rand:uniform(Num)).




