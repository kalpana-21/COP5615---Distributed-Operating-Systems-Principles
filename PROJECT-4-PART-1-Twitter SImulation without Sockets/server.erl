-module(server).
-export([start_server/0, api_handler/0, tweets_subscribed_to/1, tweets_with_hashtag/2, tweets_with_mention/1, get_my_tweets/1, get_followers/1]).


start_server() ->
  ets:new(clientsregistry, [set, public, named_table]),
  ets:new(tweets, [set, public, named_table]),
  ets:new(hashtags_mentions, [set, public, named_table]),
  ets:new(subscribedto, [set, public, named_table]),
  ets:new(followers, [set, public, named_table]),
  global:register_name(twitterServer, spawn(server, api_handler, [])).

api_handler() ->
  receive
    {registerUser, UserId, Pid} ->
      register_user(UserId, Pid),
      Pid ! {registerConfirmation};
    {tweet, TweetString, UserId} ->
      process_tweet(TweetString, UserId);
    {tweetsSubscribedTo, UserId} ->
      spawn(server, tweets_subscribed_to, [UserId]);
    {tweetsWithHashtag, Hashtag, UserId} ->
      spawn(server, tweets_with_hashtag, [Hashtag, UserId]);
    {tweetsWithMention, UserId} ->
      spawn(server, tweets_with_mention, [UserId]);
    {getMyTweets, UserId} ->
      spawn(server, get_my_tweets, [UserId]);
    {addSubscriber, UserId, SubId} ->
      add_subscribed_to(UserId, SubId),
      add_followers(SubId, UserId);
    {disconnectUser, UserId} ->
      disconnect_user(UserId);
    {loginUser, UserId, Pid} ->
      ets:insert(clientsregistry, {UserId, Pid})
  end,
  api_handler().

register_user(UserId, Pid) ->
  ets:insert(clientsregistry, {UserId, Pid}),
  ets:insert(tweets, {UserId, []}),
  ets:insert(subscribedto, {UserId, []}),
  L = ets:lookup(followers, UserId),
  if L == [] ->
    ets:insert(followers, {UserId, []})
  end.

disconnect_user(UserId) ->
  ok.

get_tweets(UserId) ->
  L = ets:lookup(tweets, UserId),
  if L == [] ->
    [];
    true ->
      [Tup] = L,
      {_, List} = Tup,
      List
  end.

get_my_tweets(UserId) ->
  [Tup] = ets:lookup(tweets, UserId),
  {_, List} = Tup,
  Pid = where_is(UserId),
  Pid ! {repGetMyTweets, List}.

get_subscribed_to(UserId) ->
  [Tup] = ets:lookup(subscribedto, UserId),
  {_, List} = Tup,
  List.

get_followers(UserId) ->
  [Tup] = ets:lookup(followers, UserId),
  {_, List} = Tup,
  List.

add_subscribed_to(UserId, Sub) ->
  [Tup] = ets:lookup(subscribedto, UserId),
  {_, List} = Tup,
  List2 = [Sub | List],
  ets:insert(subscribedto, {UserId, List2}).

add_followers(UserId, Foll) ->
  L = ets:lookup(followers, UserId),
  if L == [] ->
    ets:insert(followers, {UserId, []});
    true -> ok
  end,
  [Tup] = L,
  {_, List} = Tup,
  List2 = [Foll | List],
  ets:insert(followers, {UserId, List2}).

fetch_matching([], Regex, List) ->
  List;

fetch_matching(String, Regex, List) ->
  io:format("String is: ~p~n", [String]),
  A = re:run(String, Regex),
  if A /= nomatch ->
    io:format("~w", [A]),
    {_, [Tup]} = A,
    {SI, Len} = Tup,
    FullLen = string:length(String),
    Check = SI + 1 + Len,
    io:format("~nYes"),
    io:format("~n ~w ~w", [SI + 1, Len]),
    io:format("~n String: ~p String Completed~n", [String]),

    S = string:substr(String, SI + 1, Len),
    io:format("~n ~p", [S]),
    List2 = List ++ S,
    io:format("~nList2 is: ~p~n", [List2]),
    List2;
    true ->
      List
  end.

for_tags([], _) ->
  ok;
for_tags(List, TweetString) ->
  HashTag = lists:nth(1, List),
  List2 = lists:delete(HashTag, List),
  insert_tags(HashTag, TweetString),
  for_tags(List2, TweetString).

for_mentions([], _) ->
  ok;
for_mentions(MentionUser, TweetString) ->
  Mention = MentionUser,
  insert_tags(Mention, TweetString),
  UserName = string:slice(Mention, 1, string:length(Mention) - 1),
  if UserName /= [] ->
    Pid = where_is(UserName),
    if Pid /= nil ->
      Pid ! {live, TweetString}
    end;
    true -> ok
  end.

for_followers([], _) ->
  ok;
for_followers(List, TweetString) ->
  Follower = lists:nth(1, List),
  List2 = lists:delete(Follower, List),
  if
    Follower /= [] ->
      Pid = where_is(Follower),
      if Pid /= [] ->
        Pid ! {live, TweetString}
      end;
    true ->
      ok
  end,
  for_followers(List2, TweetString).


process_tweet(TweetString, UserId) ->
  UserTweets = ets:lookup(tweets, UserId),
  List = if UserTweets == [] ->
    [{"", []}];
           true ->
             UserTweets
         end,
  [{_, TweetList}] = List,
  List2 = [TweetString | TweetList],
  ets:insert(tweets, {UserId, List2}),
  HashtagsList = fetch_matching(TweetString, "#+[a-zA-Z0-9(_)]{1,}", []),
  for_tags(HashtagsList, TweetString),
  MentionsList = fetch_matching(TweetString, "@+[a-zA-Z0-9(_)]{1,}", []),
  for_mentions(MentionsList, TweetString),
  FList = ets:lookup(followers, UserId),
  FollowersList = if FList == [] ->
    [];
                    true ->
                      [{_, X}] = FList,
                      X
                  end,
  for_followers(FollowersList, TweetString).


insert_tags(Tag, TweetString) ->
  L = ets:lookup(hashtags_mentions, Tag),
  [Tup] = if L /= [] ->
    L;
            true ->
              [nil]
          end,
  if Tup == nil ->
    ets:insert(hashtags_mentions, {Tag, [TweetString]});
    true ->
      {_, List} = Tup,
      List2 = [TweetString | List],
      ets:insert(hashtags_mentions, {Tag, List2})
  end.

tweets_subscribed_to(UserId) ->
  SubscribedTo = get_subscribed_to(UserId),
  List = generate_tweet_list(SubscribedTo, []),
  Pid = where_is(UserId),
  Pid ! {repTweetsSubscribedTo, List}.

generate_tweet_list([], TweetList) ->
  TweetList;
generate_tweet_list([Head | Tail], TweetList) ->

  TweetList2 = [get_tweets(Head) | TweetList],
  generate_tweet_list(Tail, TweetList2).

tweets_with_hashtag(HashTag, UserId) ->
  L = ets:lookup(hashtags_mentions, HashTag),
  [Tup] = if L /= [] ->
    L;
            true ->
              [{"#", []}]
          end,
  {_, List} = Tup,
  Pid = where_is(UserId),
  Pid ! {repTweetsWithHashTag, List}.

tweets_with_mention(UserId) ->
  L = ets:lookup(hashtags_mentions, "@" ++ UserId),
  [Tup] = if L /= [] ->
    L;
            true ->
              [{"@", []}]
          end,
  {_, List} = Tup,
  Pid = where_is(UserId),
  Pid ! {repTweetsWithMention, List}.

where_is(UserId) ->
  L = ets:lookup(clientsregistry, UserId),
  if L == [] ->
    nil;
    true ->
      [Tup] = ets:lookup(clientsregistry, UserId),
      {_, List} = Tup,
      List
  end.