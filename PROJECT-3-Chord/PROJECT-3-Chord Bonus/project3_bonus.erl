-module(project3_bonus).
-export([start/0, startChord/2, findM/1, makePeers/5, sendTable/2,tableData/4,passTable/3, completed/2, node/4,findSucc/4,closePeer/3,successor/2,predecessor/2]).

start() ->
  {ok,[NoNodes]}= io:fread("Enter the number of nodes: ","~d"),
  {ok,[NoReqs]}= io:fread("Enter the number of requests: ","~d"),
  {ok,[NoofFailNodes]}= io:fread("Enter the number of Fail Nodes: ","~d"),
  register(main, spawn(project3_bonus, startChord, [NoNodes-NoofFailNodes, NoReqs])).

startChord(NoNodes, NoReqs) ->
  M = findM(NoNodes),
  [Peers, Stat] = makePeers([], round(math:pow(2, M)), M, NoNodes, dict:new()),
  sendTable(Stat,M),
  pingAndKill(Peers, NoNodes, NoReqs, M, Stat).

findM(NoNodes) ->
  trunc(math:ceil(math:log2(NoNodes))).

makePeers(Peers, _, _, 0, Stat) ->
  [Peers, Stat];
makePeers(Peers, TotNodes, M, NoNodes, Stat) ->
  [HashKey, NewStat] = joinChord(Peers, TotNodes,  M, Stat),
  makePeers(lists:append(Peers, [HashKey]), TotNodes, M, NoNodes - 1, NewStat).

tableData(_, [], Dictionary,_) ->
  Dictionary;

tableData(Stat, NetList, Dictionary,M) ->
  [Head | Tail] = NetList,
  Tables = table_Data(Head, Stat,M, 0,[]),
  tableData(Stat, Tail, dict:store(element(1, Head), Tables, Dictionary), M).

table_Data(_, _, M, M,FLis) ->
  FLis;
table_Data(Node, Stat, M, Index, FLis) ->
  HashKey = element(1, Node),
  Succ_i = findSucc(HashKey, Stat, trunc(math:pow(2, Index)), M),
  table_Data(Node, Stat, M, Index + 1, FLis ++ [{Succ_i, dict:fetch(Succ_i, Stat)}] ).

passTable([], _, _) ->
  ok;
passTable(Peers, Stat, Tables) ->
  [First|Rest] = Peers,
  Pid = dict:fetch(First ,Stat),
  Pid ! {updateFing, dict:from_list(dict:fetch(First, Tables))},
  passTable(Rest, Stat, Tables).

sendTable(Stat,M) ->
  FingerTables = tableData(Stat, dict:to_list(Stat), dict:new(),M),
  passTable(dict:fetch_keys(FingerTables), Stat, FingerTables).

completed(0, NoOfHops) ->
  main ! {totalhops, NoOfHops};

completed(NoReqs, NoOfHops) ->
  receive
    {finished, _Pid, HopsCountForTask, _Key} ->
      completed(NoReqs - 1, NoOfHops + HopsCountForTask)
  end.

calcHops() ->
  receive
    {totalhops, NoOfHops} ->
      NoOfHops
  end.
pingAndKill(Peers, NumNodes, NumRequest, M, Stat) ->
  register(monitor, spawn(project3_bonus, completed, [NumNodes * NumRequest, 0])),
  pingAllPeers(Peers, NumRequest, M, Stat),
  Hops = calcHops(),
  io:format("~n Average Number of Hops = ~p  ", [Hops/(NumNodes * NumRequest)]),
  io:format("~n Total Number of Nodes = ~p  ", [NumNodes]),
  io:format("~n Logarithm of number of nodes to the base2 = ~p  ", [math:log2(NumNodes)]),
  terminatePeers(Peers, Stat).

terminatePeers([], _) ->
  ok;
terminatePeers(Peers, Stat) ->
  [First | Rest] = Peers,
  giveProcessId(First, Stat) ! {terminate},
  terminatePeers(Rest, Stat).

giveProcessId(Hash, Stat) ->
  case dict:find(Hash, Stat) of
    error -> nil;
    _ -> dict:fetch(Hash, Stat)
  end.

pingAllPeers(_, 0, _, _) ->
  ok;
pingAllPeers(Peers, NumRequest, M, Stat) ->
  timer:sleep(1000),
  Key = lists:nth(rand:uniform(length(Peers)), Peers),
  pingChordPeer(Key, Peers, Stat),
  pingAllPeers(Peers, NumRequest - 1, M, Stat).

randomPeer(PeerId, []) -> PeerId;
randomPeer(_, Peers) -> lists:nth(rand:uniform(length(Peers)), Peers).


joinChord(Peers, TotPeers, M, Stat) ->
  RemainingHashes = lists:seq(0, TotPeers - 1, 1) -- Peers,
  Hash = lists:nth(rand:uniform(length(RemainingHashes)), RemainingHashes),
  Pid = spawn(project3_bonus, node, [Hash, M, Peers, dict:new()]),
  [Hash, dict:store(Hash, Pid, Stat)].

node(Hash, M, Peers, _NodeState) ->
  FingerTable = lists:duplicate(M, randomPeer(Hash, Peers)),
  PeerForm = dict:from_list([{id, Hash}, {predecessor, nil}, {finger_table, FingerTable}, {next, 0}, {m, M}]),
  peerListen(PeerForm).

peerListen(PeerForm) ->
  HKey = dict:fetch(id, PeerForm),
  receive
    {search, Id, K, NoOfHops, _Pid} ->
      Peer = closePeer(K, dict:fetch_keys(dict:fetch(finger_table ,PeerForm)), PeerForm),
      State = PeerForm,
      if
        (HKey == K) ->
          monitor ! {finished, HKey, NoOfHops, K};
        (Peer == K) and (HKey =/= K) ->
          monitor ! {finished, HKey, NoOfHops, K};
        true ->
          dict:fetch(Peer, dict:fetch(finger_table, PeerForm)) ! {search, Id, K, NoOfHops + 1, self()}
      end;
    {terminate} ->
      State = PeerForm,
      exit("Exiting");
    {state, Pid} -> Pid ! PeerForm,
      State = PeerForm;
    {succ, Id, Pid} ->
      Successor = successor(Id, PeerForm),
      State = PeerForm,
      {Pid} ! {succResponse, Successor};
    {updateFing, FingerTable} ->
      State = dict:store(finger_table, FingerTable, PeerForm)
  end,
  peerListen(State).

successor(Id, PeerState) ->
  PredState = predecessor(Id, PeerState),
  dict:fetch(successor, PredState).

predecessor(Id, PeerState) ->
  case
    checkRange(dict:fetch(id, PeerState) + 1, dict:fetch(id, dict:fetch(successor, PeerState)), Id, dict:fetch(m, PeerState)) of
    true -> PeerState;
    _ -> predecessor(Id, nearPredecssor(Id, PeerState, dict:fetch(m, PeerState)))
  end.

nearPredecssor(_, PeerState, 0) ->
  PeerState;
nearPredecssor(Id, PeerState, M) ->
  MthFinger = lists:nth(M, dict:fetch(finger_table, PeerState)),

  case checkRange(dict:fetch(id, PeerState), Id, dict:fetch(node ,MthFinger), dict:fetch(m, PeerState)) of
    true ->

      dict:fetch(pid ,MthFinger) ! {state, self()},
      receive
        {statereply, FingerNodeState} ->
          FingerNodeState
      end;
    _ -> nearPredecssor(Id, PeerState, M - 1)
  end.

closePeer(Key, PeerIDs, State) ->
  case lists:member(Key, PeerIDs) of
    true -> Key;
    _ -> findNearest(Key, PeerIDs, -1, 10000000, State)
  end.

findNearest(_, [], MinPeer, _, _) ->
  MinPeer;
findNearest(Key, PeerIds, MinPeer, Min, PeerSt) ->
  [Head| Tail] = PeerIds,
  Dis = findDis(Key, Head, dict:fetch(m, PeerSt), 0),
  if
    Dis < Min ->
      findNearest(Key, Tail, Head, Dis, PeerSt);
    true ->
      findNearest(Key, Tail, MinPeer, Min, PeerSt)
  end.

findDis(K, K, _, Dis) ->
  Dis;
findDis(K, PeerId, M, Dis) ->
  findDis(K, (PeerId + 1) rem trunc(math:pow(2, M)), M, Dis + 1).

checkRange(Src, Target, K, M) ->
  if
    Src < Target ->
      (Src =< K) and (K =< Target);
    trunc(Src) == trunc(Target) ->
      trunc(K) == trunc(Src);
    Src > Target ->
      ((K >= 0) and (K =< Target)) or ((K >= Src) and (K < trunc(math:pow(2, M))))
  end.


findSucc(Key, S, Index,  M) ->
  case dict:find((Key + Index) rem trunc(math:pow(2, M)), S) of
    error ->
      findSucc(Key, S, Index + 1, M);
    _ -> (Key + Index) rem trunc(math:pow(2, M))
  end.

pingChordPeer(_, [], _) ->
  ok;
pingChordPeer(K, Peers, Stat) ->
  [Head | Tail] = Peers,
  ProcessId = giveProcessId(Head, Stat),
  ProcessId ! {search, Head, K, 0, self()},
  pingChordPeer(K, Tail, Stat).