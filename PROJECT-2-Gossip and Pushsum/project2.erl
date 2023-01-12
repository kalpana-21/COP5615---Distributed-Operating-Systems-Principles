-module(project2).
-import(gossip,[start_gossip/3]).
-import(pushsum,[start_pushsum/2]).
-export([start/4]).
start(Num,Topology,Algorithm,GossipLimit) ->

  if
    ((Topology == '2d') or (Topology == imp3d )) == true->
      NumProc = trunc(math:pow(round(math:sqrt(Num)),2));
    true ->
      NumProc = Num
  end,
  if
    Algorithm == gossip -> start_gossip(NumProc, Topology, GossipLimit);
    true ->
      start_pushsum(NumProc, Topology)
  end.
