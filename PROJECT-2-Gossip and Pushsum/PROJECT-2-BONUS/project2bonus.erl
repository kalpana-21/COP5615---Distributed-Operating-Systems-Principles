-module(project2bonus).
-import(gossip,[start_gossip/4]).
-import(pushsum,[start_pushsum/3]).
-export([start/5]).
start(Num,Topology,Algorithm,GossipLimit,FaultyNumber) ->
  if
    ((Topology == '2d') or (Topology == imp3d )) == true->
      NumProc = trunc(math:pow(round(math:sqrt(Num)),2));
    true ->
      NumProc = Num
  end,
  if
    Algorithm == gossip -> start_gossip(NumProc, Topology, GossipLimit,FaultyNumber);
    Algorithm == pushsum ->
      start_pushsum(NumProc, Topology, FaultyNumber);
    true ->
      io:fwrite("\nAlgorithm ~w is invalid. Use either [gossip] or [pushsum]",[Algorithm])
  end.
