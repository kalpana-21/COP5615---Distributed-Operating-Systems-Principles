-module(gossip).
-import(neighbor,[findNeighbours/3]).
-export([start_gossip/4,spawner/5,sendNei/3,supervisor/3,checkConvergence/2,actor/3,childactor/1,childactorinit/1]).


start_gossip(NumProc, Topology, GossipLimit,FaultyNumber) ->
  ProcList = [],
  spawner(NumProc,Topology,ProcList,GossipLimit,FaultyNumber ).

spawner(0,Topology,ProcList,GossipLimit,FaultyNumber) ->
   register(supervisor_name,spawn(gossip,supervisor,[ProcList,Topology,FaultyNumber]));

spawner(NumProc,Topology, ProcList, GossipLimit,FaultyNumber ) ->
  Pid =0,
  ProcessId = spawn('gossip',actor,[GossipLimit+1,GossipLimit,Pid]),
  ProcList_temp = lists:append(ProcList,[ProcessId]),
  spawner(NumProc-1,Topology, ProcList_temp,GossipLimit,FaultyNumber).

sendNei(0,ProcList,Topology)->
  ok;
sendNei(Count,ProcList,Topology) ->
  ProcessId = lists:nth(Count, ProcList),
  Nei_list = findNeighbours(Topology,ProcList,Count),
  ProcessId ! {Nei_list},
  sendNei(Count-1, ProcList,Topology).

supervisor(ProcList,Topology,FaultyNumber) ->
  {GossipStartTime,_} = statistics(wall_clock),
  sendNei(length(ProcList),ProcList,Topology),
  NewProcList = killNode(ProcList,FaultyNumber),
  FirstProcessId = lists:nth(rand:uniform(length(NewProcList)),NewProcList),
  register(checkConvergencename,spawn(gossip,checkConvergence,[length(NewProcList),GossipStartTime])),
  FirstProcessId ! {ok,gossip}.

killNode(ProcList,0)->
  ProcList;
killNode(ProcList,KillNum) ->
  NodeToKill = lists:nth(rand:uniform(length(ProcList)),ProcList),
  NewList = lists:delete(NodeToKill,ProcList),
       NodeToKill  ! {ok,killChild,suicide},
  killNode(NewList,KillNum-1).


checkConvergence(0,GossipStartTime) ->
  {Endtime,_} = statistics(wall_clock),
  io:fwrite("\n\nConvergence time is ~wms\n",[Endtime-GossipStartTime]);
checkConvergence(Count,GossipStartTime)->
  receive
    dead ->
      ok;
    {ok,count} ->
      io:fwrite("Total number of processes not converged are ~w\n",[Count])

  end,
  checkConvergence(Count-1,GossipStartTime).


actor(0,GossipLimit,Pid) ->
  exit(Pid,normal),
  io:fwrite("\n~w Process converged ",[self()]),
  checkConvergencename ! dead,
  exit(normal);

actor(Count,GossipLimit,Pid)->
  receive
    {Nei_list} ->
      ChildProcessId =spawn(gossip,childactorinit,[Nei_list]),
      actor(Count-1,GossipLimit,ChildProcessId);
    {ok,gossip} ->
      if
        Count == GossipLimit ->
          Pid ! startgossip;
        true ->
          ok
      end;
      {ok,killChild,suicide} ->
        exit(Pid,normal),
        exit(normal)
  end,
  actor(Count-1,GossipLimit,Pid).

childactorinit(Nei_list) ->
  receive
    startgossip ->
      childactor(Nei_list)
  end.

childactor(Nei_list) ->
  ProcessId = lists:nth(rand:uniform(length(Nei_list)),Nei_list),
  ProcessId ! {ok,gossip} ,
  childactor(Nei_list).