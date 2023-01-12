-module(gossip).
-import(neighbor,[findNeighbours/3]).
-export([start_gossip/3,spawner/4,sendNei/3,supervisor/2,checkConvergence/2,actor/3,childactor/1,childactorinit/1]).


start_gossip(NumProc, Topology, GossipLimit) ->
  ProcList = [],
  spawner(NumProc,Topology,ProcList,GossipLimit ).

spawner(0,Topology,ProcList,GossipLimit) ->
   register(supervisor_name,spawn(gossip,supervisor,[ProcList,Topology]));

spawner(NumProc,Topology, ProcList, GossipLimit ) ->
  Pid =0,
  ProcessId = spawn('gossip',actor,[GossipLimit+1,GossipLimit,Pid]),
  ProcList_temp = lists:append(ProcList,[ProcessId]),
  spawner(NumProc-1,Topology, ProcList_temp,GossipLimit).

sendNei(0,ProcList,Topology)->
  ok;
sendNei(Count,ProcList,Topology) ->
  ProcessId = lists:nth(Count, ProcList),
  Nei_list = findNeighbours(Topology,ProcList,Count),
  ProcessId ! {Nei_list},
  sendNei(Count-1, ProcList,Topology).

%%supervisor code
supervisor(ProcList,Topology) ->
  {GossipStartTime,_} = statistics(wall_clock),
  sendNei(length(ProcList),ProcList,Topology),
  FirstProcessId = lists:nth(rand:uniform(length(ProcList)),ProcList),
  FirstProcessId ! {ok,gossip},
  register(checkConvergencename,spawn(gossip,checkConvergence,[length(ProcList),GossipStartTime])).

checkConvergence(0,GossipStartTime) ->
  {Endtime,_} = statistics(wall_clock),
  io:fwrite("\n\nConvergence time is ~wms\n",[Endtime-GossipStartTime]);
checkConvergence(Count,GossipStartTime)->
  receive
    dead ->
      ok
  end,
  checkConvergence(Count-1,GossipStartTime).


actor(0,GossipLimit,Pid) ->
  exit(Pid,normal),
  io:fwrite("~w Process converged \n",[self()]),
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
      end
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