-module(pushsum).
-import('neighbor',[findNeighbours/3,delete/2]).
-export([start_pushsum/3,spawner/4,sendNei/3,supervisor/3,actor/5,childactorinit/1,childactor/3,checkConvergence/2]).


start_pushsum(NumProc, Topology,FaultyNumber) ->
  ProcList = [],
  spawner(NumProc,Topology,ProcList,FaultyNumber ).

spawner(0, Topology, ProcList,FaultyNumber) ->
  register(supervisor_name,spawn(pushsum,supervisor,[ProcList,Topology,FaultyNumber]));

spawner(NumProc, Topology, ProcList,FaultyNumber) ->
  Pid =0,
  L=[],
  ProcessId = spawn(pushsum,actor,[NumProc,1,Pid,3,L]),
  ProcList_temp = lists:append(ProcList,[ProcessId]),
  spawner(NumProc-1, Topology, ProcList_temp,FaultyNumber).

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
  register(checkConvergencename,spawn(pushsum,checkConvergence,[length(NewProcList),GossipStartTime])),
  timer:sleep(10),
  FirstProcessId ! {0,0}.

killNode(ProcList,0)->
  ProcList;
killNode(ProcList,KillNum) ->
  NodeToKill = lists:nth(rand:uniform(length(ProcList)),ProcList),
  NewList = lists:delete(NodeToKill,ProcList),
  NodeToKill  ! {ok,killChild,suicide},
  killNode(NewList,KillNum-1).

checkConvergence(0,GossipStartTime) ->
  {Endtime,_} = statistics(wall_clock),
  io:fwrite("\n Convergence time is ~wms\n",[Endtime-GossipStartTime]);
checkConvergence(Count,GossipStartTime)->
  receive
    dead ->
      ok;
    {ok,count} ->
      io:fwrite("Total number of processes not converged are ~w\n",[Count])
  end,
  checkConvergence(Count-1,GossipStartTime).


actor(S,W,Pid,0,L) ->
  exit(Pid, normal),
  io:fwrite("\nProcess Converged ~w",[self()]),
  checkConvergencename ! dead,
  exit(normal);

actor(S,W,Pid,RoundCount,L)->
  receive
    {Nei_list} ->
      ChildProcessId =spawn(pushsum,childactorinit,[Nei_list]),
      actor(S,W,ChildProcessId,3,Nei_list);
    {Sum,Weight} ->
      exit(Pid,normal),
      NewChildPid = spawn(pushsum,childactorinit,[L]),
      NewS = (S+Sum)/2,
      NewW = (W+Weight)/2,
      Diff = abs((NewS/NewW)-(S/W)),
      Change = math:pow(10,-10),
      if
        Diff =< Change ->
          NewChildPid ! {startpushsum,NewS,NewW},
          actor(NewS,NewW,NewChildPid,RoundCount-1,L);
        true ->
          actor(NewS,NewW,NewChildPid,3,L)
      end;
    {ok,killChild,suicide} ->
      exit(Pid,normal),
      exit(normal)

  end.

childactorinit(Nei_list) ->
  receive
    {startpushsum,NewS,NewW} ->
      childactor(Nei_list,NewS,NewW)
  end,
  childactorinit(Nei_list).

childactor(Nei_list,S,W) ->
  ProcessId = lists:nth(rand:uniform(length(Nei_list)),Nei_list),
  ProcessId ! {S/2,W/2},
  childactor(Nei_list,S,W).
