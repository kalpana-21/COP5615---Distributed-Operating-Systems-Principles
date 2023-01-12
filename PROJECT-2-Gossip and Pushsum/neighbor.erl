-module('neighbor').
-export([findNeighbours/3,deleteallok/2,randomprocess/4]).

findNeighbours(TopologyName, ProcList,I) ->
  case  TopologyName of
    full  ->
        lists:delete(self(),ProcList);
    line ->
      N = length(ProcList),
      Nei_list = [
        if
          I-1 > 0->
            lists:nth(I-1,ProcList);
          true->
            ok
        end,
        if
          I +1 =< N->
            lists:nth(I+1,ProcList);
          true->
            ok
        end
      ],
      deleteallok(Nei_list,length(Nei_list));
    '2d'->
      N = trunc(math:sqrt(length(ProcList))),
      Nei_list = [
        if
          ((I-1) rem N) == 0 ->%%Left neighbour
            ok;
          true -> lists:nth(I-1,ProcList)

        end,
        if
          (I rem N) == 0->%%Right neighbour
            ok;
          true -> lists:nth(I+1,ProcList)
        end,
        if
          I - N > 0->%%Top neighbour
            lists:nth(I-N,ProcList);
          true -> ok
        end,
        if
          I + N =< N*N ->%%Bottom neighbour
            lists:nth(I+N,ProcList);
          true -> ok
        end
      ],
      deleteallok(Nei_list,length(Nei_list));
    imp3d->
      N = trunc(math:sqrt(length(ProcList))),
      Nei_list = [
        if
          (I - N > 0) and (((I-1) rem N) /= 0 ) == true ->   %%Top left neighbour
            lists:nth(I-N-1,ProcList);
          true -> ok
        end,
        if
          I - N > 0->%%Top neighbour
            lists:nth(I-N,ProcList);
          true -> ok
        end,
        if
          (I - N > 0) and ((I rem N) /= 0 ) == true ->   %%Top Right neighbour
            lists:nth(I-N+1,ProcList);
          true -> ok
        end,
        if
          ((I-1) rem N) == 0 ->%%Left neighbour
            ok;
          true -> lists:nth(I-1,ProcList)
        end,
        if
          (I rem N) == 0-> %%Right neighbour
            ok;
          true -> lists:nth(I+1,ProcList)
        end,
        if
          (I + N =< (N*N)) and (((I-1) rem N) /= 0 ) == true ->   %%Bottom left neighbour
            lists:nth(I+N-1,ProcList);
          true -> ok
        end,
        if
          I + N =< N*N ->%%Bottom neighbour
            lists:nth(I+N,ProcList);
          true -> ok
        end,
        if
          (I + N =< (N*N)) and ((I rem N) /= 0 ) == true ->   %%Bottom left neighbour
            lists:nth(I+N+1,ProcList);
          true -> ok
        end
      ],
      NewNeiList = deleteallok(Nei_list,length(Nei_list)),
      NewProcList = lists:delete(lists:nth(I,ProcList),ProcList),
      if(length(NewNeiList) == length(NewProcList)) -> NewNeiList;
        true ->
          lists:append(NewNeiList,[randomprocess(length(NewProcList),NewNeiList,NewProcList,[])])
      end
      end.

deleteallok(Nei_List,0)->
  Nei_List;

deleteallok(Nei_List,Length) ->
  NewList = lists:delete(ok,Nei_List),
  deleteallok(NewList,Length-1).

randomprocess(0,Nei_List,ProcList,TempNonNeiList) ->
%%  io:fwrite("\nTemp Non Nei List : ~w",[TempNonNeiList]),
  lists:nth(rand:uniform(length(TempNonNeiList)),TempNonNeiList);


randomprocess(Count,Nei_List,ProcList,NonNeiList) ->
  Member = lists:nth(Count,ProcList),
  Is_member = lists:member((Member),Nei_List),
  if Is_member == false  ->
    TempNonNeiList = lists:append(NonNeiList,[Member]);
  true ->
    TempNonNeiList = NonNeiList
  end,
    randomprocess(Count-1,Nei_List,ProcList,TempNonNeiList).
