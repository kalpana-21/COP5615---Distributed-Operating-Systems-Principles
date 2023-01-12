-module(server).
-export([start_server/1,spawn_server_worker/2,server/2,randomizer/2,worker/2]).

start_server(N) ->
 Count = 1,
  Server_Pid = spawn(server, server, [N,Count]),
  register(serverg,Server_Pid).

spawn_server_worker(N,0)->
  ok;
spawn_server_worker(N,Count)->
  spawn(server,worker,[N,serverg]),
  spawn_server_worker(N,Count-1).

server(N,Count) ->
  if
    Count == 1 ->
      spawn_server_worker(N,15);
    true ->
      ok
  end,
  receive
    {Hstr,WServer_PID} ->
%%      io:fwrite("\n server worker ~s   (~w)",[Hstr,WServer_PID]);
       io:fwrite("\n~s",[Hstr]);
    {Hstr,WClient_PID,clientmine} ->
%%      io:fwrite("\n client worker ~s   (~w)",[Hstr,WClient_PID]);
       io:fwrite("\n~s",[Hstr]);
    {clientg,Client_Node,giveN} ->
      {clientg,Client_Node} ! {N,takeN}
  end,
  server(N,0).

randomizer(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
              end, [], lists:seq(1, Length)).

worker(N,Server_Pid)  ->
  Gat_id = "kponnada;",
  Ran_str = randomizer(rand:uniform(25),"abcdefghijklmnopqrstuvwxyz1234567890"),
  Str = Gat_id ++ Ran_str,
  Hstr = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Str))]),
  A = string:slice(Hstr,0,N),
  B = string:copies("0",N),
  C=Str++" "++Hstr,
  if
    A==B ->
%%      io:format("\n only during server",[]),
      {Server_Pid,node()} ! {C,self()};
    true ->
      ok
  end,
  worker(N, Server_Pid).




