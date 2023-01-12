-module(client).
-export([spawn_client_worker/3,randomizer/2,start_client/1,client/1,client_worker/3]).

spawn_client_worker(N,0,Server_Node)->
  ok;
spawn_client_worker(N,Count,Server_Node)->
  spawn(client,client_worker,[N,serverg,Server_Node]),
  spawn_client_worker(N,Count-1,Server_Node ).

randomizer(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
              end, [], lists:seq(1, Length)).

start_client(IP)->
  Ser_node = "server@"++IP,
  Server_Node = list_to_atom(Ser_node),
  register(clientg,spawn(client,client,[Server_Node])).

client(Server_Node)->
  {serverg,Server_Node} ! {clientg,node(),giveN},
  receive
    {N,takeN} ->
    spawn_client_worker(N,15,Server_Node)
  end.

client_worker(N,Server_Pid,Server_Node)  ->
  Gat_id = "kponnada;",
  Ran_str = randomizer(rand:uniform(25),"abcdefghijklmnopqrstuvwxyz1234567890"),
  Str = Gat_id ++ Ran_str,
  Hstr = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Str))]),
  A = string:slice(Hstr,0,N),
  B = string:copies("0",N),
  C=Str++" "++Hstr,
  if
    A==B ->
      {Server_Pid,Server_Node} ! {C,self(),clientmine };
    true ->
      ok
  end,
  client_worker(N, Server_Pid,Server_Node).



