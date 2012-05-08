-file("record.erl", 1).

set({r,K1,K2,K3}) ->
    {ok,K1,K2,K3}.

set2(R) ->
    {ok,
     case R of
         {r,rec0,_,_} ->
             rec0;
         _ ->
             error({badrecord,r})
     end,
     case R of
         {r,_,rec1,_} ->
             rec1;
         _ ->
             error({badrecord,r})
     end,
     case R of
         {r,_,_,rec2} ->
             rec2;
         _ ->
             error({badrecord,r})
     end}.

module_info() ->
    erlang:get_module_info(record).

module_info(X) ->
    erlang:get_module_info(record, X).

