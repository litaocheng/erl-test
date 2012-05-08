{module, record}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{set,1},{set2,1}]}.

{attributes, []}.

{labels, 10}.


{function, set, 1, 2}.
  {label,1}.
    {line,[{location,"record.erl",10}]}.
    {func_info,{atom,record},{atom,set},1}.
  {label,2}.
    {test,is_tuple,{f,1},[{x,0}]}.
    {test,test_arity,{f,1},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {get_tuple_element,{x,0},3,{x,4}}.
    {test,is_eq_exact,{f,1},[{x,1},{atom,r}]}.
    {test_heap,5,5}.
    {put_tuple,4,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    {put,{x,3}}.
    {put,{x,4}}.
    return.


{function, set2, 1, 4}.
  {label,3}.
    {line,[{location,"record.erl",13}]}.
    {func_info,{atom,record},{atom,set2},1}.
  {label,4}.
    {test,is_tuple,{f,5},[{x,0}]}.
    {test,test_arity,{f,5},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {get_tuple_element,{x,0},3,{x,4}}.
    {test,is_eq_exact,{f,5},[{x,1},{atom,r}]}.
    {test_heap,5,5}.
    {put_tuple,4,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    {put,{x,3}}.
    {put,{x,4}}.
    return.
  {label,5}.
    {move,{literal,{badrecord,r}},{x,0}}.
    {line,[{location,"record.erl",14}]}.
    {call_ext_only,1,{extfunc,erlang,error,1}}.


{function, module_info, 0, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,record},{atom,module_info},0}.
  {label,7}.
    {move,{atom,record},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 9}.
  {label,8}.
    {line,[]}.
    {func_info,{atom,record},{atom,module_info},1}.
  {label,9}.
    {move,{x,0},{x,1}}.
    {move,{atom,record},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
