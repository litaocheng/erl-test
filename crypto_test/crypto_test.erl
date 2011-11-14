-module(crypto_test).
-compile([export_all]).

run() ->
    crypto:start(),
    do_run(),
    crypto:stop().

do_run() ->
    run_rc4().

%% 测试rc4
run_rc4() ->
    Key = <<"$xxw2_&a2!d23">>,
    Text = 
    <<"hello my name is litao, I want to be your friend! I come from china, love programming!
   love my family!">>, 
   N = 10000,
   Result =
   [begin
        {T1, Bin} = timer:tc(crypto, rc4_encrypt, [Key, Text]),
        {T2, Text} = timer:tc(crypto, rc4_encrypt, [Key, Bin]),
        T1 + T2
    end || _ <- lists:duplicate(N, dummy)],
    io:format("文本长度(~w) key:~s 加解密~w次 (时间微妙):~n"
        "total:~w min:~w max:~w avg:~w~n",
    [iolist_size(Text), Key, N,
        lists:sum(Result), lists:min(Result), lists:max(Result), 
        lists:sum(Result) / length(Result)]).
