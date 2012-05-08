%% 测试ets性能
%% 通过select, match和tab2list获得数据
-module(ets_test).
-compile([export_all]).

-record(person, {
        id,         % {id, kingdom}
        name,
        sex,
        att,
        def,
        other
       }).
-define(TABLE, ?MODULE).

run(Total) ->
    init_table(Total),
    util:do_with_statis("tab2list",
    fun(_Acc) -> 
        L = ets:tab2list(?TABLE), 
        Ids = [Id || #person{id = {Id, 3}} <- L],
        Names = [Name || #person{name = Name, sex = Sex} <- L, Sex =:= 1],
        {length(Ids), length(Names)}
    end, 0, 1),
    util:do_with_statis("match",
    fun(_Acc) -> 
        Ids = ets:match(?TABLE, #person{id = {'$1', 3}, _ = '_'}),
        Names = ets:match(?TABLE, #person{name = '$1', sex = 1, _ = '_'}),
        {length(Ids), length(Names)}
    end, 0, 1),
    util:do_with_statis("match-object",
    fun(_Acc) -> 
        Ids = ets:match_object(?TABLE, #person{id = {'_', 3}, _ = '_'}),
        Names = ets:match_object(?TABLE, #person{name = '_', sex = 1, _ = '_'}),
        {length(Ids), length(Names)}
    end, 0, 1),
    util:do_with_statis("select",
    fun(_Acc) -> 
        Ids = ets:select(?TABLE, [{#person{id = {'$1', 3}, name = '_', sex = '_', att = '_', def = '_', other = '_'},
                        [], ['$1']}]),
        Names = ets:select(?TABLE, [{#person{name = '$1', sex = 1, id = '_', att = '_', def = '_', other = '_'}, 
                        [], ['$1']}]),
        {length(Ids), length(Names)}
    end, 0, 1),
    ok.

%% 初始化table
init_table(Total) ->
    ets:new(?TABLE, [set, named_table, public, {keypos, #person.id}]),
    [begin
        Person = #person{
                    id = {Id, random:uniform(3)},
                    name = <<"name_", Id:32>>,
                    sex = random:uniform(2),
                    att = random:uniform(10000),
                    def = random:uniform(10000),
                    other = <<Id:32>>
                    },
        ets:insert(?TABLE, Person)
    end || Id <- lists:seq(1, Total)],
    ok.
