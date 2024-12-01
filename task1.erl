-module(task1).
-export([run_benchmark/0]).

run_benchmark() ->
    Data = generate_data(10000),
    [
        benchmark(map, Data),
        benchmark(proplist, Data),
        benchmark(dict, Data),
        benchmark(ets, Data),
        benchmark(process_dict, Data)
    ].

generate_data(N) ->
    [{integer_to_binary(I), I} || I <- lists:seq(1, N)].

benchmark(Type, Data) ->
    io:format("Benchmarking ~p...~n", [Type]),
    {SetupTime, Storage} = timer:tc(fun() -> setup_storage(Type) end),
    {InsertTime, UpdatedStorage} = timer:tc(fun() -> insert_data(Type, Storage, Data) end),
    {ReadTime, _} = timer:tc(fun() -> read_data(Type, UpdatedStorage, Data) end),
    {UpdateTime, _} = timer:tc(fun() -> update_data(Type, UpdatedStorage, Data) end),
    {DeleteTime, _} = timer:tc(fun() -> delete_data(Type, UpdatedStorage, Data) end),
    io:format(
        "~p Results: Setup=~p µs, Insert=~p µs, Read=~p µs, Update=~p µs, Delete=~p µs~n",
        [Type, SetupTime, InsertTime, ReadTime, UpdateTime, DeleteTime]
    ),
    ok.

setup_storage(map) -> #{};
setup_storage(proplist) -> [];
setup_storage(dict) -> dict:new();
setup_storage(ets) -> ets:new(test_ets, [set, public]);
setup_storage(process_dict) ->
    put(process_dict, #{}),
    get(process_dict).

insert_data(map, Storage, Data) ->
    lists:foldl(fun({K, V}, Acc) -> maps:put(K, V, Acc) end, Storage, Data);
insert_data(proplist, Storage, Data) ->
    Storage ++ Data;
insert_data(dict, Storage, Data) ->
    lists:foldl(fun({K, V}, Acc) -> dict:store(K, V, Acc) end, Storage, Data);
insert_data(ets, Storage, Data) ->
    lists:foreach(fun({K, V}) -> ets:insert(Storage, {K, V}) end, Data),
    Storage;
insert_data(process_dict, _, Data) ->
    lists:foreach(fun({K, V}) -> put(K, V) end, Data),
    get(process_dict).

read_data(map, Storage, Data) ->
    lists:foreach(fun({K, _}) -> maps:get(K, Storage) end, Data),
    ok;
read_data(proplist, Storage, Data) ->
    lists:map(fun({K, _}) -> lists:keyfind(K, 1, Storage) end, Data),
    ok;
read_data(dict, Storage, Data) ->
    lists:foreach(fun({K, _}) -> dict:fetch(K, Storage) end, Data),
    ok;
read_data(ets, Storage, Data) ->
    lists:foreach(fun({K, _}) -> ets:lookup(Storage, K) end, Data),
    ok;
read_data(process_dict, _, Data) ->
    lists:foreach(fun({K, _}) -> get(K) end, Data),
    ok.

update_data(map, Storage, Data) ->
    lists:foldl(fun({K, V}, Acc) -> maps:put(K, V + 1, Acc) end, Storage, Data);
update_data(proplist, Storage, _) ->
    Updated = [{K, V + 1} || {K, V} <- Storage],
    Updated;
update_data(dict, Storage, Data) ->
    lists:foldl(fun({K, V}, Acc) -> dict:store(K, V + 1, Acc) end, Storage, Data);
update_data(ets, Storage, Data) ->
    lists:foreach(fun({K, V}) -> ets:insert(Storage, {K, V + 1}) end, Data),
    Storage;
update_data(process_dict, _, Data) ->
    lists:foreach(fun({K, V}) -> put(K, V + 1) end, Data),
    get(process_dict).

delete_data(map, Storage, Data) ->
    lists:foldl(fun({K, _}, Acc) -> maps:remove(K, Acc) end, Storage, Data);
delete_data(proplist, Storage, Data) ->
    [{K, V} || {K, V} <- Storage, not lists:keymember(K, 1, Data)];
delete_data(dict, Storage, Data) ->
    lists:foldl(fun({K, _}, Acc) -> dict:erase(K, Acc) end, Storage, Data);
delete_data(ets, Storage, Data) ->
    lists:foreach(fun({K, _}) -> ets:delete(Storage, K) end, Data),
    Storage;
delete_data(process_dict, _, Data) ->
    lists:foreach(fun({K, _}) -> erase(K) end, Data),
    ok.
