-module(my_cache).
-export([create/1, insert/3, insert/4, lookup/2, delete_obsolete/1, universal_time_to_seconds/1]).

create(TableName) ->
    ets:new(TableName, [named_table, public, set]),
    ok.

insert(TableName, Key, Value) ->
    Timestamp = undefined,
    ets:insert(TableName, {Key, Value, Timestamp}),
    ok.

insert(TableName, Key, Value, Lifetime) ->
    Timestamp = calendar:universal_time(),
    ExpiryTime = universal_time_to_seconds(Timestamp) + Lifetime,
    ets:insert(TableName, {Key, Value, ExpiryTime}),
    ok.

    lookup(TableName, Key) ->
        case ets:info(TableName) of
            undefined -> undefined;
            _ ->
                case ets:lookup(TableName, Key) of
                    [] -> undefined;
                    [{Key, Value, Timestamp}] ->
                        case Timestamp of
                            undefined -> Value;
                            ExpiryTime ->
                                CurrentTime = universal_time_to_seconds(calendar:universal_time()),
                                if
                                    CurrentTime =< ExpiryTime -> Value;
                                    true -> undefined
                                end
                        end
                end
        end.

delete_obsolete(TableName) ->
    CurrentTime = universal_time_to_seconds(calendar:universal_time()),
    ObsoleteKeys = [Key || {Key, _Value, ExpiryTime} <- ets:tab2list(TableName), 
                               ExpiryTime =/= undefined, ExpiryTime < CurrentTime],
    [ets:delete(TableName, Key) || Key <- ObsoleteKeys],
    ok.

universal_time_to_seconds({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}).
