%%%-------------------------------------------------------------------
%%% @author weron
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2024 12:44
%%%-------------------------------------------------------------------
-module(pollution).
-author("weron").

%% API
-export([create_monitor/0,add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3,get_maximum_gradient_stations/2, is_station/2]).

create_monitor() -> {#{},#{}}.

add_station(Name, Cords, {CordsMap,NameMap}) ->
  case maps:find(Cords, CordsMap) of
    {ok,_} -> {error, "Station with that cords already exists"};
    error -> check_map(Name,Cords,CordsMap,NameMap)
  end.


check_map(Name,Cords,CordsMap,NameMap) ->
  case maps:find(Name,NameMap) of
    {ok,_} -> {error, "Station with that name already exists"};
    error -> {CordsMap#{Cords => Name}, NameMap#{Name => #{}}}
  end.

add_value(ID, Time, Type, Value, {CordsMap,NameMap})->
  {_,Name} = get_name(ID,CordsMap,NameMap),
  case maps:find(Name, NameMap) of
    error -> {error, "Map with given Name/Cords doesn't exist"};
    {ok,TimeMap} ->  case check_type(Type,Time,Value,TimeMap) of
                       {error,Message} -> {error,Message};
                       UpdatedMap -> {CordsMap,NameMap#{Name := UpdatedMap}}
                     end
  end.

get_name(ID,CordsMap,_) when is_tuple(ID) ->
  case maps:find(ID,CordsMap) of
    {ok, Value} -> {ok, Value};
    _ -> {ok, error}
  end;
get_name(ID,_,_) -> {ok,ID}.

is_station(ID,{CordsMap,NameMap}) ->
  Name = get_name(ID, CordsMap, NameMap),
  case maps:find(Name, NameMap) of
    error -> {error, "Map with given name/cords doesn't exists"};
    _ -> {CordsMap, NameMap}
  end.

check_type(Type,Time,Value,TimeMap) ->
  case maps:find(Time,TimeMap) of
    error -> TimeMap#{Time => [{Type,Value}]};
    {ok,List} -> case [{Key,Val} || {Key,Val} <- List, Key == Type] of
                   [_|_] -> {error,"Type at that time already exists in given station"};
                   _ -> TimeMap#{Time := List++[{Type,Value}]}
                 end
  end.

remove_value(ID,Time,Type,{CordsMap,NameMap}) ->
  {_,Name} = get_name(ID,CordsMap,NameMap),
  case maps:find(Name, NameMap) of
    error -> {error, "Map with given Name/Cords doesn't exist"};
    {ok,TimeMap} -> case maps:find(Time,TimeMap) of
                      error -> {error, "There is no reading with given time."};
                      {ok,List} -> case [{Key,Val} || {Key,Val} <- List, Key == Type] of
                                     [] -> {error, "There is no reading with given type."};
                                     _ -> {CordsMap,NameMap#{Name => TimeMap#{Time => [{Key,Value} || {Key,Value} <- List, Key /=Type]}}}
                                   end
                    end
  end.

get_one_value(ID,Time,Type,{CordsMap,NameMap}) ->
  {_,Name} = get_name(ID,CordsMap,NameMap),
  case maps:find(Name, NameMap) of
    error -> {error, "Map with given Name/Cords doesn't exist"};
    {ok,TimeMap} -> case maps:find(Time,TimeMap) of
                      error -> {error, "There is no reading with given time."};
                      {ok,List} -> case [{Key,Val} || {Key,Val} <- List, Key == Type] of
                                     [] -> {error, "There is no reading with given type."};
                                     [{Type,Value}] -> Value
                                   end
                    end
  end.

get_station_mean(ID,Type,{CordsMap,NameMap}) ->
  {_,Name} = get_name(ID,CordsMap,NameMap),
  case maps:find(Name,NameMap) of
    error -> {error, "Map with given Name/Cords doesn't exist"};
    {ok,TimeMap} -> case [Val || {_,List} <- maps:to_list(TimeMap), {OtherType,Val} <- List, OtherType==Type] of
                      [] -> {error,"Station doesn't have record with given type"};
                      List -> lists:foldl(fun (X,Acc) -> X+Acc end,0,List)/length(List)
                    end
  end.

get_daily_mean(Type,Date,{_,NameMap}) ->
  ValList = [Value || {_,TimeMap} <- maps:to_list(NameMap),{{OtherDate,_},List} <- maps:to_list(TimeMap), {OtherType,Value} <- List, OtherDate == Date, OtherType == Type],
  case ValList of
    [] -> {error, "There are no values of given day and type"};
    List -> lists:foldl(fun (X,Acc) -> X+Acc end,0,List)/length(List)
  end.

get_maximum_gradient_stations(Num,{CordsMap,NameMap}) ->
  Pollution = maps:from_list([{Station, lists:sum([Val|| {PM,Val} <- TypeList, lists:nth(1,PM) == 80, lists:nth(2,PM) == 77])} ||
    {Station, TimeMap} <- maps:to_list(NameMap),
    {_, TypeList} <- maps:to_list(TimeMap)]),
  Distances = [{Name1,Name2,euclidean_distance(X1,Y1,X2,Y2)} || {{X1,Y1},Name1} <- maps:to_list(CordsMap),{{X2,Y2}, Name2} <-maps:to_list(CordsMap), X1=<X2,Y1=<Y2, Name1 /= Name2],
  Gradient = [{(abs(maps:get(Name1,Pollution)-maps:get(Name2,Pollution)))/Distance,Name1,Name2} || {Name1,Name2,Distance} <- Distances],
  lists:sublist(Gradient,Num).

euclidean_distance(X1, Y1, X2, Y2) ->
  math:sqrt(math:pow((X2 - X1),2) + math:pow((Y2 - Y1),2)).
