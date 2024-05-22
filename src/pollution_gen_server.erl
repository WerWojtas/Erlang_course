%%%-------------------------------------------------------------------
%%% @author weron
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0, init/1, crash/0,handle_call/3]).
-export([add_station/2, add_value/4, get_maximum_gradient_stations/1, get_daily_mean/2, get_one_value/3, get_station_mean/2, remove_value/3, is_station/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, pollution:create_monitor()}.

handle_call({add_station,Name,Cords}, _From, State) ->
  case pollution:add_station(Name,Cords,State) of
    {error,Message} -> {reply,Message,State};
    NewState -> {reply,"Station added succesfully", NewState}
  end;

handle_call({add_value,ID,Time,Type,Value}, _From, State) ->
  case pollution:add_value(ID,Time,Type,Value,State) of
    {error, Message} -> {reply, Message,State};
    NewState -> {reply,"Record added succesfully", NewState}
  end;

handle_call({get_one_value,ID,Time,Type},_From, State) ->
  case pollution:get_one_value(ID,Time,Type,State) of
    {error,Message} -> {reply, Message,State};
    Value -> {reply, Value, State}
  end;

handle_call({remove_value,ID,Time,Type},_From, State) ->
  case pollution:remove_value(ID,Time,Type,State) of
    {error,Message} -> {reply, Message,State};
    _Value -> {reply, "Value removed succesfully", State}
  end;

handle_call({get_station_mean,ID,Type},_From, State) ->
  case pollution:get_station_mean(ID,Type,State) of
    {error,Message} -> {reply, Message,State};
    Mean -> {reply, Mean, State}
  end;

handle_call({get_daily_mean,Type,Date},_From, State) ->
  case pollution:get_daily_mean(Type,Date,State) of
    {error,Message} -> {reply, Message,State};
    Mean -> {reply, Mean, State}
  end;

handle_call({get_maximum_gradient_stations,Num},_From, State) ->
  case pollution:get_maximum_gradient_stations(Num,State) of
    List -> {reply,List,State}
  end;

handle_call({is_station,ID}, _From, State) ->
  {reply, pollution:is_station(ID, State), State}.

handle_cast(_Mess,State)->
  {noreply,State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("Server exit, reason: ~p~n",[Reason]).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_station(Name, Cords) ->
  gen_server:call(pollution_gen_server,{add_station, Name, Cords}).

add_value(ID, Time, Type, Value) ->
  gen_server:call(pollution_gen_server,{add_value, ID, Time, Type, Value}).

remove_value(ID, Time, Type) ->
  gen_server:call(pollution_gen_server,{remove_value, ID, Time, Type}).

get_one_value(ID, Time, Type) ->
  gen_server:call(pollution_gen_server,{get_one_value, ID, Time, Type}).

get_station_mean(ID, Type) ->
  gen_server:call(pollution_gen_server,{get_station_mean, ID, Type}).

get_daily_mean(Type, Date) ->
  gen_server:call(pollution_gen_server,{get_daily_mean, Type, Date}).

get_maximum_gradient_stations(Num) ->
  gen_server:call(pollution_gen_server,{get_maximum_gradient_stations, Num}).

is_station(ID) ->
  gen_server:call(pollution_gen_server, {is_station,ID}).

crash() ->
  gen_server:call(pollution_gen_server,crash).
