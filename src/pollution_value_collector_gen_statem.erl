%%%-------------------------------------------------------------------
%%% @author weron
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2024 15:10
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("weron").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1,set_station/1,set_station/2,add_value/3, store_data/0, callback_mode/0, waiting/3, adding/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_statem:start_link({local, gen_statem_value_collector}, ?MODULE, [], []).

set_station(Name, Cords) ->
  gen_statem:cast(gen_statem_value_collector, {set_station, Name, Cords}).

set_station(ID) ->
  gen_statem:call(gen_statem_value_collector, {set_station, ID}).

add_value(Time, Type, Value) ->
  gen_statem:cast(gen_statem_value_collector, {add_value,Time,Type,Value}).

store_data() ->
  gen_statem:call(gen_statem_value_collector,{store_data}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  {ok, waiting, []}.

callback_mode() ->
  state_functions.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.

waiting(_Event, {set_station, Name, Cords}, _State) ->
  gen_server:call(pollution_gen_server,{add_station, Name, Cords}),
  {next_state, adding, {Name, []}};

waiting({call, From}, {set_station, ID}, _State) ->
  case pollution_gen_server:is_station(ID) of
    {error, _} -> {next_state,waiting,[],{reply,From,"Station has not been found. Consider adding it first by providing name and cords"}};
    _ -> {next_state,adding,{ID,[]},{reply,From,"Station found"}}
  end.

adding(_Event, {add_value, Time,Type,Value}, {Name, List}) ->
  {next_state,adding,{Name, List ++ [{Time,Type,Value}]}};

adding({call,From}, {store_data}, {Name, List})->
  Reply = [pollution_gen_server:add_value(Name,Time,Type,Value) || {Time, Type, Value} <- List],
  {next_state,waiting,[],{reply,From,Reply}}.


%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
