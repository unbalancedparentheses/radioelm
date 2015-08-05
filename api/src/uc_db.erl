-module(uc_db).

-export([
         execute/1,
         start_link/0
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

execute(Query) ->
    gen_server:call(?MODULE, {execute, Query}, infinity).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% internal
init([]) ->
    process_flag(trap_exit, true),

    #{db := DbConfig} = uc_config:read(),
    #{first_reconnect_interval := FirstReconectInterval} = DbConfig,

    State = #{db => DbConfig,
              conn => undefined,
              connects => 0,
              auto_reconnect => true,
              reconnect_interval => FirstReconectInterval},
    case connect(State) of
        {error, _Reason} ->
            {ok, State};
        Ok ->
            Ok
    end.

terminate(_Reason, _State) ->
    ok.

handle_call(_Msg, _From, #{conn := C} = State) when C =:= undefined ->
            {reply, {error, disconnected}, State};
handle_call({execute, Query}, _From, #{conn := C} = State) ->
    Answer = epgsql:squery(C, Query),
    {reply, Answer, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reconnect, #{connects := Connects} = State) ->
    case connect(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _Reason} ->
            NewState = State#{connects := Connects + 1},
            {noreply, NewState}
    end;
handle_info({'EXIT', _Pid, _Reason}, State)->
    disconnect(State);
handle_info(Msg, State) ->
    lager:info("Received unsupported message: ~p", [Msg]),
    {noreply, State}.

code_change(_Ol, _State, _Extra) ->
    {ok, _State}.

connect(#{db := Db,
          conn := undefined,
          connects := Connects} = State) ->
    #{host := Host,
      name := Name,
      user := User,
      password := Password,
      first_reconnect_interval := FirstReconectInterval
     } = Db,
    case epgsql:connect(Host, User, Password, [{database, Name}, {timeout, 3000}]) of
        {ok, C} ->
            {ok, State#{conn := C,
                        reconnect_interval := FirstReconectInterval,
                        connects := Connects + 1}};
        Error ->
            Error
    end.

disconnect(#{conn := C,
             auto_reconnect := AutoReconnect,
             reconnect_interval := ReconnectInterval
            } = State) ->
    case C of
        undefined ->
            ok;
        C ->
            ok = epgsql:close(C)
    end,

    NewState = State#{conn := undefined},

    case AutoReconnect of
        true ->
            lager:info("Db disconnected. Reconnecting in ~p milliseconds", [ReconnectInterval]),
            erlang:send_after(ReconnectInterval, self(), reconnect),
            {noreply, increase_interval(NewState)};
        false ->
            {stop, disconnected, NewState}
    end.

increase_interval(#{db := #{max_reconnect_interval := MaxReconnectInterval},
                    reconnect_interval := ReconnectInterval} = State) ->
    case ReconnectInterval of
        Interval when Interval < MaxReconnectInterval ->
            NewInterval = min(Interval+Interval, MaxReconnectInterval),
            State#{reconnect_interval := NewInterval};
        _ ->
            State
    end.
