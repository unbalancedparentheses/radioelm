-module(uc_pinger).

-export([
         is_live/1,
         start_link/2,
         stop/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

is_live(Pid) ->
  try
    gen_server:call(Pid, live)
  catch
    _:_ ->
      false
  end.

stop(Pid) ->
  gen_server:call(Pid, stop).

start_link(Name, Url) ->
  gen_server:start_link(?MODULE, {Name, binary:bin_to_list(Url)}, []).

init({Name, Url}) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), check),
  {ok, #{name => Name, url => Url, live => false}}.

terminate(_Reason, #{id := Id} = _State) ->
  ibrowse:stream_close(Id),
  ok;
terminate(_Reason, _State) ->
  ok.

handle_call(live, _From, #{live := Live} = State) ->
  {reply, Live, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(check, #{url := Url} = State) ->
  case ibrowse:send_req(Url, [], get, [], [{stream_to, self()}]) of
    {ibrowse_req_id, Id} ->
      {noreply, State#{id => Id}};
    _ ->
      {noreply, State}
  end.

handle_info(Info, #{id := Id, name := Name} = State) ->
  case Info of
    {ibrowse_async_headers, Id, "200", _Headers} ->
      {noreply, State#{live := true}};
    {ibrowse_async_response, Id, {error, {content_length_undefined, {stat_code,"200"}, _}}} ->
      {noreply, State#{live := true}};
    {ibrowse_async_response, Id, {error,req_timedout}} ->
      {noreply, State};
    {ibrowse_async_response_timeout, Id} ->
      {noreply, State};
    Message ->
      lager:debug("Received unsupported ibrowse message for Url ~p"
                  "with the following Message: ~p~n~n", [Name, Message]),
      {noreply, State}
  end;
handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_Ol, _State, _Extra) ->
  {ok, _State}.
