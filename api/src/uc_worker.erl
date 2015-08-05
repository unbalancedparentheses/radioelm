-module(uc_worker).

-export([
         livings/0,
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

livings() ->
  Urls = ets:tab2list(urls),
  lists:map(fun ({Name, Url}) ->
                #{name => Name,
                  url => Url
                 }
            end,
            Urls).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(urls, [set, public, named_table, {read_concurrency, true}]),
  uc_pinger_sup:start_link(),
  #{check_interval := CheckInterval,
    live_check_interval := LiveCheckInterval} = uc_config:read(),

  erlang:send_after(0, self(), check),

  {ok, #{checkers => [],
         check_interval => CheckInterval,
         live_check_interval => LiveCheckInterval
        }}.

terminate(_Reason, _State) ->
  ok.

handle_call(_Msg, _From, State) ->
  {reply, noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(check, #{checkers := Checkers,
                     live_check_interval := LiveCheckInterval} = State) ->
  lager:info("Pinging all the urls"),
  Checkers1 = recheck(Checkers),
  erlang:send_after(LiveCheckInterval, self(), dump),
  {noreply, State#{checkers := Checkers1}};

handle_info(dump, #{checkers := Checkers,
                    check_interval := CheckInterval} = State) ->
  lager:info("Dumping the urls that answered the ping"),
  Livings = livings(Checkers),
  dump(Livings),

  erlang:send_after(CheckInterval, self(), check),
  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_Ol, _State, _Extra) ->
  {ok, _State}.

%% internal
stop_checkers(Checkers) ->
  lists:foreach(fun (#{pid := Pid}) ->
                    uc_pinger_sup:terminate(Pid)
                end, Checkers).

recheck(Checkers) ->
  stop_checkers(Checkers),
  case uc_urls:get_all() of
    {ok, Radios} ->
      lists:map(fun (#{name := Name, url := Url}) ->
                    {ok, Pid} = uc_pinger_sup:start(Name, Url),
                    #{
                       name => Name,
                       url => Url,
                       pid => Pid
                     }
                end, Radios);
    {error, _Error} ->
      []
  end.

livings(Checkers)->
  lists:filtermap(fun (#{pid := Pid, name := Name, url := Url}) ->
                      case uc_pinger:is_live(Pid) of
                        true ->
                          {true, {Name, Url}};
                        false ->
                          false
                      end
                  end, Checkers).

dump(Livings) ->
  ets:delete_all_objects(urls),
  ets:insert(urls, Livings).
