-module(uc).
-export([
         start/0,
         start/2,
         stop/0,
         stop/1
        ]).

start() ->
    {ok, _Started} = application:ensure_all_started(uc).

stop() ->
    application:stop(uc).

start(_StartType, _StartArgs) ->
    ok = uc_config:check(),
    {ok, Pid} = uc_sup:start_link(),
    start_listeners(),
    {ok, Pid}.

%% internal
stop(_State) ->
    cowboy:stop_listener(uc_http),
    ok.

start_listeners() ->
    #{http_listener_count := HttpListenerCount,
      http_port := HttpPort
     } = uc_config:read(),

    Dispatch =
        cowboy_router:compile(
          [{'_',
            [
             {<<"/">>, uc_root_handler, []}
            ]
           }
          ]),

    RanchOptions =
        [{port, HttpPort}],
    CowboyOptions =
        [{env,
          [
           {dispatch, Dispatch}
          ]},
         {compress, true},
         {timeout, 12000}],
    cowboy:start_http(uc_http, HttpListenerCount, RanchOptions, CowboyOptions).
