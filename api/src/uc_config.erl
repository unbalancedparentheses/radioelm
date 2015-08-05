-module(uc_config).

-export([
        read/0,
         check/0
        ]).

read() ->
    {ok, HttpPort} = application:get_env(uc, http_port),
    {ok, HttpListenerCount} = application:get_env(uc, http_listener_count),

    {ok, CheckInterval} = application:get_env(uc, check_interval),
    {ok, LiveCheckInterval} = application:get_env(uc, live_check_interval),

    {ok, DbConfig} = application:get_env(uc, db),
    Host = proplists:get_value(host, DbConfig),
    Name = proplists:get_value(name, DbConfig),
    Table = proplists:get_value(table, DbConfig),
    User = proplists:get_value(user, DbConfig),
    Password = proplists:get_value(password, DbConfig),
    FirstReconectInterval = proplists:get_value(first_reconnect_interval, DbConfig),
    MaxReconnectInterval = proplists:get_value(max_reconnect_interval, DbConfig),

    #{http_port =>  HttpPort,
      http_listener_count => HttpListenerCount,
      check_interval => CheckInterval,
      live_check_interval => LiveCheckInterval,
      db =>
          #{host => Host,
            name => Name,
            table => Table,
            user => User,
            password => Password,
            first_reconnect_interval => FirstReconectInterval,
            max_reconnect_interval => MaxReconnectInterval
           }
     }.

check() ->
    Config = application:get_all_env(uc),
    Check = [http_port,
             http_listener_count,
             check_interval,
             db
            ],
    ok = check(Config, Check),
    ok = check_db(),
    ok.

%% internal
check_db() ->
    Config = application:get_all_env(uc),
    DbConfig = proplists:get_value(db, Config),
    Check = [host,
             name,
             table,
             user,
             password,
             first_reconnect_interval,
             max_reconnect_interval],

    ok = check(DbConfig, Check),
    ok.

check(PropList, Elements) ->
    lists:foreach(fun (Element) ->
                          case proplists:is_defined(Element, PropList) of
                              true ->
                                  ok;
                              false ->
                                  throw({error, "Key "
                                         ++ erlang:atom_to_list(Element) ++
                                         " must be present in the configuration file"})
                          end
                  end,
                  Elements).
