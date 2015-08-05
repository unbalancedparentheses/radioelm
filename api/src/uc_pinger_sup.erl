-module(uc_pinger_sup).
-behaviour(supervisor).

-export([
         start_link/0,
         start/2,
         terminate/1
        ]).

-export([
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    {ok,
     {
       #{strategy => simple_one_for_one,
         intensity => 20,
         period => 60
        },
       [
        #{id => uc_pinger,
          start => {uc_pinger,
                    start_link,
                    []
                   },
          restart => transient,
          type => worker
         }
       ]
     }
    }.

start(Name, Url)->
    supervisor:start_child(?MODULE, [Name, Url]).

terminate(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).
