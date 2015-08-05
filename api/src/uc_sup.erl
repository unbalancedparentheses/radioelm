-module(uc_sup).

-behaviour(supervisor).

-export([
         start_link/0
        ]).

-export([
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    {ok,
     {
       #{strategy => one_for_one,
         intensity => 60,
         period => 60
        },
       [
        #{id => uc_worker,
          start => {uc_worker,
                    start_link,
                    []
                   },
          type => worker
         },
        #{id => uc_db,
          start => {uc_db,
                    start_link,
                    []
                   },
          type => worker
         }
       ]
     }
    }.
