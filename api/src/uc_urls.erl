-module(uc_urls).

-export([
         get_all/0
        ]).

get_all() ->
    Query = ["SELECT * FROM ", table_name(), ";"],

    case uc_db:execute(Query) of
        {ok, _, Rows} ->
            Urls = lists:map(fun({_Id, Name, Url}) ->
                              #{name => Name,
                                url => Url}
                      end, Rows),
            {ok, Urls};
        Error ->
            Error
    end.

%% internal
table_name() ->
    #{db := #{table := Table}} = uc_config:read(),
    Table.
