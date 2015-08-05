-module(uc_root_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {uc_default,
         [
          init/3,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2
         ]}
       ]).

-export(
  [
   allowed_methods/2,
   handle_get/2
  ]).

%% cowboy
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

%% internal
handle_get(Req, State) ->
  Radios = uc_worker:livings(),
  Body = jiffy:encode(#{urls => Radios}),
  {Body, Req, State}.
