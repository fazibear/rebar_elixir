-module(rebar_elixir_shell).

-export([
         init/1,
         do/1
        ]).

-define(PROVIDER, shell).
-define(NAMESPACE, elixir).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> rebar_state:t().
init(State) ->
    rebar_state:add_provider(State,
        providers:create([{name, ?PROVIDER},
                          {module, ?MODULE},
                          {namespace, ?NAMESPACE},
                          {bare, true},
                          {deps, ?DEPS},
                          {example, "rebar3 elixir shell"},
                          {short_desc, ""},
                          {desc, ""},
                          {opts, []}])).

do(State) ->
  user_drv:start_shell(#{initial_shell => {'Elixir.IEx', start, []}}),
  receive
    wait -> forever
  end,
  {ok, State}.
