-module(rebar_elixir).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  %% Add elixir tu build path
  State1 = rebar_elixir_utils:add_elixir(State),
  State2 = rebar_elixir_utils:add_elixir_to_build_path(State1),

  %% Add rebar resources
  State3 = rebar_state:add_resource(State2, {hex, rebar_elixir_hex}),
  State4 = rebar_state:add_resource(State3, {iex_dep, rebar_elixir_dep}),

  %% Add project builder that works with some versions of rebar3
  State5 =
    case erlang:function_exported(rebar_state, add_project_builder, 3) of
      true -> rebar_state:add_project_builder(State4, mix, rebar_elixir_builder);
      _ -> State4
    end,

  %% Update project release config
  LibDir = rebar_elixir_utils:get_lib_dir(State5),
  RelxConfig = rebar_state:get(State5, relx, []),
  NewRelxConfig =
    case lists:keyfind(lib_dirs, 1, RelxConfig) of
      {lib_dirs, OldLibDir} ->
        NewLibDir = OldLibDir ++ [LibDir],
        lists:keyreplace(lib_dirs, 1, RelxConfig, {lib_dirs, NewLibDir});
      false ->
        [{lib_dirs, [LibDir]}] ++ RelxConfig
    end,

  State6 = rebar_state:set(State5, relx, NewRelxConfig),

  State7 = rebar_elixir_consolidate_protocols:init(State6),
  State8 = rebar_elixir_compile:init(State7),
  State9 = rebar_elixir_shell:init(State8),
  {ok, State9}.
