-module(rebar_elixir_compile).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, compile).
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
                          {example, "rebar3 elixir compile"},
                          {short_desc, ""},
                          {desc, ""},
                          {opts, []}])).

do(State) ->
  [AppInfo] = case rebar_state:current_app(State) of
              undefined ->
                rebar_state:project_apps(State);
              App ->
                [App]
            end,

  Plugins = rebar_state:all_plugin_deps(State),
  {ok, P} = rebar_app_utils:find(<<"rebar_elixir">>, Plugins),
  ScriptDir = filename:join(rebar_app_info:dir(P), "lib"),

  LibDir = rebar_app_info:dir(AppInfo) ++ "/lib",
  OutDir = rebar_app_info:ebin_dir(AppInfo),

  case rebar_utils:sh("elixir compile.exs",
                      [{cd, ScriptDir},
                       {return_on_error, true},
                       {use_stdout, true},
                       {env, [
                              {"ELIXIR_LIB_DIR", LibDir},
                              {"ELIXIR_OUT_DIR", OutDir},
                              {"ERL_FLAGS", ""}
                             ]
                       }]) of
    {error, {127, _}} ->
      {error, {?MODULE, elixir_not_found}};
    {error, {_Code, _Error}} ->
      {error, {?MODULE, elixir_script_failed}};
    _ ->
      {ok, State}
  end.

% Files = rebar_utils:find_files(LibDir, ".*\\.ex\$"),
% 'Elixir.Kernel.ParallelCompiler':compile(["/home/m.kalbarczy2/dev/local_mix_x/lib/example.ex"])

% run elixirc

% Cmd = "elixirc --verbose " ++ LibDir ++ " -o " ++ OutDir,
% Opts = [ {return_on_error, true}, {use_stdout, true} ],
%
% case rebar_utils:sh(Cmd, Opts) of
%   {error, {127, _}} ->
%     {error, {?MODULE, elixir_not_found}};
%   {error, {_Code, _Error}} ->
%     {error, {?MODULE, elixirc_failed}};
%   _ ->
%     {ok, State}
% end.

format_error({elixirc_found, Name}) ->
  io_lib:format("Elixir and mix must be installed to build application ~ts. "
                "Install Elixir or check your path and try again.", [Name]);
format_error(Error) ->
  io_lib:format("~p", [Error]).
