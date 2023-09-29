-module(rebar_elixir_consolidate_protocols).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, consolidate_protocols).
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
                        {example, "rebar3 elixir consolidate_protocols"},
                        {short_desc, ""},
                        {desc, ""},
                        {opts, []}])).

do(State) ->
    Apps = project_deps(State),
    [AppInfo] = case rebar_state:current_app(State) of
                undefined ->
                  rebar_state:project_apps(State);
                App ->
                  [App]
              end,

    OutDir = rebar_app_info:ebin_dir(AppInfo),

    Plugins = rebar_state:all_plugin_deps(State),
    {ok, P} = rebar_app_utils:find(<<"rebar_elixir">>, Plugins),
    ScriptDir = filename:join(rebar_app_info:dir(P), "lib"),

    DepsDir = rebar_dir:deps_dir(State),
    EbinDirs = ebin_dirs(Apps, DepsDir),
    EbinDirsString = lists:flatten(lists:join(":", EbinDirs)),

    %% in order to not require figuring out where Elixir lives we
    %% shell out to elixir to run an elixir script. so the only
    %% requirement is that elixir is in the path.
    case rebar_utils:sh("elixir consolidate_protocols.exs",
                        [{cd, ScriptDir},
                         {return_on_error, true},
                         {use_stdout, true},
                         {env, [{"ELIXIR_DEPS_EBIN", EbinDirsString},
                                {"ELIXIR_OUT_DIR", OutDir},
                                {"ERL_FLAGS", ""}]}]) of
        {error, {127, _}} ->
            {error, {?MODULE, elixir_not_found}};
        {error, {_Code, _Error}} ->
            {error, {?MODULE, elixir_script_failed}};
        _ ->
            {ok, State}
    end.

format_error({mix_not_found, Name}) ->
    io_lib:format("Elixir and mix must be installed to build application ~ts. "
                  "Install Elixir or check your path and try again.", [Name]);
format_error(Error) ->
    io_lib:format("~p", [Error]).

project_deps(State) ->
    Profiles = rebar_state:current_profiles(State),
    DepList = lists:foldl(fun(Profile, Acc) ->
                                  rebar_state:get(State, {deps, Profile}, []) ++ Acc
                          end, [], Profiles),
    LockList = lists:foldl(fun(Profile, Acc) ->
                                   rebar_state:get(State, {locks, Profile}, []) ++ Acc
                           end, [], Profiles),
    Deps = [normalize(name(Dep)) || Dep <- DepList++LockList],
    lists:usort(Deps).

name(App) when is_tuple(App) -> element(1, App);
name(Name) when is_binary(Name); is_list(Name); is_atom(Name) -> Name.

normalize(AppName) when is_list(AppName) -> AppName;
normalize(AppName) when is_atom(AppName) -> atom_to_list(AppName);
normalize(AppName) when is_binary(AppName) -> binary_to_list(AppName).

ebin_dirs(Apps, DepsDir) ->
    lists:map(fun(App) ->
                      io_lib:format("~ts/~ts/ebin", [DepsDir, App])
              end, Apps).
