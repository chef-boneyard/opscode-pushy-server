%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Mark Anderson <mark@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.2
% @end
-module(chef_keyring).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_key/1,
         list_keys/0,
         reload/0,
         reload_if_changed/0,
         stats/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(RELOAD_INTERVAL_MS, 60*1000).

-include_lib("eunit/include/eunit.hrl").

-record(state,
        { keys=dict:new(),
          watch_dir="",
          dir_mod_time=erlang:universaltime(),
          last_updated=os:timestamp()
        }).
%%%
%%% Keys use atoms for names
%%%
%%% * Environment variable pushy:keyring_dir specifies a directory containing .pem
%%%   files; the name is the basename of the the file as an atom
%%%
%%% * Environment variable pushy:keyring is a list of {Name, Path} pairs, where Name
%%%   is an atom, and Path is the path to a pem file
%%%
%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    error_logger:info_msg("Chef Keyring starting~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_key(KeyName) ->
    gen_server:call(?SERVER, {get_key, KeyName}, infinity).

list_keys() ->
    gen_server:call(?SERVER, list_keys, infinity).

stats() ->
    gen_server:call(?SERVER, stats, infinity).

reload() ->
    gen_server:cast(?SERVER, reload).

reload_if_changed() ->
    gen_server:cast(?SERVER, reload_if_changed).


init([]) ->
    try
        State = load(#state{}),
        %% TODO : Decide how and where we want to handle reloading of keys...
        Interval = case application:get_env(pushy, keyring_reload_interval) of
                       undefined -> ?RELOAD_INTERVAL_MS;
                       {ok, I} -> I
                   end,
        timer:apply_interval(Interval, ?MODULE, reload_if_changed, []),
        {ok, State}
    catch
        throw:Error -> {stop, Error}
    end.

%
%
handle_call({get_key, KeyName}, _From, #state{keys=Keys}=State) ->
    Reply = case dict:find(KeyName, Keys) of
                {ok, RawKey} ->
                    {ok, RawKey};
                error ->
                    {error, unknown_key}
            end,
    {reply, Reply, State};
handle_call(list_keys, _From, #state{keys=Keys}=State) ->
    Reply = case dict:size(Keys) of
                0 -> [];
                _ -> lists:sort(dict:fetch_keys(Keys))
            end,
    {reply, Reply, State};
handle_call(stats, _From, State) ->
    Reply =
        [{key_count, dict:size(State#state.keys)},
         {watch_dir, State#state.watch_dir},
         {dir_mod_time, State#state.dir_mod_time},
         {last_updated, State#state.last_updated}],
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%
%
handle_cast(reload_if_changed, #state{watch_dir=undef} = State) ->
    {noreply, State};
handle_cast(reload_if_changed, #state{dir_mod_time=DirModTime, watch_dir=WatchDir} = State) ->
    case modtime(WatchDir) of
        DirModTime -> {noreply, State};
        error -> {noreply, State}; % If we error getting the directory timestamp, we shouldn't reload everything
        _Else -> {noreply,  load(State)}
    end;
handle_cast(reload, State) ->
    NewState = load(State),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

load(State) ->
    {Dir, DirModTime} = check_keyring_dir(),
    KeyRing0 = load_keyring_from_env(State#state.keys),
    KeyRing1 = load_keyring_from_dir(Dir, KeyRing0),
    State#state{keys=KeyRing1,
                watch_dir = Dir,
                dir_mod_time = DirModTime,
                last_updated = os:timestamp()}.

load_keyring_from_env(OldKeys) ->
    case application:get_env(pushy, keyring) of
        undefined -> OldKeys;
        {ok, KeyRing} when is_list(KeyRing) ->
            {ok, Keys} = load_keyring_files(KeyRing, OldKeys),
            Keys;
        {ok, KeyRing} ->
            error_logger:error_msg("Keyring not parsed properly ~s ~n", [KeyRing]),
            throw({bad_keyring, KeyRing})
    end.

check_keyring_dir() ->
    case application:get_env(pushy, keyring_dir) of
        undefined -> {undef, erlang:universaltime()};
        {ok, Path} -> {Path, modtime(Path)}
    end.

%%%
%%% Load all of the .pem files in the specified directory into the keys dictionary
%%%
load_keyring_from_dir(undef, Keys) ->
    error_logger:info_msg("ERLAAAANNNGG"),
    Keys;
load_keyring_from_dir(Dir, Keys) ->
    case filelib:wildcard(filename:join([Dir,"*.pem"])) of
        [] ->
            error_logger:info_msg("Error reading keyring directory ~s: ~p~n", [Dir, "No *.pem files found"]),
            Keys;
        FileNames ->
            KeyPaths = [{list_to_atom(filename:rootname(filename:basename(F))), F} || F <- FileNames ],
            {ok, NewKeys} = load_keyring_files(KeyPaths, Keys),
            NewKeys
    end.


%%%
%%% Load a list of {keyname, filename} pairs into the keys dictionary
%%%
load_keyring_files([], Keys) ->
    {ok, Keys};
load_keyring_files([{Name, Path}|T], Keys) ->
    case key_from_file(Name, Path) of
        {ok, Key} -> load_keyring_files(T, dict:store(Name, Key, Keys));
        _ -> load_keyring_files(T, Keys)
    end.


%%%
%%% Load key from file
%%%
key_from_file(Name, File) ->
    case file:read_file(File) of
        {ok, RawKey} ->
            case chef_authn:extract_public_or_private_key(RawKey) of
                {error, bad_key} ->
                    error_logger:error_msg("Failed to decode PEM file ~s for ~p~n", [File, Name]),
                    {error, bad_key};
                PrivateKey when is_tuple(PrivateKey) ->
                    KeyType = element(1, PrivateKey),
                    error_logger:info_msg("Loaded key ~s of type ~s from file ~s ~n", [Name, KeyType, File]),
                    {ok, PrivateKey}
            end;
        Error ->
            error_logger:error_msg("Error reading file ~s for ~p: ~p~n", [File, Name, Error]),
            Error
    end.

%%% Last modified time in UTC
-include_lib("kernel/include/file.hrl").
modtime(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} ->
            erlang:localtime_to_universaltime(MTime);
        _ -> error
    end.

