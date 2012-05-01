%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2011-2012 Opscode Inc.

-module(pushy_db).

-export([statements/1]).

statements(DbType) when DbType == mysql;
                        DbType == pgsql ->
    File = atom_to_list(DbType) ++ "_statements.config",
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", File]),
    {ok, Statements} = file:consult(Path),
    Statements.
