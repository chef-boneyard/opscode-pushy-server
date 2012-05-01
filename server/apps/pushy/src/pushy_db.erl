-module(pushy_db).

-export([statements/1]).

statements(DbType) when DbType == mysql;
                        DbType == pgsql ->
    File = atom_to_list(DbType) ++ "_statements.config",
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", File]),
    {ok, Statements} = file:consult(Path),
    Statements.
