-module(jar_search).
-export([find/1]).

find(Dir) ->
    get_files([Dir], []).
    
get_files([], Matches) ->
    Matches;
    
get_files([File|Files], Matches) ->
    case file:list_dir(File) of
        {ok, DirFiles}   -> get_files(Files ++ prepend_path(File, DirFiles), Matches);
        {error, enotdir} -> 
        	case does_file_match(File) of
        		match -> get_files(Files, [File|Matches]);
        		_     -> get_files(Files, Matches)
        	end;
        {error, _}       -> get_files(Files, Matches)
    end.

prepend_path(Path, []) ->
    [];

prepend_path(Path, [File|Files]) ->
    [Path ++ "/" ++ File | prepend_path(Path, Files)].

does_file_match(File) ->
	case regexp:match(File, ".+\.jar") of
		{match, _, _} -> match;
		nomatch       -> nomatch
	end.

%does_file_match2(File) ->
%    {ok, MP} = re:compile(".+\.jar"),
%    case re:run(File, MP) of
%        {match, _} -> true;
%        _          -> false
%    end.
