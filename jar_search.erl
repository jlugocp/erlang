-module(jar_search).
-export([find/2]).

find(Dir, SearchTerm) ->
    Files = find_files([Dir], []).
    % MatchingFiles = findMatchingClass(Files, SearchTerm).
    
findMatchingClass([], SearchTerm) ->
    [];
    
findMatchingClass([File|Files], SearchTerm) ->
    %% TODO - Open file, look for class that matches SearchTerm
    findMatchingClass(Files, SearchTerm).
    
find_files([], Matches) ->
    Matches;
    
find_files([File|Files], Matches) ->
    case file:list_dir(File) of
        {ok, DirFiles}   -> find_files(Files ++ prepend_path(File, DirFiles), Matches);
        {error, enotdir} -> 
        	case does_file_match(File) of
        		match -> find_files(Files, [File|Matches]);
        		_     -> find_files(Files, Matches)
        	end;
        {error, _}    -> find_files(Files, Matches)
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
