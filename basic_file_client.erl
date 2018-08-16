-module(basic_file_client).
-export([ls/1, get_file/2, put_file/3, download_file/2]).

ls(Server) ->
    Server ! {self(), list_dir},
    receive 
        {Server, FileList} -> FileList
    end.

get_file(Server, FileName) ->
    Server ! {self(), {get_file, FileName}},
    receive 
        {Server, File} -> File
    end.

put_file(Server, Name, Content) ->
    Server ! {self(), {put_file, Name, Content}}.

% create a download directory for the client and store a file in it
download_file(Server, Name) ->
    io:fwrite("~p~n", [file:make_dir("Downloads")]),
    Server ! {self(), {get_file, Name}},
    receive 
        {Server, Content} ->
            {ok, File} = file:open("Downloads//" ++ Name, [write]),
            file:write(File, Content) %%% TODO: why does this only create the file, but doesn't put the content?
    end.
            
