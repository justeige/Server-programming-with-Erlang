-module(basic_file_server).
-export([start/1, loop/1]).

start(FileRepo) -> 
    spawn(basic_file_server, loop, [FileRepo]). % spawn is build-in and creates a new process

loop(FileRepo) ->
    receive 
        %% list all files in the file-repository
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(FileRepo)};

        %% try and fetch a file and output it to the console
        {Client, {get_file, File}} ->
            Content = filename:join(FileRepo, File),
            Client ! {self(), file:read_file(Content)};

        %% put any content into a new file
        {Client, {put_file, Name, Content}} ->
            {ok, File} = file:open(Name, [write]),
            file:write(File, Content),
            Client ! {self(), ok}

    end,
    loop(FileRepo).