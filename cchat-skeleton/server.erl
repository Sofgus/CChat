-module(server).
-export([start/1,stop/1]).





% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    Pid = spawn(fun() -> loop() end),
    register(ServerAtom, Pid),
    Pid.
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
loop() ->
    receive
        {Join, From} ->
            From ! {ok, joined},
            loop();


        {Leave, From} ->
            From ! {ok, left},
            loop();

        {SendMsg, From} ->
            From ! {ok, msgSent},
            loop()
    end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.


default_handler(State, _Request) ->
    {reply, ok, State}.