-module(server).
-export([start/1,stop/1,handler/2]).


% Record syntax for the structure of the servers state
-record(server_st, {
    channels % a list of {Channel, [Clients]}
}).


% Initializing the state to just an empty list. 
% Later to be filled with channels and the clients belonging to the respective channel
init_state() ->
    #server_st{
        channels = []
    }.


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    InitState = init_state(),
    Pid = spawn(fun() -> loop(InitState, fun server:handler/2) end),
    register(ServerAtom, Pid),
    Pid.

loop(State, Handler) ->
    receive
        % This is what we get from client.erl handle-functions.
        {From, Ref, Command} ->
            % Using server.erl own handle-functions to compute logic.
            % case-syntax unneccessary here..
            case Handler(State, Command) of
                % "if" we get {response, NewState}..
                {response, NewState} ->
                    % sends message back to the client.erl handler and restarts loop.
                    From ! {reply, Ref},
                    loop(NewState, Handler)
            end;

        {stop, _From, _Ref} -> ok
            
    end.



% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).
















% Join channel 
handler(State, {join, Channel}) ->
        {reply, okJoin, State}; 

% Leave channel
handler(State, {leave, Channel}) ->
        {reply, okLeave, State}; 

% Send msg
handler(State, {message_send, Channel, Msg}) ->
        {reply, okMsgSend, State}; 

% For everything else
handler(State, _) ->
        {reply, {error, unknown_command}, State}. 


