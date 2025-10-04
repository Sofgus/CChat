-module(server).
-export([start/1,stop/1,handler/3]).


% Record syntax for the structure of the servers state
-record(server_st, {
    channels % a list of channel and users(Clients): [{Channel, [Clients]}]
}).
% OBS! Se till att det verkligen är en lista av tuples!

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
    Pid = spawn(fun() -> loop(InitState, fun server:handler/3) end),
    register(ServerAtom, Pid),
    Pid.
% OBS jag tror inte State är nånting just nu? eller är det InitState verkligen?
loop(State, Handler) ->
    receive
        % This is what we get from client.erl handle-functions.
        {From, Ref, Command} ->
            % Using server.erl own handle-functions to compute logic.
            % case-syntax unneccessary here..
            case Handler(State, Command, From) of
                % "if" we get {response, NewState}..
                {response, ok, NewState} ->
                    % sends message back to the client.erl handler and restarts loop.
                    From ! {reply, Ref, ok},
                    loop(NewState, Handler)

                {response, {error, Reason}, NewState} ->
                    From ! {reply, Ref, {error, Reason}},
                    loop(NewState, Handler)
            end;

        {stop, _From, _Ref} -> ok
            
    end.



% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    






% Join channel handler
handler(State, {join, Channel}, From) ->
    case existChannel(Channel, State, From) of
        {error, Reason} ->
            {response, {error, Reason}, State};
        NewState -> 
            {response, ok, NewState}
    end.

% helper function, checks if channels exists in the list. Returns true or false.
existChannel(Key, State, From) ->
    case lists:keymember(Key, 1, State#server_st.channels) of
        false -> createChannel(Key, State, From);
        true -> joinChannel(Key, State, From)
    end.

% Using the Key(the channel the client gave us) and the servers State, 
% we add a new channel, and the user to that channel, 
% to the record of the server and updates it State.
% the function returns NewState.
createChannel(Key, State, From) -> 
    Channels = State#server_st.channels,
    NewChannels = [{Key, [From]} | Channels],
    NewState = State#server_st{ channels = NewChannels }.

% Using he Key(the channel the client gave us) and the servers State. 
% keyreplace() returns [{ChName, Lst}] aka the whole shebang.
% the function returns NewState.
joinChannel(Key, State, From) -> 
    case lists:keyfind(Key, 1, State#server_st.channels) of
        false -> 
            {error, no_such_channel}; 

        {value, {ChName, Lst}} -> 
            case lists:member(From, Lst) of
                true -> 
                    {error, user_already_joined};
                false -> 
                    NewChannels = 
                        lists:keyreplace(Key, 1, 
                                        State#server_st.channels, 
                                        {ChName, [From | Lst]}),

                    NewState = State#server_st{ channels = NewChannels }
            end
    end.


% Leave Channel handler
handler(State, {leave, Channel}, From) ->
    case leaveChannel(State, Channel, From) of
        {error, Reason} ->
            {response, {error, Reason}, State};
        NewState -> 
            {response, ok, NewState}
    end.



% Helper function for Leave Channel
leaveChannel(State, Channel, From) ->
    case lists:keyfind(Channel, 1, State#server_st.channels) of
        false ->
            {error, no_such_channel};

        {value, {ChName, Lst}} ->
            case lists:member(From, Lst) of
                false -> 
                    {error, user_not_joined};
                true -> 
                    NewLst = lists:delete(From, Lst),
                    NewChannels = 
                        lists:keyreplace(Channel, 1, 
                                        State#server_st.channels, 
                                        {ChName, NewLst}),
                    NewState = State#server_st{ channels = NewChannels } 
            end
    end.



% Send message handler
% Kanske ändra denna mer likt de andra handlers, nu returnerar du 
% {response, {error, no_such_channel_msg_not_send}, State};
handler(State, {message_send, Channel, Msg}, From) ->
    case lists:keyfind(Channel, 1, State#server_st.channels) of
    false ->
        {response, {error, no_such_channel_msg_not_send}, State};

    {value, {ChName, Lst}} ->
        [send_message(H, Channel, From, Msg) || H <- Lst, H =/= From],
        {response, ok, State} 
    end.
        

% Helper function to Send message, structure from Lab instructions
send_message(ToPid, Channel, FromPid, Msg) ->
    Ref = make_ref(),
    ToPid ! {request, self(), Ref, {message_receive, Channel, FromPid, Msg}},
    receive
    {result, Ref, _Data} -> ok
    end.





% For everything else
handler(State, _Unknown, _From) ->
        {response, {error, unknown_command}, State}. 












