-module(server).
-export([start/1,stop/1]).


% Record syntax for the structure of the servers state
-record(server_st, {
    channels % a list of channels: [channel1, channel2, etc.]
}).


% Initializing the state to just an empty list. 
% Later to be filled with channels.
init_state() ->
    #server_st{
        channels = []
    }.


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, init_state(), fun server:handler/2).




% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    






% Join-Channel-Handler for the server.
% Checks if the channel exists: if false --> Creates channel
%                               if true  --> Sends a request to join that channel.
%  
handler(State, {join, Channel, From}) ->                               
    case lists:member(Channel, State#server_st.channels) of
        false -> 
            channel:createChannel(Channel, From),
            NewChannels = [Channel | State#server_st.channels],
            {reply, ok, State#server_st.channels = NewChannels};

        % The channel-process handles the joining.  
        true -> 
            try genserver:request(Channel, {join, From}) of
                user_already_joined ->
                    {reply, user_already_joined, State};
                ok ->
                    {reply, ok, State}
                


                
















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












