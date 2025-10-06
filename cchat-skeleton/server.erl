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
    try genserver:request(ServerAtom, close_channels) of
        ok -> ok;
    catch

    end,
    genserver:stop(ServerAtom).




% Close-Channel-Handler for the server. 
% Looping through the servers state (list of channels) and
% closes each one with foreach loop
handler(State, close_channels) ->
    Channels = State#server_st.channels,
    lists:foreach( fun (Channel) -> close_channel(Channel) end, Channels),
    {reply, ok, State#server_st{channels = []}}.


close_channel(Channel) -> 
    try genserver:stop(Channel) of
        ok -> ok
    catch
    
    end.



% Join-Channel-Handler for the server.
% Checks if the channel exists: if false --> Creates channel
%                               if true  --> Sends a request to join that channel.
%  
handler(State, {join, Channel, From}) ->                               
    case lists:member(Channel, State#server_st.channels) of
        false -> 
            channel:createChannel(Channel, From),
            NewChannels = [Channel | State#server_st.channels],
            {reply, ok, State#server_st{channels = NewChannels}};

        % The channel-process handles the joining.  
        true -> 
            try genserver:request(Channel, {join, From}) of
                user_already_joined ->
                    {reply, user_already_joined, State};
                ok ->
                    {reply, ok, State}
                


                



% For everything else
handler(State, _Unknown, _From) ->
        {response, {error, unknown_command}, State}. 












