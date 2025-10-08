-module(channel).
-export([handler/2, createChannel/2]).



% The channels state, a registered name and a list of all users belonging to the channel.
-record( channel_st, {
    chName,
    user = []
}).



% Initializing the state of the channel. Used when a channel is created in "createChannel".
init_state(ChName, UserPid) ->
    #channel_st{
        chName = ChName, 
        user = [UserPid]
    }.


% Using genserver:start/3 to start a new process for a channel.
% A Pid is returned for a new channel process.
createChannel(Channel, UserPid) -> 
    genserver:start(Channel, init_state(Channel, UserPid), fun channel:handler/2).



% Join-Channel-Handler for the channel. 
% Checks if the one who sent the join-request to the channel is already a user or not.
%                       if false --> The user is added to the channel.
%                       if true  --> The handler returns "user_already_joined".
handler(State, {join, From}) ->
    case is_member(State, From) of
        false ->
            NewState = add_user(State, From),
            {reply, ok, NewState};
        true ->
            {reply, user_already_joined, State}
    end;

% Leave-Channel-Handler for the channel.
% Checks if the one who sent the request is a already a member of the channel or not.
%                       if false --> The handler returns "user_not_joined".
%                       if true  --> The user gets deleted from the channel.
handler(State, {leave, From}) ->
    case is_member(State, From) of
        false ->
            {reply, user_not_joined, State};
        true ->
            NewState = remove_user(State, From),
            {reply, ok, NewState}
    end;

% Send-Message-Handler for the channel.
% Checks if the one who sent the request is indeed a member of the channel or not.
%                          if false --> The handler returns "user_not_joined".
%                          if true  --> The function send_msg_process creates a new process
%                                       for sending the message to everyone in the channel.
handler(State, {message_send, Msg, Nick, From}) ->
    case is_member(State, From) of
        false ->
            {reply, user_not_joined, State};
        true ->
            send_msg_process(State, Msg, Nick, From),
            {reply, ok, State}
    end.







% //////////////////////////////////// Helper functions //////////////////////////////////// %


% Helper function to Join-Channel-Handler.
% Copies the list to variable Users. 
% List construction to add the new user to Users.
add_user(State, User) ->
    Users = State#channel_st.user,
    State#channel_st{user = [User | Users]}.

% Helper function to Leave-Channel-Handler.
% Copies the list to variable Users.
% Deletes the user from the list Users. 
% Updates the channel state.
remove_user(State, User) ->
    Users = lists:delete(User, State#channel_st.user),
    State#channel_st{user = Users}.

% Just made this to avoid code-duplication in Handler-functions..lol.
% Returns true or false.
is_member(State, User) ->
    lists:member(User, State#channel_st.user).




% Spawns a new process in charge of sending a message to everyone in the channel.
% We are spawning a new process for every client so we are not blocking the channel.
% Note that this becomes an asynchronous call, since when we are spawning a new process
% for every client and the channel does not wait for a response. 
% So the channel becomes free to handle other types of requests. 
send_msg_process(State, Msg, Nick, Sender) ->
    Users = State#channel_st.user,
    ChName = State#channel_st.chName,
    Receivers = get_receivers(Users, Sender),

    iterate_through_users(ChName, Msg, Receivers, Nick),
    ok.


% Helper function to send_msg_process.
% Deletes the one sending the message in the list of receivers of a message.
% The sender does not need it's own message, obviously.
get_receivers(Users, Sender) ->
    lists:delete(Sender, Users).



% Sending the message to everyone registered in the channel using foreach.
% With lists:foreach and spawn_msg_to_client we spawn a new process for every client, delivering the message.
iterate_through_users(ChName, Msg, Receivers, Nick) ->
    lists:foreach( fun(Receiver) -> spawn_msg_to_client(ChName, Msg, Nick, Receiver) end, Receivers).


% Helper function to iterate_through_users.
% Spawns a process to send a message to a client.
spawn_msg_to_client(ChName, Msg, Nick, Receiver) ->
    spawn(fun() -> send_msg_to_client(ChName, Msg, Nick, Receiver) end).


% Helper function to spawn_msg_to_client.
% Sending the message to a client.
send_msg_to_client(ChName, Msg, Nick, Receiver) ->
    try genserver:request(Receiver, {message_receive, atom_to_list(ChName), Nick, Msg} ) of
        ok -> ok
    catch
        throw:timeout_error ->
            no_response
        end.


