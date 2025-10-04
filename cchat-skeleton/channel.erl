% The channels state, a registered name and a list of all users belonging to the channel.
-record( channel_st, {
    chName,
    user = []
}).



% Initializing the state of the channel. Used when a channel is created in "createChannel".
init_state(ChName, UserPid) ->
    #channel_st{
        chName = ChName, 
        user = UserPid
    }.


% Using genserver:start/3, a Pid is returned for a new channel process.
createChannel(Channel, UserPid) -> 
    genserver:start(Channel, init_state(Channel, UserPid), fun channel:handler/2 )



% Join-Channel-Handler for the channel. 
% Checks if the one who sent the request is already a user or not.
%                       if false --> The user is added to the channel.
%                       if true  --> The handler returns "user_already_joined".
handler(State, {join, From}) ->
    case is_member(State, From) of
        false ->
            NewState = add_user(State, From),
            {reply, ok, NewState};
        true ->
            {reply, user_already_joined, State}
    end.



% Leave-Channel-Handler for the channel.
% Checks if the one who sent the request is a already a member of the channel or not.
%                       if false --> The handler returns "user_not_member_of_channel".
%                       if true  --> The user gets deleted from the channel.
handler(State, {leave, From}) ->
    case is_member(State, From) of
        false ->
            {reply, user_not_member_of_channel, State};
        true ->
            NewState = remove_user(State, From),
            {reply, ok, NewState}
    end.



% Send-Message-Handler for the channel.
%
handler(State, {message_send, Msg, self()}) ->

    




% //////////////////////////////////// Helper functions //////////////////////////////////// %


% Helper function to Join-Channel-Handler.
% Copies the list to variable Users. 
% List comprehension to add the new user to Users.
add_user(State, User) ->
    Users = State#channel_st.user,
    State#channel_st{user = [User | Users]}.

% Helper function to Leave-Channel-Handler.
% Copies the list to variable Users.
% Deletes the user from the Users. Updates the channel state.
remove_user(State, User) ->
    Users = lists:delete(User, State#channel_st.user),
    State#channel_st{user = Users}.

% Just made this to avoid code-duplication in handler-functions.
is_member(State, User) ->
    lists:member(User, State#channel_st.user).