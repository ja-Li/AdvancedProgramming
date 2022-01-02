-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

%% starts up the emoji server and make sure every pair in list is unque.
-spec start([{string(), emoji()}]) -> any().
start(Initial) -> 
    Alias = orddict:new(),
    Analytics = [],
    ShortcodesOnly = lists:map(fun({Shortcode, _}) -> Shortcode end, Initial),
    case length(ShortcodesOnly) == sets:size(sets:from_list(ShortcodesOnly)) of
        true-> 
               case Initial of 
                   [] -> E = spawn(fun() -> loop({Initial, Alias, Analytics}) end),
                         {ok, E};
                    _ -> NewInitial = [{X,[Y]} ||{X, Y} <-Initial],
                         E = spawn(fun() -> loop({orddict:from_list(NewInitial), Alias, Analytics}) end),
                         {ok, E}
               end;
        false -> {error, "Duplicates in Initial"}
    end.

%% Main server loop.
% State = {Initial, Alias, Analytics}
loop(State) ->
    receive
        {From, stop} -> From ! {ok, "over loop"};
        {From, Request} -> 
            {Res, NewState} = handle_call(Request, State),
            From ! Res,
            loop(NewState);
        % this is for lookup function
        {From, Request, worker} ->
            Me = self(),
            Worker = spawn(fun() -> 
                % if we catch a error we will raise a exception
                try
                    {Res, NewState} = handle_call(Request, State),
                    From ! Res,
                    Me ! {self(), NewState}
                catch
                    _:Reason -> Me ! {error, Reason}
                end
            end),
            NewState = 
                receive
                    {Worker, New} -> New;
                    {error, Reason} -> From ! {error, Reason}
                end,
            loop(NewState)
    end.
%% add new non-existing short codes.
new_shortcode(E, Short, Emo) -> 
    E ! {self(), {new_shortcode_request, Short, Emo}},
    receive 
        {ok} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Add an alias for a short code.
alias(E, Short1, Short2) -> 
    E ! {self(), {alias_request, Short1, Short2}},
    receive
        {ok} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Delete a short code.
delete(E, Short) -> 
    E ! {self(), {delete_request, Short}}.

%% Look up a short code. 
%% Subsequently runs all the attached analytics functions.
%% analytics function may run a very long time. so
%% we need to use a seperate process to deal with.
lookup(E, Short) -> 
    E! {self(), {look_request, Short}, worker},
    receive
        {no_emoji} -> no_emoji;
        {ok, Emo} -> {ok, Emo}
    end.

%% Add an analytics functions to a short code.
- spec analytics(pid(), string(), analytic_fun(_), string(), _) -> any().
analytics(E, Short, Fun, Label, Init) -> 
    E! {self(), {analytics_request, Short, Fun, Label, Init}},
    receive 
        {ok} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Get the results of the analytics functions of a given short code.
get_analytics(E, Short) -> 
    E ! {self(), {get_analytics_request, Short}},
    receive
        {ok, Stat} -> {ok, Stat};
        {error, Reason} -> {error, Reason}
    end.

%% Remove an analytics functions from a short code.
remove_analytics(E, Short, Label) -> 
  E ! {self(), {remove_analytics_request, Short, Label}}.

%% Stop the emoji server.
stop(E) -> 
    E ! {self(), stop},
    receive
        {ok, _} -> ok
    end.

%% seperation of concerns
handle_call(Request, State) ->
    case Request of
        {new_shortcode_request, Short, Emo} -> handle_new_shortcode(Short, Emo, State);
        {alias_request, Short1, Short2} -> handle_alias(Short1, Short2, State);
        {delete_request, Short} -> handle_delete(Short, State);
        {analytics_request, Short, Fun, Label, Init} -> handle_analytics(Short, Fun, Label, Init, State);
        {look_request, Short} -> handle_lookup(Short, State);
        {get_analytics_request, Short} -> handle_get_analytics(Short, State);
        {remove_analytics_request, Short, Label} -> handle_remove_analytics(Short, Label, State)
    end.

%%%% refactored receive functions from the loop %%%%

handle_new_shortcode(Short, Emo, {Initial, Alias, Analytics}) ->
    case orddict:is_key(Short, Initial) of
                true -> 
                    Res = {error, "This short name is existed"},
                    NewState = {Initial, Alias, Analytics};
                false ->
                    NewInitial = orddict:append(Short, Emo, Initial),
                    Res = {ok},
                    NewState = {NewInitial, Alias, Analytics}
    end,
    {Res, NewState}.

handle_alias(Short1, Short2, {Initial, Alias, Analytics}) -> 
    case orddict:is_key(Short1, Initial) of
        false ->
            Res = {error, "This short name not existed"},
            NewState = {Initial, Alias, Analytics};
        true ->
            case orddict:is_key(Short2, Initial) of
                true -> 
                    Res = {error, "This alias name is already register"},
                    NewState = {Initial, Alias, Analytics};
                false -> 
                    NewState = 
                        aux_alias(Short1, Short2, Alias, Initial, Analytics),
                    Res =  {ok}
            end         
    end,
    {Res, NewState}.

handle_delete(Short, {Initial, Alias, Analytics}) -> 
    Res = {ok},
    case orddict:is_key(Short, Initial) of 
        false ->
            NewState = {Initial, Alias, Analytics};
        true -> 
            NewState =
                aux_delete(Short, Alias, Initial, Analytics)
    end,
    {Res, NewState}.

handle_analytics(Short, Fun, Label, Init, {Initial, Alias, Analytics}) ->
    case orddict:is_key(Short, Initial) of 
        false -> 
            Res = {error, "The short is not be registered"},
            NewState = {Initial, Alias, Analytics};
        true -> 
            All_alias_shortcode = getallname(Short, Alias),
            Analytics_copy  = Analytics,
            case lists:search(fun({X,{_,W,_}}) -> {W, X} =:= {Label, Short} end, Analytics_copy) of
                false -> 
                    NewAnalytics = Analytics ++ 
                        [{ShortcodeA, {Fun, Label, Init}} || ShortcodeA <-All_alias_shortcode],
                    Res = {ok},
                    NewState = {Initial, Alias, NewAnalytics};
                _ -> 
                    Res = {error,"The label in this short was registered"},
                    NewState = {Initial, Alias, Analytics}
            end
    end,
    {Res, NewState}.

 handle_lookup(Short, {Initial, Alias, Analytics}) ->
     case orddict:is_key(Short, Initial) of
        false -> 
            Res = {no_emoji}, 
            NewState = {Initial, Alias, Analytics};
        true -> 
            {ok, [Emo]} = orddict:find(Short, Initial),
            Res = {ok, Emo},

            %process analytics functions
            All_alias_ShortCode = getallname(Short, Alias),
            NewAnalytics = [case lists:member(Short_B, All_alias_ShortCode) of 
                                true -> {Short_B, {Fun_B, Label_B, runAnalyticsFun(Fun_B, Short, Init_B)}};
                                false-> {Short_B, {Fun_B, Label_B, Init_B}} end
                            || {Short_B, {Fun_B, Label_B, Init_B}} <- Analytics],
            NewState = {Initial, Alias, NewAnalytics}
    end,
    {Res, NewState}.

handle_get_analytics(Short, {Initial, Alias, Analytics}) -> 
    case orddict:is_key(Short, Initial) of
        false -> Res = {error, "There are not this Short in Analystics"}, 
                 NewState = {Initial, Alias, Analytics};
        _ -> 
            Stats = [{W,  V}|| {X, {_,W,V}} <- Analytics, X =:= Short],
            Res = {ok, Stats},
            NewState = {Initial, Alias, Analytics}
    end,
    {Res, NewState}.

handle_remove_analytics(Short, Label, {Initial, Alias, Analytics}) ->
    Res = {ok},
    case lists:search(fun({X,{_,W,_}}) -> {W, X} =:= {Label, Short} end, Analytics) of 
        false -> 
            Res = {ok},
            NewState = {Initial, Alias, Analytics};
        _ ->  
            All_alias_shortcode = getallname(Short, Alias),
            NewAnalytics = [{XN, {QN,WN,VN}} || {XN, {QN,WN,VN}} <- Analytics,
                                case lists:member(XN, All_alias_shortcode) of
                                    false -> true;
                                    _ -> WN =/= Label end],
            NewState = {Initial, Alias, NewAnalytics}
    end,
    {Res, NewState}.    

%%%%% HELPER FUNCTIONS %%%%%

aux_alias(Short1, Short2, Alias, Initial, Analytics) ->
    case orddict:is_key (Short1, Alias) of 
        false -> 
            NewAlias = orddict:append(Short2, Short1, Alias);
        true -> 
            {ok, [OriginalShortcode]} = orddict:find(Short1, Alias),
            NewAlias = orddict:append(Short2, OriginalShortcode, Alias)
    end,
    {ok, [Emo_short1]} = orddict:find(Short1, Initial),
    NewInitial = orddict:append(Short2, Emo_short1, Initial),
    NewAnalytics = alias_copy_ana(Short2, Short1, Analytics),
    {NewInitial, NewAlias, NewAnalytics}.

%helper function of delete
aux_delete(Short, Alias, Initial, Analytics) -> 
    case orddict:is_key(Short, Alias) of 
        false -> 
            All_name_list = findallname(Short, Alias);
        true -> 
            {ok, [OriginalShortcode]} = orddict:find(Short, Alias),
            All_name_list = findallname(OriginalShortcode, Alias)
    end,
    All_name_list_copy = All_name_list,
    All_name_list_copy_ana = All_name_list,
    NewInitial = eraselist(All_name_list, Initial),
    NewAlias = eraselist(All_name_list_copy, Alias),
    case lists:search(fun({X, _}) -> X =:= Short end, Analytics) of
        false -> NewAnalytics = Analytics;
        _ -> NewAnalytics = [{XN, VN} || {XN, VN} <- Analytics ,
                            case lists:search(fun(X) -> X =:= XN end, All_name_list_copy_ana) of
                                    false -> true;
                                    _ ->false end]
    end,
    {NewInitial, NewAlias, NewAnalytics}.

findallname(Shortcode, Aliasfind) -> 
    Alias_allname = [Fst || {Fst, Sec}<-Aliasfind, Sec =:= [Shortcode]],
    [Shortcode|Alias_allname].

getallname(ShortCode_A, Alias_A) ->
    case orddict:is_key(ShortCode_A, Alias_A) of
        false -> AliasList = [Fst || {Fst, Sec}<-Alias_A, Sec =:= [ShortCode_A]],
                 [ShortCode_A | AliasList];
        true -> {ok, [ShortCode_B]} = orddict:find(ShortCode_A, Alias_A),
                AliasList = [Fst || {Fst, Sec}<-Alias_A, Sec =:= [ShortCode_B]],
                [ShortCode_B | AliasList]
    end.

alias_copy_ana(Alias_C, ShortCode_C, Analytics_C) ->
    case lists:search(fun({X, _}) -> X =:= ShortCode_C end, Analytics_C) of
        false -> Analytics_C;
        _ -> [{Alias_C, V_A}||{X_A, V_A}<-Analytics_C, X_A =:= ShortCode_C] ++ Analytics_C
    end.


eraselist([], Alias_erase) -> Alias_erase;
eraselist([Head|Tail], Alias_erase) -> 
    NewAlias = orddict:erase(Head, Alias_erase), eraselist(Tail, NewAlias).

%% if we catch a error in analytics functions, we will not change the State
runAnalyticsFun(F, Short, State) ->
    try 
      F(Short, State)
    catch
      _ -> State
    end.