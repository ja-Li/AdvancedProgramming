-module(async).

-export([new/2, wait/1, poll/1]).

% spawns a supervisor process to call the loop.
new(Fun, Arg) ->
    spawn (fun() -> action(Fun, Arg) end).

% spawns a working process to handle the function and get the result.
action(Fun,Arg) ->
    Aid = self(),
    spawn_link(fun() ->
        Res = try
            {ok,Fun(Arg)}
        catch
            E -> {exception,E}
        end,
        Aid ! {done,Res}
    end),
    loop({none,false}).

% Waits the result until process is completed.
wait(Aid) ->
    % Aid ! {self(),get_state},
    % receive
    %     % nothing -> wait(Aid);
    %     % Add sleep.
    %     nothing -> timer:sleep(100),wait(Aid);
    %     {ok, FinalRes} -> FinalRes;
    %     {exception, E} -> throw(E)
    % end.
    case poll(Aid) of
        nothing -> timer:sleep(100),wait(Aid);
        {ok, FinalRes} -> FinalRes;
        {exception, E} -> throw(E)
    end.

% Get the feedback immediately.
poll(Aid) ->
    Aid ! {self(),get_state},
    receive
        nothing -> nothing;
        {ok, FinalRes} -> {ok, FinalRes};
        {exception, E} -> {exception, E}
    end.

loop({Res, IsFinished} = State) ->
    receive
        {done, NewRes} ->
            NewState = {NewRes,true},
            loop(NewState);
        {From, get_state} ->
            case IsFinished of true -> From ! Res;
                               false-> From ! nothing
            end,
            loop(State)
        % Failed in onlineTA, so I changed the method.
        % {From, wait} ->
        %     case IsFinished of true -> From ! Res;
        %                        false -> wait_loop(From)
        %     end,
        %     loop(State);
    end.