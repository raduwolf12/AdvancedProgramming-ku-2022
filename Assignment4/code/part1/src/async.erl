-module(async).

-export([new/2, wait/1, poll/1]).

% async:new(fun(T) -> io:format("hello\n"), timer:sleep(T), io:format("goodbye\n") end, 20000).
% async:wait(<0.86.0>).
% async:poll(<0.86.0>).

new(Fun, Arg) -> spawn(fun() -> 
    Me = self(),
    execute_function(Me,Fun,Arg), 
    loop({start})
    end).

execute_function(Aid, Fun, Arg) ->spawn_link (fun()->
    try 
        Result=Fun(Arg),
        Aid!{ok,Result}
    catch
        _:Exception->Aid!{error,Exception}
        end
    end).



wait(Aid) -> 
    case poll(Aid) of 
        nothing -> wait(Aid);
        {ok, Res}->Res;
        {exception,Exception}->throw(Exception)
    end.
            
poll(Aid) -> 
    Aid!{self(),getState},
    receive 
        {ok, Res} -> {ok, Res};
        {error,Exception} -> {exception,Exception };
        {nothing}->nothing
    end.

loop(State)->
    receive
        {ok,Res} -> loop({ok,Res});
        {error,Reason} -> loop({error,Reason});
        {From,getState}->
            case State of
                {start} -> From!{nothing},loop(State);
                _->From!State,loop(State)
            end
    end.



