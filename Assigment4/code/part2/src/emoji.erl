-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

% start(Initial) 😄 for starting an Emoji server, with the initial shortcodes definition 
% given by Initial. The type of Initial should be a list of pairs, where the first component 
% is a string and the second is a binary.
% All shortcodes in Initial should be unique, otherwise it is an error.
% Returns {ok, E} on success, or {error, Reason} if some error occurred.

start(Initial) ->
    ListShortCode = to_list_shortcode(Initial),
    Result = check_unique(ListShortCode),
    % Map=#{},
    if length(ListShortCode)=/=0 ->
          Map = create_map_alias_add(ListShortCode,#{});
          true ->
            Map=#{}
    end,
    io:format("~p~n", [Map]),
	if Result =:= ok ->
		E = spawn(fun() ->  io:format("Server start\n"), loop(Initial,Map) end),
		{Result, E};
    Result =:= error ->{Result, "Duplicate shortcodes"}
	end.

create_map_alias_add(List, Map)->
  case List of
  []->ers;
  [X|[]] -> maps:put(X,[X],Map);
  [A|B] ->
    maps:put(A,[A],create_map_alias_add(B,Map))
    % Map=create_map_alias_add(B,Map)
  end.
 

create_map_alias_insert(Short, Alias, Map)-> 
  maps:put(Short,[Short],Map). 

create_map_alias(Short, Alias, Map)->

    List=maps:get(Short, Map),
    io:format("~p~n", [List]), 
    
    List1=lists:append(List,[Alias]),
    io:format("~p~n", [List]), 
    Map1=maps:put(Short,List1,Map),
    Map1 . 


check_unique(List) -> 
  case List of 
    []->ok;
    [_|[]] -> ok;
    [A|B] ->
      case lists:member(A, B) of
        true -> error;
        false -> check_unique(B)
      end
 end.

to_list_shortcode(ListEmoji) ->
  lists:map(fun(Emoji)->case Emoji of {ShortCode, _}->ShortCode end end, ListEmoji).


loop(ListEmoji,MapAlias)->
  receive
      {From,stop}->From!{self(),ok};
      {From,alias,Short1,Short2}->
        TempList=[{Short1,Short2}|ListEmoji],
          Result=check_unique(TempList),
          case Result of
            error -> From!{self(),{error, "The Short1 is not registered!"}},loop(ListEmoji,MapAlias); 
            ok -> From!{self(),ok}, 
            MapAliasNew=create_map_alias(Short1, Short2, MapAlias), 
            io:format("~p~n", [MapAliasNew]), 
            
            loop(TempList,MapAliasNew)
            end;
      {_,delete,Short}->
        TempList=[{Short,Short}|ListEmoji],
        Result=check_unique(TempList),
        case Result of
          error -> loop(ListEmoji,MapAlias);
          ok -> TempListShortCode=to_list_shortcode(TempList),
            NewList=delete_from_list(TempListShortCode,Short),
            % NewListEmoji=delete_from_list_tuple(ListEmoji,Short),

            NewListEmoji=proplists:delete(Short, ListEmoji),
            MapAliasNew=maps:remove(Short, MapAlias),

          % MapAliasNew=create_map_alias(Short, Short, MapAlias), 
          io:format("~p~n", [NewListEmoji]), 
          io:format("~p~n", [MapAliasNew]), 
          loop(NewListEmoji,MapAliasNew)
            end;
      {From,new_shortcode,Short,Emo}->
        TempList=[{Short,Emo}|ListEmoji],
        io:format("~p~n", [TempList]),
          Result=check_unique(TempList),
          case Result of 
            ok -> From!{self(),ok}, 
            MapAliasNew=create_map_alias_insert(Short, Short, MapAlias), 
            io:format("~p~n", [MapAliasNew]), 
            
            loop(TempList,MapAliasNew);
            error -> From!{self(),{error, "Duplicate shortcode"}},loop(ListEmoji,MapAlias)
          end
        end.
        
        
new_shortcode(E, Shortcode, Emoji) -> 
  E!{self(),new_shortcode,Shortcode,Emoji},
  receive
      {E,ok}->ok;
      {E,{error,Reason}}->{error, Reason}
  end.


alias(E,Short1, Short2) -> 
  E!{self(),alias,Short1,Short2},
  receive
      {E,ok}->ok;
      {E,{error,Reason}}->{error, Reason}
  end.


delete(E, Short) -> 
    E!{self(),delete,Short},
    ok.

delete_from_list(List,Short)->
  remove(Short,List).


remove(X, L) ->
    [Y || Y <- L, Y =/= X].


lookup(E, Short) ->
    E!{self(),lookup,Short}, 
    receive
        {E,{ok,Emo}}->{ok,Emo};
        {E,no_emoji}->no_emoji
    end.

% lookupValueList(Short,List,ListTuple,AliasMap) ->
%   case List of 
%     []->ok;
%     [X|[]] -> ok;
%     [A|B] ->
%       case lists:member(A, B) of
%         true -> error;
%         false -> check_unique(B)
%       end
%     end.

get_emoji(List,Key)->
  proplists:lookup(Key,List).

get_key_from_map(MapList,Value)->
  maps:get(Value, MapList).

check_if_a_short_in_list(Short,List)->
  lists:search(Short,List).

get_first_element_list(List)->
  [A || {A,_,_} <-List].

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(Server) -> 
  Server!{self(),stop},
  receive 
      {Server,ok}->ok;
      {Server,{error,Reason}}->{error, Reason}
  end.
