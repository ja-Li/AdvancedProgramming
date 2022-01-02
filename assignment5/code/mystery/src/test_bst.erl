-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A non-symbolic generator for bst, parameterised by key and value generators
% bst1(Key, Value) ->
%     ?LET(KVS, eqc_gen:list({Key, Value}),
%          lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
%                      empty(),
%                      KVS)).

bst() ->
    ?LAZY(
        frequency([
            {1,{call, bst, empty, []}},
            {4,?LETSHRINK([T],[bst()],
                {call, bst, insert, [int_key(), int_value(), T]})}
            ])
        ).

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
% prop_arbitrary_valid1() ->
%     ?FORALL(T, bst1(atom_key(), int_value()),
%             valid(T)).

prop_arbitrary_valid() ->
    ?FORALL(T, bst(),
            valid(eval(T))).

% if we insert into a valid tree it stays valid
% prop_insert_valid() ->
%     ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
%             valid (insert(K, V, T))).

prop_insert_valid() ->
    ?FORALL({K, V, T}, {int_key(), int_value(), bst()},
            valid (insert(K, V, eval(T)))).

prop_delete_valid() ->
    ?FORALL({K, T},{int_key(),bst()},
            valid (delete(K, eval(T)))).

prop_empty_valid() ->
    ?FORALL(T, empty(),
            valid(eval(T))).

prop_union_valid() ->
    ?FORALL({T1, T2},{bst(), bst()},
            valid (union(eval(T1), eval(T2)))).

%%% -- postcondition properties

% prop_insert_post() ->
%     ?FORALL({K1, K2, V, T},
%             {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
%             eqc:equals(find(K2, insert(K1, V, T)),
%                        case K1 =:= K2 of
%                            true ->  {found, V};
%                            false -> find(K2, T)
%                        end)).

prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {int_key(), int_key(), int_value(), bst()},
            eqc:equals(find(K2, insert(K1, V, eval(T))),
                        case K1 =:= K2 of 
                            true -> {found, V};
                            false -> find(K2, eval(T))
                        end)).

prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    % ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
    %         eqc:equals(find(K, insert(K, V, T)),
    %                    {found, V})).
    ?FORALL({K, V, T},
            {int_key(), int_value(), bst()},
            eqc:equals(find(K, insert(K, V, eval(T))), {found, V})).

prop_find_post_absent() ->
     % ∀ k t. find k (delete k t) === nothing
    ?FORALL({K, T},
            {int_key(), bst()},
            eqc:equals(find(K, delete(K, eval(T))), nothing)). 

prop_delete_post() ->
    ?FORALL({K1, K2, T},
            {int_key(), int_key(), bst()},
            eqc:equals(find(K2, delete(K1, eval(T))),
                        case K1 =:= K2 of
                           true -> nothing;
                           false -> find(K2, eval(T))
                        end)).

prop_union_post() ->
    ?FORALL({K, T1, T2},
            {int_key(), bst(), bst()},
            eqc:equals(find(K, union(eval(T1), eval(T2))),
                        case find(K, eval(T1)) of
                            {found, V} -> {found, V};
                            nothing -> find(K, eval(T2))
                        end)).

%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    % ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
    %         bst:size(insert(K, V, T)) >= bst:size(T)).
    ?FORALL({K, V, T},
            {int_key(), int_value(), bst()},
            bst:size(insert(K, V, eval(T)) >= bst:size(eval(T)))).


obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(eval(T1)), to_sorted_list(eval(T2))).

% prop_insert_insert() ->
%     ?FORALL({K1, K2, V1, V2, T},
%             {atom_key(), atom_key(), int_value(), int_value(),
%              bst(atom_key(), int_value())},
%             obs_equals(insert(K1, V1, insert(K2, V2, T)),
%                        case K1 =:= K2 of
%                            true ->  insert(K1, V1, T);
%                            false -> insert(K2, V2, insert(K1, V1, T))
%                        end)).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {int_key(), int_key(), int_value(), int_value(), bst()},
            obs_equals(insert(K1, V1, insert(K2, V2, eval(T))),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, eval(T));
                           false -> insert(K2, V2, insert(K1, V1, eval(T)))
                       end)).

prop_insert_delete() ->
    ?FORALL({K1, K2, V1, T},
            {int_key(), int_key(), int_value(), bst()},
            obs_equals(insert(K1, V1, delete(K2, eval(T))),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, eval(T));
                           false -> delete(K2, insert(K1, V1, eval(T)))
                       end)).

prop_insert_Union() ->
    ?FORALL({K, V, T1, T2},
            {int_key(), int_value(), bst(), bst()},
            obs_equals(insert(K, V, union(eval(T1), eval(T2))),
                       union(insert(K, V, eval(T1)), eval(T2)))).

prop_delete_empty() ->
    ?FORALL({K, T},
            {int_key(), empty()},
            eqc:equals(delete(K, eval(T)), leaf)).

prop_delete_insert() ->
    ?FORALL({K1, K2 , V2, T},
            {int_key(), int_key(), int_value(), bst()},
            obs_equals(delete(K1, insert(K2, V2, eval(T))),
                        case K1 =:= K2 of
                            true -> delete(K1, eval(T));
                            false -> insert(K2, V2, delete(K1, eval(T)))
                        end)).
prop_delete_delete() ->
    ?FORALL({K1, K2, T},
            {int_key(), int_key(), bst()},
            obs_equals(delete(K1, delete(K2, eval(T))), 
                       delete(K2, delete(K1, eval(T))))).

prop_delete_union() ->
    ?FORALL({K, T1, T2},
            {int_key(), bst(), bst()},
            obs_equals(delete(K, union(eval(T1), eval(T2))), 
                       union(delete(K, eval(T1)), delete(K, eval(T2))))).

prop_union_empty1() ->
    ?FORALL(T,bst(),
            eqc:equals(union(empty(), eval(T)), 
                       eval(T))).

prop_union_empty2() ->
    ?FORALL(T,bst(),
            eqc:equals(union(eval(T), empty()), 
                eval(T))).

%%% -- Model based properties
model(T) -> to_sorted_list(T).


% prop_insert_model() ->
%     ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
%             equals(model(insert(K, V, T)),
%                    sorted_insert(K, V, delete_key(K, model(T))))).
prop_insert_model() ->
    ?FORALL({K, V, T},
            {int_key(), int_value(), bst()},
            eqc:equals(model(insert(K, V, eval(T))),
                       sorted_insert(K, V, delete_key(K, model(eval(T)))))).

-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].

prop_find_model() ->
    ?FORALL({K, T},
            {int_key(), bst()},
            eqc:equals(find(K,eval(T)),
                       lookup_key(K, model(eval(T))))).

lookup_key(_, []) -> nothing;
lookup_key(Key, [{K,V} | Rest]) -> 
    case Key =:= K of
        true -> {found, V};
        false -> lookup_key(Key, Rest)
    end.

prop_empty_model() -> 
    ?FORALL(T, empty(),
            eqc:equals(model(eval(T)),[])).

prop_delete_model() ->
    ?FORALL({K, T},
            {int_key(), bst()},
            eqc:equals(model(delete(K, eval(T))), 
                       delete_key(K, model(eval(T))))).

prop_union_model() ->
    ?FORALL({T1, T2},
            {bst(), bst()},
            eqc:equals(model(union(eval(T1), eval(T2))),
                       sorted_union(model(eval(T1)), model(eval(T2))))).

sorted_union(T, []) -> T;
sorted_union([], T) -> T;
sorted_union([{K1, V1} | Rest1],[{K2, V2} | Rest2]) when K1 < K2 ->
    [{K1,V1} | sorted_union(Rest1, [{K2,V2} | Rest2])];
sorted_union([{K1, V1} | Rest1],[{K2, V2} | Rest2]) when K1 > K2 ->
    [{K2,V2} | sorted_union([{K1,V1} | Rest1], Rest2)];
sorted_union([{K1, V1} | Rest1],[{K2, _} | Rest2]) when K1 =:= K2 ->
    [{K1,V1} | sorted_union(Rest1,Rest2)].


%% -- Test all properties in the module: eqc:module(test_bst)


%% Test quality of generators
prop_aggregate() ->
        ?FORALL(T, bst(),
             aggregate(call_names(T), true)).

% prop_measure() ->
%         ?FORALL(T, bst(),
%              collect(length(to_sorted_list(eval(T))),true)).
