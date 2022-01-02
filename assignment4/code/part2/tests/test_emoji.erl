-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"======================= Basic behaviour ======================= ", spawn,
       [ test_start_server()
       , test_shortcode_smiley()
       , test_newshotcode()
       , test_lookup()
       , test_alias()
       , test_delete()
       , test_stop()
       ]
      },
      {"======================= Analytics =======================", spawn,
        [ test_analytics()
        , test_get_analytics()
        , test_remove_analytics()
        , test_analytics_lookup()
        ]
      }
    ].

test_start_server() ->
    [{"We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end },
    {"We can call start/1 with one unique key, and it returns {ok,E}",
     fun () ->
       Initial = [{"smiley", <<240, 159, 152, 131>>}],
       ?assertMatch({ok, _}, emoji:start(Initial))
     end },
    {"We can call start/1 with two unique keys, and it returns {ok,E}",
     fun () ->
       Initial = [{"smiley", <<240, 159, 152, 131>>},{"poop", <<"\xF0\x9F\x92\xA9">>}],
       ?assertMatch({ok, _}, emoji:start(Initial))
     end },
    {"We can call start/1 with two not unique keys, and it returns {error,Reason}",
     fun () ->
       Initial = [{"smiley", <<240, 159, 152, 131>>},{"smiley", <<240, 159, 152, 131>>}],
       ?assertMatch({error, _}, emoji:start(Initial))
     end }].

test_shortcode_smiley() ->
    {"Register new shortcode",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
     end }.

test_newshotcode() ->
    [{"Register new shortcode to one unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "thumbsup", <<240,159,145,141>>))
    end },
    {"Register same shortcode to one unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"thumbsup", <<240,159,145,141>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "smiley", <<240,159,167,186>>))
    end },
    {"Register unique shortcode to the same value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"thumbsup", <<240,159,145,141>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "smileyx", <<240, 159, 152, 131>>))
    end }].

test_alias() ->
 [{"Register an alias of shortcode which doesnt exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:alias(S, "smiley", "smileyx"))
    end },
    {"Register an alias of shortcode which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(ok, emoji:alias(S, "smiley", "smileyx"))
    end },
    {"Register an alias of alias of shortcode which exists",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:alias(S, "smiley", "smileyx"),
      ?assertEqual(ok, emoji:alias(S, "smileyx", "smileyy"))
    end}].

test_lookup() ->
    [{"Register new shortcode and look it up",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley", <<240, 159, 152, 131>>),
      ?assertEqual({ok,<<240, 159, 152, 131>>}, emoji:lookup(S, "smiley"))
    end },
    {"Lookup of shortcode which doesn't exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"Lookup of shortcode with alias which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"smileyx", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual({ok, <<240, 159, 152, 131>>}, emoji:lookup(S, "smileyx"))
    end },
    {"Register a new alias and look it up",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:alias(S, "smiley", "y"),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "y"))
    end },
    {"Register a new alias's alias and look it up",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:alias(S, "smiley", "smileyx"),
      emoji:alias(S, "smileyx", "smileyy"),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "smileyy"))
    end }].


test_delete() ->
    [{"Delete shortcode from empty Initial",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))     
    end },
    {"Delete shortcode from one unique value Initial",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"Delete specific shortcode other shortcode no change",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"thumbsup", <<240,159,145,141>>},
        {"cigarette", <<240,159,154,172>>}, {"carrot", <<240,159,165,149>>}],
      {ok, S} = emoji:start(Initial),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
      ?assertEqual({ok, <<240,159,145,141>>}, emoji:lookup(S, "thumbsup")),
      ?assertEqual({ok, <<240,159,154,172>>}, emoji:lookup(S, "cigarette")),
      ?assertEqual({ok, <<240,159,165,149>>}, emoji:lookup(S, "carrot"))
    end },
    {"Delete one alias",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:alias(S, "smiley", "smileyx"),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
      ?assertEqual(no_emoji, emoji:lookup(S, "smileyx"))
    end },
    {"Delete alias of alias",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"thumbsup", <<240,159,145,141>>},
        {"cigarette", <<240,159,154,172>>}, {"carrot", <<240,159,165,149>>}],
      {ok, S} = emoji:start(Initial),
      emoji:alias(S, "smiley", "smileyx"),
      emoji:alias(S, "smileyx", "smileyy"),
      emoji:delete(S, "smileyy"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
      ?assertEqual(no_emoji, emoji:lookup(S, "smileyx")),
      ?assertEqual(no_emoji, emoji:lookup(S, "smileyy")),
      ?assertEqual({ok, <<240,159,145,141>>}, emoji:lookup(S, "thumbsup")),
      ?assertEqual({ok, <<240,159,154,172>>}, emoji:lookup(S, "cigarette")),
      ?assertEqual({ok, <<240,159,165,149>>}, emoji:lookup(S, "carrot"))
    end }].

test_analytics() ->
   [{"Add an analytics function to shortcode from empty Initial",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0))     
    end },
    {"Add an analytics function to smiley",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ?assertEqual(ok, emoji:analytics(S, "smiley",fun(_, N) -> N+1 end, "counter", 0))
    end },
    {"Add two analytics function to smiley",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ?assertEqual(ok, emoji:analytics(S, "smiley",fun(_, N) -> N+1 end, "counter", 0)),
      ?assertEqual(ok, emoji:analytics(S, "smiley",fun(_, N) -> N+1 end, "counter1", 0))
    end },
    {"Add the same label analytics to smiley",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:analytics(S, "smiley",fun(_, N) -> N+1 end, "counter", 0),
      ?assertMatch({error,_}, emoji:analytics(S, "smiley",fun(_, N) -> N+1 end, "counter", 0))
    end},
    {"Adding analytics to alias",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:alias(S, "smiley", "smileyx"),
      ?assertEqual(ok, emoji:analytics(S, "smileyx", fun(_, N) -> N+1 end, "counter", 0))     
    end }].

test_get_analytics() ->
    [{"Get the analytics of the not-exist shortcode",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the analytics of the existing shortcode empty analytics",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ?assertMatch({ok, []}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the analytics of the existing shortcode with analytic",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
      ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the two analytics of the existing shortcode two analytics",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
      emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Count1", 0),
      ?assertMatch({ok, [{"Count",0}, {"Count1",0}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the two analytics of the existing shortcode alias two analytics",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:alias(S, "smiley", "smileyx"),
      emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
      emoji:analytics(S, "smileyx", fun(_, N) -> N-1 end, "Count1", 0),
      ?assertMatch({ok, [{"Count",0}, {"Count1",0}]}, emoji:get_analytics(S, "smileyx"))     
    end }].

test_remove_analytics() ->
    [{"Remove analytics which shortcode doesn't exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:remove_analytics(S, "smiley", "Count"),
      ?assertMatch({error, _}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove analytics which label doesn't exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:remove_analytics(S, "smiley", "Count"),
      ?assertMatch({ok, []}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove analytics which shortocode alias and label exist check by shortcode",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:alias(S, "smiley", "smileyx"),
      emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
      emoji:remove_analytics(S, "smileyx", "Count"),
      ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove analytics which shortocode alias and label exist check by alias",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:alias(S, "smiley", "smileyx"),
      emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
      emoji:remove_analytics(S, "smiley", "Count"),
      ?assertEqual({ok,[]}, emoji:get_analytics(S, "smileyx"))        
    end }].

test_analytics_lookup() ->
    [{"Lookup on emtpy analytics",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:lookup(S, "smiley"),
      ?assertMatch({ok, []}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Lookup on one analytics for one shortcode",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
      ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
      emoji:lookup(S, "smiley"),
      ?assertMatch({ok, [{"Count", 1}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Lookup on two analytics for one shortcode",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
      ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
      emoji:lookup(S, "smiley"),
      ?assertMatch({ok, [{"Count", 1}]}, emoji:get_analytics(S, "smiley")),
      emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
      ?assertMatch({ok, [{"Count", 1},{"Minus", 0}]}, emoji:get_analytics(S, "smiley")),
      emoji:lookup(S, "smiley"),
      ?assertMatch({ok, [{"Count", 2},{"Minus", -1}]}, emoji:get_analytics(S, "smiley"))
    end }].

test_stop() ->
    [{"Start the server and stop it",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ?assertEqual(ok, emoji:stop(S))
    end },
    {"Start the servers and stop them",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      {ok, S1} = emoji:start(Initial),
      emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      emoji:new_shortcode(S1, "carrot", <<240,159,165,149>>),
      ?assertEqual(ok, emoji:stop(S1)),
      ?assertEqual(ok, emoji:stop(S))
    end }].  
