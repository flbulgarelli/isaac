-module(topics_server_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


list_topics_should_be_initially_empty_test() ->
  Server = topics_server:start_link(),
  Topics = topics_server:list_topics(),
  ?assertEqual([], Topics).

starting_topic_should_answer_new_topic_id_test() ->
  Server = topics_server:start_link(),
  TopicId = topics_server:start_topic({400, majority:simple()}),
  ?assertEqual(1, TopicId).

starting_topic_should_add_it_to_topics_list_test() ->
  Server = topics_server:start_link(),
  TopicId = topics_server:start_topic({400, majority:simple()}),
  Topics = topics_server:list_topics(),
  ?assertEqual([TopicId], Topics).