-module(topics_server_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
%% TODO stop and start
list_topics_should_be_initially_empty_test() ->
  topics_server:start_link(),
  Topics = topics_server:list_topics(),
  ?assertEqual([], Topics).

starting_topic_should_answer_new_topic_id_test() ->
  topics_server:start_link(),
  TopicId = topics_server:start_topic({400, majority:simple()}),
  ?assert(is_number(TopicId)).

starting_topic_should_add_it_to_topics_list_test() ->
  topics_server:start_link(),
  TopicId = topics_server:start_topic({400, majority:simple()}),
  Topics = topics_server:list_topics(),
  ?assert(lists:member(TopicId, Topics)).


starting_topic_should_add_it_to_topics_list_2_test() ->
  topics_server:start_link(),
  TopicId1 = topics_server:start_topic({400, majority:simple()}),
  TopicId2 = topics_server:start_topic({400, majority:simple()}),
  Topics = topics_server:list_topics(),
  ?assert(lists:member(TopicId1, Topics)),
  ?assert(lists:member(TopicId2, Topics)).