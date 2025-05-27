ls_xmlFields <- list(
  groups = c("gid", "sid", "group_order", "randomization_group", "grelevance"),
  group_l10ns = c("id", "gid", "group_name", "description", "language",
                  "sid", "group_order", "randomization_group", "grelevance"),
  questions = c("qid", "parent_qid", "sid", "gid", "type", "title", "preg",
                "other", "mandatory", "question_order", "scale_id",
                "same_default", "relevance", "modulename", "encrypted",
                "question_theme_name", "same_script"),
  subquestions = c("qid", "parent_qid", "sid", "gid", "type", "title",
                   "preg", "other", "mandatory", "question_order", "scale_id",
                   "same_default", "relevance", "modulename", "encrypted",
                   "question_theme_name", "same_script"),
  question_l10ns = c("id", "qid", "question", "help", "language", "script"),
  answers = c("aid", "qid", "code", "sortorder", "assessment_value", "scale_id"),
  answer_l10ns = c("id", "aid", "answer", "language"),
  question_attributes = c("qid", "attribute", "value", "language")
);
