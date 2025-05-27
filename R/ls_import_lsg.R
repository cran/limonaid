ls_import_lsg <- function(x) {

  res <- xml2::read_xml(x);

  return(res);

}


# # devtools::load_all();
# a <- ls_import_lsg(system.file("extdata", "limesurvey_group_1370.lsg", package="limonaid"));
# a2 <- ls_import_lsg(file.path("C:", "pC", "git", "limesurvey", "exports", "limesurvey_group_2689.lsg"));
# b <- xml2::as_list(a);
# b2 <- xml2::as_list(a2);
# c <- xml2::as_xml_document(b);
# identical(a, c); ### Not identical because {xml2} doesn't write with CDATA
# xml2::write_xml(c, here::here("inst", "extdata", "limesurvey_group_1370_rewritten.lsg"))
# # ### ^ This imports correctly though
#
# att_qid <- unlist(lapply(b2$document$question_attributes$rows, \(x) x$qid));
# att_attribute <- unlist(lapply(b2$document$question_attributes$rows, \(x) x$attribute));
# att_value <- lapply(b2$document$question_attributes$rows, \(x) x$value);
# att_value <- unlist(ifelse(unlist(lapply(att_value, length)), att_value, NA));
# att_language <- lapply(b2$document$question_attributes$rows, \(x) x$language);
# att_language <- unlist(ifelse(unlist(lapply(att_language, length)), att_language, NA));
#
# attributes_by_qid <- lapply(unique(att_qid),
#                             \(x) stats::setNames(att_value[att_qid %in% x],
#                                                  att_attribute[att_qid %in% x]));
# names(attributes_by_qid) <- unique(att_qid);
#
# attNames_by_qid <- lapply(unique(att_qid),
#                           \(x) unname(att_attribute[att_qid %in% x]));
# names(attNames_by_qid) <- unique(att_qid);
#
# attLanguage_by_qid <- lapply(unique(att_qid),
#                              \(x) stats::setNames(att_language[att_qid %in% x],
#                                                   att_attribute[att_qid %in% x]));
# names(attLanguage_by_qid) <- unique(att_qid);
#
# for (i in unique(att_qid)) {cat("\n", i, "\n"); dput(attributes_by_qid[[i]]);}
# for (i in unique(att_qid)) {cat("\n", i, "\n"); dput(attNames_by_qid[[i]]);}
#
# hasLang_by_qid <- lapply(attLanguage_by_qid, \(x) !is.na(x));
#
# dput(unique(unlist(lapply(hasLang_by_qid, \(x) names(x)[x]))));




