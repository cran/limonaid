xmlNode_from_vector <- function(nodeName, subNames, vector, cdata=FALSE) {

  res <- xml2::xml_new_root(nodeName);

  if (length(subNames) == 1) {
    subNames <- rep(subNames, length(vector));
  }

  for (i in seq_along(vector)) {
    if (is.null(vector[i]) || is.na(vector[i])) {
      xml2::xml_add_child(res, subNames[i]);
    } else {
      if (cdata) {
        xml2::xml_add_child(res, subNames[i]);
        xml2::xml_add_child(
          xml2::xml_child(res, search = xml2::xml_length(res)),
          xml2::xml_cdata(as.character(vector[i]))
        );
      } else {
        xml2::xml_add_child(res, subNames[i], vector[i]);
      }
    }
  }

  return(res);

  # return(
  #   xml2::as_xml_document(
  #     stats::setNames(
  #       list(
  #         stats::setNames(
  #           lapply(
  #             vector,
  #             list
  #           ),
  #           nm = rep(subNames, length = length(vector))
  #         )
  #       ),
  #       nm = nodeName
  #     )
  #   )
  # );
}
