ampersand_filter <- function() {
  std_input <- file("stdin")
  ast <- readLines(std_input, warn = FALSE)
  close.connection(std_input)

  write(ast, "~/ast_test.txt")
  write(Encoding(ast), "~/ast_encoding.txt")

  intext_regex <- "\"citationMode\":\\{\"t\":\"AuthorInText\".*?\\}\\]\\]\\}"

  intext_citations <- unlist(stringr::str_extract_all(ast, intext_regex))
  corrected_citations <- stringr::str_replace(intext_citations, "&", "and")

  intext_locations <- stringr::str_locate_all(ast, intext_regex)[[1]]

  for(i in rev(seq_along(corrected_citations))) {
    stringr::str_sub(ast, intext_locations[i, "start"], intext_locations[i, "end"]) <- corrected_citations[i]
  }

  write(ast, "~/ast_test2.txt")
  write(Encoding(ast), "~/ast_encoding.txt", append = TRUE)

  ast <- iconv(ast, to = "UTF-8")
  write(ast, "~/ast_test3.txt")
  write(Encoding(ast), "~/ast_encoding.txt", append = TRUE)

  write(ast, stdout())
  closeAllConnections()
}
