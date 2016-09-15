#' Read header rows of a qualtrics file into a tibble of variable names and
#' labels
#'
#' Reads in the first two lines of a Qualtrics Survey file. The first line
#' contains the variable names and the second contains the complete labels, such
#' as question text. The minor exception are the default Qualtrics variables,
#' which are named V1, V2, and so on. The variable name for these are stored in
#' the second row. Thus, for these, the label is substituted as the variable
#' name. Any empty variables are also dropped (as files end with comma, leaving
#' empty cell at end).
#'
#' @inheritParams readr::read_lines
#' @export
#'
#' @return tibble with variable (`var`) and corresponding label columns.
q_readh <- function(file) {
  # Read the first two lines of the file
  headers <- readr::read_lines(file, n_max = 2) %>%
               stringr::str_split(",")

  # Replace that Qualtrics variables named V1, V2, etc...
  to_replace <- stringr::str_detect(headers[[1]], "^V[0-9]+$")
  headers[[1]][to_replace] <- headers[[2]][to_replace]

  names(headers) <- c("var", "label")

  tibble::as_tibble(headers) %>% dplyr::filter(var != "")
}
