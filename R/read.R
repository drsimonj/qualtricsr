#' Read variable information from a qualtrics file into a tibble of variable
#' names and labels
#'
#' Reads in the first two lines of a Qualtrics Survey file. The first line
#' contains the variable names and the second contains the complete labels, such
#' as question text. The minor exception are the default Qualtrics variables,
#' which are named V1, V2, and so on. The variable name for these are stored in
#' the second row. Thus, for these, the label is substituted as the variable
#' name.
#'
#' @inheritParams readr::read_lines
#' @drop_empty Boolean. Should empty variables be dropped?
#' @export
#'
#' @return tibble with variable (`var`) and corresponding label columns.
q_readv <- function(file, drop_empty = TRUE, comma_warn = TRUE) {
  # Read the first two lines of the file
  .v <- readr::read_lines(file, n_max = 2) %>%
               stringr::str_split(",")

  # Replace that Qualtrics variables named V1, V2, etc...
  to_replace <- which(stringr::str_detect(.v[[1]], "^V[0-9]+$"))
  .v[[1]][to_replace] <- .v[[2]][to_replace]

  names(.v) <- c("var", "label")

  if (length(.v$var) == length(.v$label)) {
    .v <- tibble::as_tibble(.v)

    if (drop_empty)
      .v <- .v %>% dplyr::filter(var != "")
  } else if (comma_warn) {
    warning("There were commas in the second line meaning that question numbers and text will not align.")
  }

  .v
}

#' Read qualtrics file as a tibble
#'
#' Reads in a qualtrics file and handles formatting of header rows. Variable
#' names contained in the first two rows are read via \code{\link{q_readv}}. The
#' remainder of the data set is then read in, using the formatted variable names
#' as column names.
#'
#' @export
#' @inheritParams readr::read_csv
#'
#' @return tibble. Column names derived using \code{\link{q_readv}}
q_read <- function(file) {
  # Read vars
  .v <- q_readv(file, drop_empty = F, comma_warn = F)

  # Identify any empty columns
  is_empty <- .v$var == ""
  .v[is_empty, ] <- paste0("___", seq(sum(is_empty)))

  # Import data
  .d <- readr::read_csv(file, skip = 2, col_names = .v$var)

  # Drop any empty columns
  .d <- .d[, !is_empty]

  # Return with `q_df` class
  class(.d) <- c("q_tbl", class(.d))
  .d
}
