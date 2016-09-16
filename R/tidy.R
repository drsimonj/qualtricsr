#' Tidy columns generated in loop and merge blocks.
#'
#' Convenience function that takes a data frame and applies
#' \code{\link[tidyr]{gather}} to columns ending in `(number)`, which is how
#' loop and merge data is outputted. By default, it returns key-value results as
#' .loop and .dat
#'
#' @export
#' @inheritParams tidyr::gather_
#' @inheritParams base::grep
q_tidyloops <- function(data, key_col = ".loop",
                        value_col = ".dat", pattern = "\\([0-9]+\\)$") {

  gather_cols <- names(data)[grep(pattern, names(data))]

  if(!length(gather_cols))
    stop("No columns matched the search pattern")

  tidyr::gather_(data, key_col = key_col, value_col = value_col,
                 gather_cols = gather_cols)
}
