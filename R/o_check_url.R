#' Checks if the base url given or set for the session can be parsed
#'
#' @inheritParams o_set
#' @param silent Logical, defaults to `FALSE`. If `TRUE`, the function does not
#'   print informative messages related to this check.
#' @param error Logical, defaults to `TRUE`. If `TRUE`, the function throws an
#'   error if this check fails.
#'
#' @returns A list with three elements: `valid` (either `TRUE` or `FALSE`),
#'   `value` (returning `base_url`), and `message` (returning an informative
#'   message about the result of the check, typically shown with
#'   [cli::cli_inform()]).
#' @export
#'
#' @examples
#'
#' o_check_url("wrong_url/api")
#'
#' o_check_url("http://127.0.0.0")
o_check_url <- function(base_url = NULL, silent = FALSE, error = TRUE) {
  if (is.null(base_url)) {
    base_url <- o_set()[["base_url"]]
  }

  parsed <- rlang::try_fetch(
    expr = httr2::url_parse(base_url),
    error = function(e) {
      return(e)
    }
  )

  if (inherits(parsed, "error")) {
    message <- c(
      "x" = "The provided {.arg base_url} is not a valid url.",
      "i" = "Provided {.arg base_url}: {.val {base_url}}",
      "!" = "Ensure a valid {.arg base_url} has been provided, or set with {.fun o_set}."
    )
  } else {
    message <- c(
      v = "The provided {.arg base_url} seems to be a valid url.",
      i = "Provided {.arg base_url}: {.val {base_url}}"
    )
  }

  check_result <- list(
    valid = !(inherits(parsed, "error")),
    value = base_url,
    message = message
  )

  if (!silent) {
    if (error & !(inherits(parsed, "error"))) {
      cli::cli_abort(check_result[["message"]])
    } else {
      cli::cli_inform(check_result[["message"]])
    }
  }

  invisible(check_result)
}
