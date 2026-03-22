#' Sets settings for the current session.
#'
#' @param base_url Endpoint of the API of the Omeka installation, or base url of
#'   the website. Internally, it is ensured that the `api` suffix is always
#'   appended to the given url.
#' @param error Defaults to `TRUE`. Passed to [o_check_url()]. If `TRUE`, throws
#'   an error if the url set or retrieved is invalid.
#'
#' @returns Invisibly, the settings as a list.
#' @export
#'
#' @examples
#'
#' o_set(base_url = "https://example.com/api")
#'
#' base_url <- o_set()[["base_url"]]
#'
#' base_url
o_set <- function(base_url = NULL, error = TRUE) {
  if (is.null(base_url)) {
    base_url <- Sys.getenv("omekasr_base_url")
  } else {
    Sys.setenv(omekasr_base_url = as.character(base_url))
  }

  if (base_url == "") {
    cli::cli_abort(
      message = c(
        "x" = "The provided {.arg base_url} is empty.",
        "!" = "Ensure a valid {.arg base_url} has been provided, or set with {.fun o_set}."
      )
    )
  }

  o_check_url(base_url = base_url, silent = TRUE, error = error)

  if (!stringr::str_ends(string = base_url, pattern = "/api|/api/")) {
    base_url <- fs::path(base_url, "api")
  } else if (stringr::str_ends(string = base_url, pattern = "/api/")) {
    base_url <- stringr::str_remove(string = base_url, pattern = "/$")
  }

  invisible(list(
    base_url = as.character(base_url)
  ))
}
