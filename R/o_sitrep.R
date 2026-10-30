#' Situation report
#'
#' Performs a set of tests for troubleshooting common issues, and provides
#' informative message.
#'
#' More specifically, `o_sitrep()` checks:
#'
#' \itemize{
#'   \item that the `base_url` can be parsed and appears to be a valid
#'   \item that there is a functioning internet connection
#' }
#'
#' @inheritParams o_set
#'
#' @returns Mostly used for its side effects, return list with check results.
#' @export
#'
#' @examples
#'
#' o_sitrep()
o_sitrep <- function(base_url = NULL) {
  cli::cli_h1("{.pkg omekasr} status report")

  cli::cli_h2("Check if API url endpoint can be parsed")
  check_url_result <- o_check_url(base_url = base_url)

  cli::cli_h2("Check if internet connection is available")
  internet_available <- curl::has_internet()

  check_url_result <- list(
    value = internet_available,
    message = c(
      "Internet connection status: {if(internet_available) cli::col_green('Online') else cli::col_red('Offline')}"
    )
  )
  cli::cli_inform(message = check_url_result[["message"]])
  if (!internet_available) {
    cli::cli_alert_warning("Internet connection not available")
  }

  invisible(list(
    check_url = check_url_result,
    check_internet = check_url_result
  ))
}
