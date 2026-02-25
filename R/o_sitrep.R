#' Situation report
#'
#' Performs a set of tests for troubleshooting common issues, and provides
#' informative message.
#'
#' @inheritParams o_set
#'
#' @returns Nothing, used for its side effects.
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
  cli::cli_inform(c(
    "Internet connection status: {if(internet_available) cli::col_green('Online') else cli::col_red('Offline')}"
  ))
  if (!internet_available) {
    cli::cli_alert_warning("Internet connection not available")
  }
}
