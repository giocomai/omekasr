#' Retrieve results from Omeka S API
#'
#' @param id An Omeka S numeric identifier.
#' @param type Defaults to "items". Accepted values include "item" and
#'   "properties".
#' @inheritParams or_set
#'
#' @returns A list object, with attributes detailing the source url, the type of
#'   API endpoint called, and the timestamp of data retrieval.
#' @export
#'
#' @examples
#' \dontrun{
#'   or_get(id = 123, type = "items")
#'   or_get(id = "123", type = "items") # id can be given as character
#'   or_get(id = 12, type = "properties")
#' }
or_get <- function(id, type = c("items", "properties"), base_url = NULL) {
  type <- rlang::arg_match(arg = type, values = c("items", "properties"))

  check_url_result <- or_check_url(base_url = base_url)

  if (!check_url_result[["valid"]]) {
    cli::cli_abort(message = check_url_result[["message"]])
  }

  base_url <- check_url_result[["value"]]

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(type, as.character(id))

  resp <- req |>
    httr2::req_perform()

  resp_l <- httr2::resp_body_json(resp)

  response_s7 <- omekas_response(
    data = resp_l,
    url = as.character(req[["url"]]),
    type = as.character(type),
    retrieved_at = Sys.time()
  )

  response_s7
}
