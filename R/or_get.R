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

  if (is.null(base_url)) {
    base_url <- or_set()[["base_url"]]
  }

  rlang::try_fetch(
    expr = httr2::url_parse(base_url),
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "The provided {.arg base_url} is not a valid url.",
          "i" = "Provided {.arg base_url}: {.val {base_url}}",
          "!" = "Ensure a valid {.arg base_url} has been provided, or set with {.fun or_set}."
        )
      )
    }
  )

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
