#' Retrieve Omeka S item based on id
#'
#' @param item_id The numeric identifier of an Omeka S item.
#' @param base_url Defaults to NULL, typically set for the active session with [or_set()].
#'
#' @returns A list object, with everything returned by the API.
#' @export
#'
#' @examples
#' \dontrun{
#'   or_get_item(id = 123)
#' }
or_get_item <- function(item_id, base_url = NULL) {
  if (is.null(base_url)) {
    base_url <- or_set()[["base_url"]]
  }

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append("items", as.character(item_id))

  resp <- req |>
    httr2::req_perform()

  item_l <- httr2::resp_body_json(resp)

  item_l
}
