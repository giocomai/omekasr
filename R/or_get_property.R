#' Retrieve Omeka S property based on id
#'
#' @param property_id The numeric identifier of an Omeka S property.
#' @param base_url Defaults to `NULL`, typically set for the active session with
#'   [or_set()].
#'
#' @returns A list object, with everything returned by the API.
#' @export
#'
#' @examples
#' \dontrun{
#'   or_get_property(id = 12)
#' }
or_get_property <- function(property_id, base_url = NULL) {
  if (is.null(base_url)) {
    base_url <- or_set()[["base_url"]]
  }

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append("properties", as.character(property_id))

  resp <- req |>
    httr2::req_perform()

  property_l <- httr2::resp_body_json(resp)

  property_l
}
