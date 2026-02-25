#' Retrieve Omeka S item based on id
#'
#' @param item_id The numeric identifier of an Omeka S item.
#' @param base_url Defaults to `NULL`, typically set for the active session with
#'   [o_set()].
#'
#' @returns A list object, with everything returned by the API.
#' @export
#'
#' @examples
#' \dontrun{
#'   o_get_item(id = 123)
#' }
o_get_item <- function(item_id, base_url = NULL) {
  resp7 <- o_get(id = item_id, type = "items", base_url = base_url)

  item7 <- omekas_item(
    data = resp7@data,
    url = resp7@url,
    type = resp7@type,
    retrieved_at = resp7@retrieved_at,
    id = as.integer(item_id)
  )

  item7
}
