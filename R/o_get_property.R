#' Retrieve Omeka S property based on id
#'
#' @param property_id The numeric identifier of an Omeka S property.
#' @param base_url Defaults to `NULL`, typically set for the active session with
#'   [o_set()].
#'
#' @returns A list object, with everything returned by the API.
#' @export
#'
#' @examples
#' \dontrun{
#'   o_get_property(property_id = 12)
#' }
o_get_property <- function(property_id, base_url = NULL) {
  resp7 <- o_get(id = property_id, type = "properties", base_url = base_url)

  property7 <- omekas_property(
    data = resp7@data,
    url = resp7@url,
    type = resp7@type,
    retrieved_at = resp7@retrieved_at,
    id = as.integer(property_id)
  )

  property7
}
