#' S7 class representing an Omeka S API response
#'
#' @slot data A list; the parsed json from the API response.
#' @slot url Full URL from where the response was retrieved.
#' @slot type The type of contents or resource retrieved from the API, e.g.
#'   "items", or "properties".
#' @slot retrieved_at Time when the given resrouce was retrieved.
omekas_response <- S7::new_class(
  name = "omekas_response",
  properties = list(
    data = S7::class_list,
    url = S7::class_character,
    type = S7::class_character,
    retrieved_at = S7::class_POSIXct
  ),
  validator = function(self) {
    if (length(self@type) != 1) {
      "Only one type - e.g. items, or properties - can be provided."
    }
  }
)

#' S7 class representing an Omeka S item
#'
#' @slot id Numeric, the identifier of the resource requested.
omekas_item <- S7::new_class(
  name = "omekas_item",
  parent = omekas_response,
  properties = list(
    id = S7::class_integer
  ),
  validator = function(self) {
    if (!"o:id" %in% names(self@data)) {
      "An item must always have an id"
    } else if (self@type != "items") {
      "Items must be retrieved from the items endpoint"
    }
  }
)


#' S7 class representing an Omeka S property
#'
#' @slot id Numeric, the identifier of the resource requested.
omekas_property <- S7::new_class(
  name = "omekas_property",
  parent = omekas_response,
  properties = list(
    id = S7::class_integer
  ),
  validator = function(self) {
    if (!"o:id" %in% names(self@data)) {
      "A property must always have an id"
    } else if (self@type != "properties") {
      "Properties must be retrieved from the properties endpoint"
    }
  }
)
