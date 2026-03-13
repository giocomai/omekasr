#' S7 class representing an Omeka S API response
#'
#' @param data A list; the parsed json from the API response.
#' @param url Full URL from where the response was retrieved.
#' @param type The type of contents or resource retrieved from the API, e.g.
#'   "items", or "properties".
#' @param retrieved_at Time when the given resource was retrieved.
omekas_response <- S7::new_class(
  name = "omekas_response",
  properties = list(
    data = S7::class_list,
    url = S7::class_character,
    type = S7::class_character,
    retrieved_at = S7::new_property(
      class = S7::class_POSIXct,
      default = quote(Sys.time())
    )
  ),
  validator = function(self) {
    if (length(self@type) != 1) {
      "Only one type - e.g. items, or properties - can be provided."
    }
  }
)

#' S7 class representing an Omeka S item
#'
#' @param id Numeric, the identifier of the resource requested.
#' @inheritParams omekas_response

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
#' @param id Numeric, the identifier of the resource requested.
#' @inheritParams omekas_response
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
