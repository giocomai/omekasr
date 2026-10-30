
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omekasr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-universe
version](https://giocomai.r-universe.dev/omekasr/badges/version)](https://giocomai.r-universe.dev/omekasr)
<!-- badges: end -->

The goal of `omekasr` is to facilitate interaction with the Omeka S REST
API.

It is currently only minimally functional. Features are documented in
this document as they are introduced and tested.

Planned features include:

- producing pdf reports of Omeka S items

## Installation

You can install the development version of `omekasr` from its main
repository on Codeberg:

``` r
remotes::install_git("https://codeberg.org/giocomai/omekasr")
```

or from r-universe with:

``` r
install.packages("omekasr", repos = c("https://giocomai.r-universe.dev", "https://cloud.r-project.org"))
```

## Introducing `omekasr`

The first thing you do when working with `omekasr` is to set the url of
the Omeka S instance you want to work with. Throughout the
documentation, a dedicated testing instance will be used for reference;
its contents are derived from the [Omeka S official sandbox
instance](https://dev.omeka.org/omeka-s-sandbox/).

Using `o_set()` will set the base url for the current session.

``` r
library("omekasr")
o_set(base_url = "https://omekasr.giorgiocomai.eu/")
```

Internally, this will check that the given input appears to be a valid
URL, and will append `api` as requested by Omeka S. This test can be
conducted interactively with `o_check_url()`.

``` r
o_check_url()
#> ✔ The provided `base_url` seems to be a valid url.
#> ℹ Provided `base_url`: "https:/omekasr.giorgiocomai.eu/api"
```

This only checks that the URL appears to be valid, but does not ensure
that an active Omeka S server is responeding at that address.

## Retrieve items and properties

It is possible to retrieve an data from any supported endpoint with
`o_get()`; at present, only `items` and `properties` are supported

``` r
get_response <- o_get(id = 12, type = "items")
```

Main types have also convenience functions that return an `S7` object
that actively identifies the response as an Omeka S item.

``` r
item <- o_get_item(item_id = 12)

item
#> <omekasr::omekas_item>
#>  @ data        :List of 26
#>  .. $ @context              : chr "https://omekasr.giorgiocomai.eu/api-context"
#>  .. $ @id                   : chr "https://omekasr.giorgiocomai.eu/api/items/12"
#>  .. $ @type                 :List of 2
#>  ..  ..$ : chr "o:Item"
#>  ..  ..$ : chr "dctype:Text"
#>  .. $ o:id                  : int 12
#>  .. $ o:is_public           : logi TRUE
#>  .. $ o:owner               :List of 2
#>  ..  ..$ @id : chr "https://omekasr.giorgiocomai.eu/api/users/1"
#>  ..  ..$ o:id: int 1
#>  .. $ o:resource_class      :List of 2
#>  ..  ..$ @id : chr "https://omekasr.giorgiocomai.eu/api/resource_classes/31"
#>  ..  ..$ o:id: int 31
#>  .. $ o:resource_template   :List of 2
#>  ..  ..$ @id : chr "https://omekasr.giorgiocomai.eu/api/resource_templates/2"
#>  ..  ..$ o:id: int 2
#>  .. $ o:thumbnail           : NULL
#>  .. $ o:title               : chr "The Valley of Fear"
#>  .. $ thumbnail_display_urls:List of 3
#>  ..  ..$ large : chr "https://omekasr.giorgiocomai.eu/files/large/848cf8608c756b5f98952e2c3ad2334fbffda7d0.jpg"
#>  ..  ..$ medium: chr "https://omekasr.giorgiocomai.eu/files/medium/848cf8608c756b5f98952e2c3ad2334fbffda7d0.jpg"
#>  ..  ..$ square: chr "https://omekasr.giorgiocomai.eu/files/square/848cf8608c756b5f98952e2c3ad2334fbffda7d0.jpg"
#>  .. $ o:created             :List of 2
#>  ..  ..$ @value: chr "2026-03-15T15:02:32+00:00"
#>  ..  ..$ @type : chr "http://www.w3.org/2001/XMLSchema#dateTime"
#>  .. $ o:modified            :List of 2
#>  ..  ..$ @value: chr "2026-03-15T16:41:07+00:00"
#>  ..  ..$ @type : chr "http://www.w3.org/2001/XMLSchema#dateTime"
#>  .. $ o:primary_media       :List of 2
#>  ..  ..$ @id : chr "https://omekasr.giorgiocomai.eu/api/media/99"
#>  ..  ..$ o:id: int 99
#>  .. $ o:media               :List of 1
#>  ..  ..$ :List of 2
#>  ..  .. ..$ @id : chr "https://omekasr.giorgiocomai.eu/api/media/99"
#>  ..  .. ..$ o:id: int 99
#>  .. $ o:item_set            :List of 2
#>  ..  ..$ :List of 2
#>  ..  .. ..$ @id : chr "https://omekasr.giorgiocomai.eu/api/item_sets/86"
#>  ..  .. ..$ o:id: int 86
#>  ..  ..$ :List of 2
#>  ..  .. ..$ @id : chr "https://omekasr.giorgiocomai.eu/api/item_sets/87"
#>  ..  .. ..$ o:id: int 87
#>  .. $ o:site                : list()
#>  .. $ dcterms:title         :List of 1
#>  ..  ..$ :List of 5
#>  ..  .. ..$ type          : chr "literal"
#>  ..  .. ..$ property_id   : int 1
#>  ..  .. ..$ property_label: chr "Title"
#>  ..  .. ..$ is_public     : logi TRUE
#>  ..  .. ..$ @value        : chr "The Valley of Fear"
#>  .. $ dcterms:creator       :List of 1
#>  ..  ..$ :List of 12
#>  ..  .. ..$ type               : chr "resource"
#>  ..  .. ..$ property_id        : int 2
#>  ..  .. ..$ property_label     : chr "Creator"
#>  ..  .. ..$ is_public          : logi TRUE
#>  ..  .. ..$ @id                : chr "https://omekasr.giorgiocomai.eu/api/items/1"
#>  ..  .. ..$ value_resource_id  : int 1
#>  ..  .. ..$ value_resource_name: chr "items"
#>  ..  .. ..$ url                : NULL
#>  ..  .. ..$ display_title      : chr "Sir Arthur Ignatius Conan Doyle"
#>  ..  .. ..$ thumbnail_url      : chr "https://omekasr.giorgiocomai.eu/files/square/883c22bbd9cb2bec1c48e0be35a8c6d41f83a70e.jpg"
#>  ..  .. ..$ thumbnail_title    : chr "Arthur Conany Doyle by Walter Benington, 1914"
#>  ..  .. ..$ thumbnail_type     : chr "image/png"
#>  .. $ dcterms:date          :List of 1
#>  ..  ..$ :List of 5
#>  ..  .. ..$ type          : chr "literal"
#>  ..  .. ..$ property_id   : int 7
#>  ..  .. ..$ property_label: chr "Date"
#>  ..  .. ..$ is_public     : logi TRUE
#>  ..  .. ..$ @value        : chr "1915"
#>  .. $ dcterms:publisher     :List of 1
#>  ..  ..$ :List of 5
#>  ..  .. ..$ type          : chr "literal"
#>  ..  .. ..$ property_id   : int 5
#>  ..  .. ..$ property_label: chr "Publisher"
#>  ..  .. ..$ is_public     : logi TRUE
#>  ..  .. ..$ @value        : chr "Goerge H. Doran Company"
#>  .. $ dcterms:description   :List of 1
#>  ..  ..$ :List of 5
#>  ..  .. ..$ type          : chr "literal"
#>  ..  .. ..$ property_id   : int 4
#>  ..  .. ..$ property_label: chr "Description"
#>  ..  .. ..$ is_public     : logi TRUE
#>  ..  .. ..$ @value        : chr "Involing the Molly Maguires, an extended episode in the Americas, and generally a bit odd."
#>  .. $ dcterms:type          :List of 1
#>  ..  ..$ :List of 5
#>  ..  .. ..$ type          : chr "literal"
#>  ..  .. ..$ property_id   : int 8
#>  ..  .. ..$ property_label: chr "Type"
#>  ..  .. ..$ is_public     : logi TRUE
#>  ..  .. ..$ @value        : chr "novel"
#>  .. $ dcterms:identifier    :List of 1
#>  ..  ..$ :List of 5
#>  ..  .. ..$ type          : chr "literal"
#>  ..  .. ..$ property_id   : int 10
#>  ..  .. ..$ property_label: chr "Identifier"
#>  ..  .. ..$ is_public     : logi TRUE
#>  ..  .. ..$ @value        : chr "VALL"
#>  .. $ osii:source_resource  :List of 1
#>  ..  ..$ :List of 5
#>  ..  .. ..$ type          : chr "uri"
#>  ..  .. ..$ property_id   : int 185
#>  ..  .. ..$ property_label: chr "source resource"
#>  ..  .. ..$ is_public     : logi TRUE
#>  ..  .. ..$ @id           : chr "https://dev.omeka.org/omeka-s-sandbox/api/items/1793"
#>  .. $ osii:source_site      :List of 1
#>  ..  ..$ :List of 5
#>  ..  .. ..$ type          : chr "uri"
#>  ..  .. ..$ property_id   : int 186
#>  ..  .. ..$ property_label: chr "source site"
#>  ..  .. ..$ is_public     : logi TRUE
#>  ..  .. ..$ @id           : chr "https://dev.omeka.org/omeka-s-sandbox/"
#>  @ url         : chr "https://omekasr.giorgiocomai.eu/api/items/12"
#>  @ type        : chr "items"
#>  @ retrieved_at: POSIXct[1:1], format: "2026-10-30 23:22:52"
#>  @ id          : int 12
```

``` r
class(item)
#> [1] "omekasr::omekas_item"     "omekasr::omekas_response"
#> [3] "S7_object"
```

## Troubleshooting

This package features a situation report function, that can be used to
troubleshoot most common issues, such as incorrect API base url or lack
of internet connection

``` r
o_sitrep()
#> 
#> ── omekasr status report ───────────────────────────────────────────────────────
#> 
#> ── Check if API url endpoint can be parsed ──
#> 
#> ✔ The provided `base_url` seems to be a valid url.
#> ℹ Provided `base_url`: "https:/omekasr.giorgiocomai.eu/api"
#> 
#> 
#> ── Check if internet connection is available ──
#> 
#> 
#> 
#> Internet connection status: Online
```
