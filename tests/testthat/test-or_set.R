library("testthat")
test_that("API is appended correctly to the base url", {
  expect_equal(
    or_set("https:/example.com/")[["base_url"]],
    "https:/example.com/api"
  )

  expect_equal(
    or_set("https:/example.com/api")[["base_url"]],
    "https:/example.com/api"
  )

  expect_equal(
    or_set("https:/example.com/api/")[["base_url"]],
    "https:/example.com/api"
  )

  expect_equal(
    or_set("https:/example.com")[["base_url"]],
    "https:/example.com/api"
  )
})
