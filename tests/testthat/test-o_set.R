library("omekasr")
library("testthat")

test_that("API is appended correctly to the base url", {
  expect_equal(
    o_set("https:/example.com/")[["base_url"]],
    "https:/example.com/api"
  )

  expect_equal(
    o_set("https:/example.com/api")[["base_url"]],
    "https:/example.com/api"
  )

  expect_equal(
    o_set("https:/example.com/api/")[["base_url"]],
    "https:/example.com/api"
  )

  expect_equal(
    o_set("https:/example.com")[["base_url"]],
    "https:/example.com/api"
  )
})


test_that("An error is thrown is an invalid url is given", {
  expect_error({
    settings <- o_set(base_url = "")
  })

  expect_no_error({
    settings <- o_set(base_url = "https:/example.com")
  })
})
