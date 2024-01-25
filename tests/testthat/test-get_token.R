test_that("Token is not returned when login does not exist", {
  expect_error(get_token(email = 'fisherman@sea.com', password = 'ilovefish'))
})
