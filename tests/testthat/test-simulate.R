test_that('simulate_lang_data works', {
  sim <- simulate_lang_data(n = 50)
  expect_equal(nrow(sim$participants), 50)
})
