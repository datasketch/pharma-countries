
test_that("test-counting_r.", {
  expect_equal(counting_r(iris, "Species"), iris %>% group_by(Species) %>% summarize(count=n()))
})


