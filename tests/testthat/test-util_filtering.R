


test_that("filtering1 works", {
  expect_equal(nrow(filtering_list(iris,list(Species="setosa"))), nrow(iris |> dplyr::filter(Species=="setosa")))
})


test_that("filtering2  works", {
  expect_equal(nrow(filtering_list(iris,list(Species=c("setosa","versicolor")))), nrow(iris |> dplyr::filter(Species=="setosa" | Species=="versicolor")))
})
