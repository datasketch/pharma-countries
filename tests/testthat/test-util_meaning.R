
test_that("meaning1 works", {
  expect_equal(meaning_r(iris,"Species","Petal.Width"), iris |> group_by(Species)  |> summarize(mean=mean(Petal.Width)))
})
