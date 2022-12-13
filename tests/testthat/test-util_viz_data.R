#probar oromedios y desagregado

load("data/data.rda")


test_that("ifelse typeviz map  works", {
  expect_equal(selecting_viz_data(data,type_viz = "map",group_by_viz = "country", variable_viz = "tender_value_amount"), meaning_r(data,"country","tender_value_amount"))
})

test_that("ifelse typeviz line 1 works", {
  expect_equal(selecting_viz_data(data,type_viz = "line",group_by_viz = "tender_year", variable_viz = "tender_value_amount",desagregation_viz = "ATC.product_name"), meaning_r(data,"tender_year","tender_value_amount","ATC.product_name"))
})


test_that("ifelse typeviz line 2cworks", {
  expect_equal(selecting_viz_data(data,type_viz = "line",group_by_viz = "tender_year", variable_viz = "tender_value_amount",desagregation_viz = "country"), meaning_r(data,"tender_year","tender_value_amount","country"))
})

test_that("ifelse typeviz bar works", {
  expect_equal(selecting_viz_data(data,type_viz = "bar",group_by_viz = "country", variable_viz = "tender_value_amount"), meaning_r(data,"country","tender_value_amount"))
})


test_that("ifelse typeviz bar works", {
  expect_equal(selecting_viz_data(data,type_viz = "treemap",group_by_viz = "country", variable_viz = "tender_value_amount"), meaning_r(data,"country","tender_value_amount"))
})
