load("data/sample_data.rda")


test_that("ifelse typeviz map  works", {
  df=selecting_viz_data(data,type_viz = "map",group_by_viz = "country", variable_viz = "tender_value_amount")

  expect_equal(selecting_viz_typeGraph(df,"map"), "GnmNum")
})

test_that("ifelse typeviz line 1 works", {
  df3 <- selecting_viz_data(data,type_viz = "line",group_by_viz = "tender_year", variable_viz = "tender_value_amount")
  # hgchmagic::hgch_line_YeaNum(df3)
  expect_equal(selecting_viz_typeGraph(df3,"line"), "YeaNum")
})


test_that("ifelse typeviz line 2 works", {
  df3 <- selecting_viz_data(data,type_viz = "line",group_by_viz = "tender_year", variable_viz = "tender_value_amount",desagregation_viz = "ATC.product_name")
  # hgchmagic::hgch_line_CatYeaNum(df3)
  expect_equal(selecting_viz_typeGraph(df3,"line"), "CatYeaNum")
})


test_that("ifelse typeviz line 2cworks", {
  df3 <- selecting_viz_data(data,type_viz = "line",group_by_viz = "tender_year", variable_viz = "tender_value_amount",desagregation_viz = "country")
  # hgchmagic::hgch_line_CatYeaNum(df3)
  expect_equal(selecting_viz_typeGraph(df3,"line"), "CatYeaNum")
})


test_that("ifelse typeviz bar works", {
  df3 <- selecting_viz_data(data,type_viz = "bar",group_by_viz = "ATC.product_name", variable_viz = "tender_value_amount")
  # hgchmagic::hgch_bar_YeaNum(df3)
  expect_equal(selecting_viz_typeGraph(df3,"bar"), "YeaNum")
})


test_that("ifelse typeviz bar works", {
  df3 <- selecting_viz_data(data,type_viz = "treemap",group_by_viz = "country", variable_viz = "tender_value_amount")
  # hgchmagic::hgch_treemap_CatNum(df3)
  expect_equal(selecting_viz_typeGraph(df3,"treemap"), "YeaNum")

})
