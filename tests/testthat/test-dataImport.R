test_that("read_data works", {
  df <- read_data("/data/Limma.csv", sep=",")
  expect_equal(dim(df),c(4,500))
})
