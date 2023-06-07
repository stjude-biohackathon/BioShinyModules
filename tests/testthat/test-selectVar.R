test_that("find_vars works", {
  f_path <- file.path(system.file("data-raw", package = "BioShinyModules"),
    "MS_2.rda")
  df <- read_data(f_path, sep = ",", df_name = "df")
  expect_equal(dim(df), c(164, 32))
  col_chr <- find_vars(df, filter = is.numeric)
  expect_equal(length(col_chr), 32)
  col_chr <- find_vars(df, filter = is.character)
  expect_equal(length(col_chr), 0)
  df[, 1] <- cut(df[, 1], 3)
  col_chr <- find_vars(df, filter = is.factor)
  expect_equal(length(col_chr), 1)
})
