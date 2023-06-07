test_that("find_vars works", {
  f_path <- file.path(system.file("data", package = "BioShinyModules"),
    "hg19_chr_list.rda")
  df <- read_data(f_path, df_name = "hg19_chr_list")
  expect_equal(dim(df), c(21, 3))
  col_chr <- find_vars(df, filter = is.numeric)
  expect_equal(col_chr, c("start", "end"))
  col_chr <- find_vars(df, filter = is.character)
  expect_equal(length(col_chr), 0)
  col_chr <- find_vars(df, filter = is.factor)
  expect_equal(col_chr, "chr")
})
