test_that("read_data works", {
  f_path <- file.path(system.file("data", package = "BioShinyModules"),
    "hg19_chr_list.rda")
  df <- read_data(f_path, df_name = "hg19_chr_list")
  expect_equal(dim(df), c(21, 3))
})

test_that("get_dataframe works", {
  f_path <- file.path(system.file("data", package = "BioShinyModules"),
    "hg19_chr_list.rda")
  df_name <- get_dataframe(f_path)
  expect_equal(df_name, "hg19_chr_list")
})
