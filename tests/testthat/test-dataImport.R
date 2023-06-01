test_that("read_data works", {
  f_path <- file.path(system.file("data-raw", package = "BioShinyModules"),
    "Limma.csv")
  df <- read_data(f_path, sep = ",")
  expect_equal(dim(df), c(15095, 7))

  f_path <- file.path(system.file("data-raw", package = "BioShinyModules"),
    "MS_2.rda")
  df <- read_data(f_path, sep = ",", df_name = "df")
  expect_equal(dim(df), c(164, 32))
})

test_that("get_dataframe works", {
  f_path <- file.path(system.file("data-raw", package = "BioShinyModules"),
    "Limma.csv")
  df_name <- get_dataframe(f_path)
  expect_equal(df_name, NULL)

  f_path <- file.path(system.file("data-raw", package = "BioShinyModules"),
    "MS_2.rda")
  df_name <- get_dataframe(f_path)
  expect_equal(df_name, c("df", "feature_meta", "sample_meta"))
})
