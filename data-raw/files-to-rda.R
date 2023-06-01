# All the files present in data-raw need to be
# transferred to the data folder as rda

# Here is written the script to do so

# The best would be:
# - source the original file
# - modify it here according to the needs
# - export it to the data folder as rda

#### Global libraries ####
library(dplyr)
library(usethis)

#### hg19_chr_list ####
library(chromhmmData)
chromsizes <- system.file("extdata/CHROMSIZES", package = "chromhmmData")
hg19_path <- system.file("extdata/CHROMSIZES", "hg19.txt", package = "chromhmmData")
hg19_chr_list <- read.table(hg19_path, col.names = c("chr", "end")) %>%
  mutate(start = 1, .before=end) %>%
  filter(chr %in% paste0("chr", seq(1, 21)))
usethis::use_data(hg19_chr_list)
