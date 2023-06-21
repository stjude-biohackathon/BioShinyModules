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
library(readxl)
library(httr)
library(janitor)

#### hg19_chr_list ####
library(chromhmmData)
chromsizes <- system.file("extdata/CHROMSIZES", package = "chromhmmData")
hg19_path <- system.file("extdata/CHROMSIZES", "hg19.txt", package = "chromhmmData")
hg19_chr_list <- read.table(hg19_path, col.names = c("chr", "end")) %>%
  mutate(start = 1, .before=end) %>%
  filter(chr %in% paste0("chr", seq(1, 21)))
usethis::use_data(hg19_chr_list)

#### IC_BPS.rda ####
# IC_BPS.rda includes ic_bps, ic_bps_sig, ic_bps_stat, ic_bps_stat_sig, feature_meta, sample_meta
# in the forms of a list
url1 <- "https://static-content.springer.com/esm/art%3A10.1038%2Fs41598-022-12197-2/MediaObjects/41598_2022_12197_MOESM1_ESM.xlsx"

httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
fh <- readxl::read_excel(tf, sheet = 3L, col_names = FALSE)
feature_meta <- fh[-1,1:8] %>%
        janitor::row_to_names(row_number = 1) %>%
        janitor::clean_names() %>%
        as.data.frame()
sample_meta <- fh[c(1,2),-c(1:8)] %>%
        t() %>%
        as.data.frame() %>%
        remove_rownames() %>%
        column_to_rownames(., "V2") %>%
        `colnames<-`("Label")
ic_bps <- fh[-1, c(1, 9:ncol(fh))] %>%
        janitor::row_to_names(row_number = 1) %>%
        #rename("peptide_sequence" = "Peptide sequence")
        column_to_rownames(., "Peptide sequence") %>%
        mutate(across(everything(), as.numeric))
ic_bps_stat <- readxl::read_excel(tf, sheet = 4L, col_names = TRUE) %>%
        rename("peptide_sequence" = "Peptide sequence") %>%
        as.data.frame()
ic_bps_stat_sig <- readxl::read_excel(tf, sheet = 5L, col_names = TRUE) %>%
        rename("peptide_sequence" = "Peptide sequence") %>%
        rename("accession" = "Accession") %>%
        as.data.frame()
ic_bps_sig <- ic_bps %>%
        rownames_to_column(., "peptide_sequence") %>%
        dplyr::filter(peptide_sequence %in% ic_bps_stat_sig$peptide_sequence) %>%
        column_to_rownames(., "peptide_sequence")

IC_BPS = list(ic_bps, ic_bps_sig, ic_bps_stat, ic_bps_stat_sig, feature_meta, sample_meta)
names(IC_BPS) = c("ic_bps", "ic_bps_sig", "ic_bps_stat", "ic_bps_stat_sig",
                  "feature_meta", "sample_meta")
usethis::use_data(IC_BPS)

