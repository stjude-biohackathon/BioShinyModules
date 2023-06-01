#' hg19_chr_list
#'
#' @description List of all dogs chromosome with start and end.
#'
#' @details All 21 humans' chromosomes are here listed.
#' The start and end in base pair are also present for each of them.
#' Data come from the chromhmmData library, a start field is added ( = 1) and
#' only the 21 first chromosomes are kept.
#'
#' @format A data frame with 3 variables:
#' \describe{
#' \item{\code{chr}}{Name of the chromosome}
#' \item{\code{start}}{Base pair start number}
#' \item{\code{end}}{Base pair end number}
#' }
#'
#' @usage data(hg19_chr_list)
#'
#' For further details, see
#' \url{https://bioconductor.org/packages/chromhmmData/}
#'
"hg19_chr_list"
