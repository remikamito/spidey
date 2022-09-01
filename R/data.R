#' Tract FDC data from fixel-based analysis of DWI data
#'
#' Data from a cohort of healthy control participants and
#' patients with epilepsy. The data included are mean fibre density
#' and cross-section (FDC) values from selected tracts of interest,
#' and were computed using the fixel-based analysis pipeline.
#'
#' @docType data
#'
#' @usage data(tractdata)
#'
#' @format A data frame with 100 rows and 36 variables:
#' \describe{
#'   \item{Group}{Group of participant, either patient (1) or control (0)}
#'   \item{Scanner_ID}{identifier of scanner or site, for use in ComBat}
#'   ...
#' }
"tractdata"
