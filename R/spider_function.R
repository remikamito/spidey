# Function to plot Spideys
# Date: 9 August 2022

#-----------------------------------------------------------------------------------

#' SPIDERZ function
#'
#' This function plots tract-based data as a radar (or spider) chart. Input data
#' should be tract-specific Z-scores for a given patient, when compared to a
#' control cohort. Z-scores can be computed using the RUNTRACTZ function if input
#' data contains tract data for both healthy control participants and the patient.
#' @param input_Zscores A dataframe containing Z-scores for tract data for a patient.
#' @param bilateral I have not done this yet. Defaults to TRUE.
#' @param sigline Include a significance threshold line (Z=-1.96) in plot? Defaults to TRUE
#' @param customtracts I have not done this yet. Defaults to FALSE
#' @param leftcol Colour for left-sided tracts. Defaults to "#3498DB"
#' @param rightcol Colour for left-sided tracts. Defaults to "#E74C3C"
#' @export
#' @examples
#' SPIDERZ()

SPIDERZ <- function(input_Zscores, bilateral=TRUE, sigline=TRUE, customtracts=FALSE,
                    leftcol="#3498DB", rightcol="#E74C3C"){
  require(fmsb)
  require(scales)

  pt_ID <- input_Zscores[,"ID"] # note this is hardcoded, should maybe include an identifier

  # first set up left and right data **note: should probably grep these directly from data
  roi_names <- c("AF","CG", "CST", "FX", "IFOF", "ILF", "SCP", "SLF III", "SLF II", "SLF I", "UF", "CC")

  CCnames <- grep("CC_FDC", colnames(input_Zscores), value=TRUE)
  leftnames <- grep("left",colnames(input_Zscores), value=TRUE)
  rightnames <- grep("right",colnames(input_Zscores), value=TRUE)

  left_data <- input_Zscores[c(leftnames,CCnames)]
  colnames(left_data) <- roi_names
  rownames(left_data) <- c("Left")

  right_data <- input_Zscores[c(rightnames,CCnames)]
  colnames(right_data) <- roi_names
  rownames(right_data) <- c("Right")

  # set up rows of zeros, maxes and sig
  zeros <- data.frame(matrix(0, nrow = 1, ncol = length(roi_names)))
  colnames(zeros) <- roi_names

  maxes <- data.frame(matrix(-4, nrow = 1, ncol = length(roi_names)))
  colnames(maxes) <- roi_names

  sig <- data.frame(matrix(-1.96, nrow = 1, ncol = length(roi_names)))
  colnames(sig) <- roi_names

  # assign tract order
  col_order <- c("CC", "SLF III", "SLF II", "SLF I",
                 "ILF", "IFOF", "UF", "AF",
                 "FX", "CG",
                 "SCP", "CST")

  # bind data and add significance line
  if(sigline==TRUE){
    radardatmm <- rbind(zeros, maxes, sig, left_data, right_data)
    radardatmm <- radardatmm[, col_order]
    hemi_color <- c("black", leftcol, rightcol)

    hemi_lty <- c(2,1,1)
    hemi_lwd <- c(1,4,4)

    colors_border <- hemi_color
    colors_in <- c(alpha(hemi_color[1],0),alpha(hemi_color[2],0.3),alpha(hemi_color[3],0.3))

  } else if(sigline==FALSE){
    radardatmm <- rbind(zeros, maxes, left_data, right_data)
    radardatmm <- radardatmm[, col_order]
    hemi_color <- c(leftcol, rightcol)

    hemi_lty <- c(1,1)
    hemi_lwd <- c(4,4)

    colors_border <- hemi_color
    colors_in <- c(alpha(hemi_color[1],0.3),alpha(hemi_color[2],0.3))

  }

  # grid specifications ***NOTE: include a +1 as well??
  axislab <- c(-4,-3,-2,-1,"HC mean")

  grid_lty <- c(1,2,2,2,2)
  grid_cols <- c("black", "grey","grey","grey","grey")

  radarplot <- fmsb::radarchart( radardatmm  , axistype=4 , #seg = 6,
                                 #custom polygon
                                 pcol=colors_border, pfcol=colors_in, plwd=hemi_lwd , plty=hemi_lty,
                                 #custom the grid
                                 cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, # cglty=c(2,1,3,5,6,2,2,2,2),
                                 #custom labels
                                 vlcex=0.8, vlabels=col_order,
                                 #custom axis
                                 caxislabels=axislab,
                                 #title
                                 title=pt_ID
  )

  return(radarplot)
}

