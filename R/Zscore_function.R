# Z-score function
# There are 3 main functions here: COMBATTRACTS, RANKINT, COMPUTEZ, and RUNTRACTZ
# These will perform ComBat, perform rank-based INT, compute Z-scores for tracts for a patient

#--------------------------------------------------------

#' COMBAT function
#'
#' This function allows you to perform neuroCombat on tract data, if data are
#' extracted from multiple sites and/or scanners and require harmonisation.
#' @param input_data A dataframe containing unharmonised tract data.
#' @param ID_column The column index corresponding to participant ID
#' @param tractrange The column(s) corresponding to tract data
#' @param scanner_column The column corresponding to site or scanner ID
#' @param age_column The column corresponding to participant age
#' @keywords neuroCombat tracts
#' @export
#' @examples
#' COMBATTRACTS(tractdata, 1, 8:36, 5, 3, 7)

COMBATTRACTS <- function(input_data, ID_column, tractrange, scanner_column, age_column, group_column) {
  require(neuroCombat)
  require(reshape)
  require(tidyr)
  require(dplyr)

  # format dataframe
  FDCdat <- input_data[, c(ID_column,tractrange)]
  batch <- input_data[[scanner_column]]
  tractnames <- colnames(input_data)[tractrange]
  ID_name <- colnames(FDCdat)[1]
  dat_format <- data.frame(t(FDCdat[-1]))
  colnames(dat_format) <- FDCdat[,1]

  # set up model including age and group. **NOTE: include flexibility here?
  age <- input_data[[age_column]]
  group <- input_data[[group_column]]
  mod <- model.matrix(~age + group)

  # run data harmonization
  combat.harmonized <- neuroCombat(dat=dat_format, batch=batch, mod=mod)

  # Output combat data
  harmon_dat <- combat.harmonized$dat.combat # note: this is a matrix
  combat_data <- data.frame(t(harmon_dat))
  combat_data <- cbind(ID_name=rownames(combat_data), combat_data)

  return(combat_data)
}

#' RANKINT function
#'
#' This function performs rank-based inverse normal transformation (INT) on tract
#' data using the RNOmni 'RankNorm' function.
#' @param input_data A dataframe containing tract data.
#' @param tract Column name of tract data to be normalised
#' @param int_tract Column name of inverse-normal transformed tract data
#' @export
#' @examples
#' RANKINT(tractdata, CST_right, CST_right_INT)

RANKINT <- function(input_data, tract, int_tract) {
  require(RNOmni)
  input_data[[int_tract]] <- RNOmni::RankNorm(input_data[[tract]], k=3/8) #look at syntax for this
  return(input_data)
}

#' COMPUTEZ function
#'
#' This function computes the mean and SD on tract data across all healthy control
#' participants included in the data. Z-scores are then computed for a specified
#' patient, compared to the control cohort.
#' @param input_data A dataframe containing tract data
#' @param tract Column name of raw tract data
#' @param patient_ID The column index corresponding to patient ID
#' @param HC Healthy control identifier. Defaults to 0
#' @param ID_column The column index corresponding to participant IDs. Defaults to 1
#' @param Group_column The column index corresponding to the Group identifier. Defaults to 12
#' @export
#' @examples
#' COMPUTEZ(tractdata, CST_right, "P01", HC=0, 1, 7)

COMPUTEZ <- function(input_data, tract, patient_ID, HC = 0, ID_column, Group_column){
  tract_INT <- paste(tract, "_INT", sep="")
  HC_ID <- input_data[Group_column]==HC
  HCdat <- input_data[HC_ID,]
  tract_mean <- mean(HCdat[[tract_INT]])
  tract_SD <- sd(HCdat[[tract_INT]])
  pt_ID <- input_data[ID_column]==patient_ID
  pt_dat <- input_data[pt_ID,]
  tract_Z <- (pt_dat[[tract_INT]] - tract_mean) / tract_SD
  return(tract_Z)
}


#' RUNTRACTZ function
#'
#' This function performs the computation of tract-specific Z-scores for a given patient,
#' using the RANKINT and COMPUTEZ functions. A dataframe should be provided as input,
#' with a column corresponding to patient ID, and column(s) corresponding to tract-specific
#' measures included as a minimum. If data harmonisation is required, columns corresponding
#' to site (numeric identifier) and age should also be included
#' @param input_data A dataframe containing tract data
#' @param tractrange The column(s) corresponding to tract data
#' @param patient_ID The column index corresponding to patient ID
#' @param combat Perform neuroCombat site/scanner harmonisation? Defaults to TRUE
#' @param ID_column The column index corresponding to participant IDs. Defaults to 1
#' @param age_column The column index corresponding to age. Defaults to 8
#' @export
#' @examples
#' RUNTRACTZ(tractdata, 8:36, "P01)


RUNTRACTZ <- function(input_data, tractrange, patient_ID, combat=TRUE,
                      ID_column=1, scanner_column=5, age_column=3, group_column=7){
  # if combat = "TRUE", then run COMBAT, else nah
  Zdat <- data.frame(ID=patient_ID)
  # also grab age, ICV, scanner, other important variables into this dataframe?
  tractnames <- colnames(input_data[tractrange])
  if(combat==TRUE){
    # first combat
    combat_data <- COMBATTRACTS(input_data, ID_column, tractrange, scanner_column, age_column, group_column)
    clinical_data <- input_data[, -c(tractrange)]
    names(combat_data)[names(combat_data) == 'ID_name'] <- colnames(input_data)[1]
    combat_data <- merge(clinical_data, combat_data, by = colnames(input_data)[1])
    # then Z-scores
    for (t in tractnames) {
      tractname_INT <- paste(t, "_INT", sep="")
      INT_data <- RANKINT(combat_data, t, tractname_INT)
      newdat <- COMPUTEZ(INT_data, t, patient_ID, HC = 0, ID_column, group_column)
      Zdat[ , ncol(Zdat) + 1] <- newdat
      colnames(Zdat)[ncol(Zdat)] <- paste0(t, "_Z")
    }
  } else if(combat==FALSE){
    for (t in tractnames) {
      tractname_INT <- paste(t, "_INT", sep="")
      INT_data <- RANKINT(input_data, t, tractname_INT)
      newdat <- COMPUTEZ(INT_data, t, patient_ID, HC = 0, ID_column, group_column)
      Zdat[ , ncol(Zdat) + 1] <- newdat
      colnames(Zdat)[ncol(Zdat)] <- paste0(t, "_Z")
    }
  }

  return(Zdat)
}
