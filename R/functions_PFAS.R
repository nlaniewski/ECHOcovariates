#' PFAS dataframe
#'
#' @param pfas.dataset.filepath file path to PFAS dataset (BLIS)
#'
#' @return a curated data.frame
#' @export
#
generate.pfas.data.frame <- function(pfas.dataset.filepath){
  ##SUBSET SOURCE FILE BASED ON 'pfas.of.interest','columns.of.interest'
  pfas.of.interest <- c("PFDA",
                        "PFHXS",
                        "PFNA",
                        "PFOA",
                        "PFOS",
                        "PFPEA")

  ##
  other.columns.of.interest <- c("Participant ID",
                                 "Family Id",
                                 "Sequence Num",
                                 "Visit",
                                 "Pca Weeks Pfas")#double-check;case-sensitive
  ##READ IN SOURCE
  pfas.data <- readxl::read_xlsx(pfas.dataset.filepath)
  ##SUBSET
  pfas.data.subset <- pfas.data[,colnames(pfas.data) %in% c(other.columns.of.interest, pfas.of.interest)]
  ##Log10 TRANSFORM
  message("Log10 (+1) transforming pfas...")
  pfas.data.subset[,pfas.of.interest] <- log10(pfas.data.subset[,pfas.of.interest]+1)#adding a constant (+1) to deal with '-Inf'
  ##
  #rm(pfas.of.interest)
  colnames(pfas.data.subset) <- gsub(" ",".",tolower(colnames(pfas.data.subset)))
  return(pfas.data.subset)
}