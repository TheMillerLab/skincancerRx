#' fda_skin_cancer_data
#'
#' @return data of FDA approvals for skin cancer
#' @export
#'
fda_skin_cancer_data <- function(){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- read.csv("/Users/davidmiller/Dropbox (Partners HealthCare)/mLab/The Evolving Landscape of Regulatory Medicine in Cutaneous Oncology/FDA_cOnc_Data/files/FDA_cONC.csv")

  ##########################################################################################################################
  # Return DF
  ##########################################################################################################################

  return(dt)

}
