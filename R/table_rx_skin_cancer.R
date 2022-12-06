#' Create a data frame of drugs with an indication for skin cancer
#' @description
#' `table_rx_skin_cancer()` Creates a table of fda approvals in skin cancer
#' @param .data A data frame of unprocessed FDA appovals in skin cancer. Defaults to the embedded data set
#' @return a data frame
#' @export
#'
table_rx_skin_cancer <- function(.data = skincancerRx_data){

  ##########################################################################################################################
  # load data
  ##########################################################################################################################
  dt <- .data
  

  ### Table 1. Therapies with Indications for Skin Cancer

  # The goal of this table is to highlight key data used in this paper
  ## First let's do some data wrangling to create the table

  ## We are going to make a vector that lists a pivotal trial's design as "Single Arm", "Placebo-Controlled" or "Active Comparator"
  ind_usage <- str_detect(dt$submission_class_code_description, "Type") |
    str_detect(dt$submission_class_code_description, "Efficacy")

  skin_cancer_Dz <- c("Melanoma","MCC","SCC","AK","BCC","CTCL","DFSP","KS","Agnostic")

  dt_ind_usage <- dt[ind_usage,]
  dt_ind_usage <- dt_ind_usage %>%
    drop_na(New_Ind_in_Skin_Ca_Y_N)

  dt_ind_usage$Action_Date <- lubridate::as_date(dt_ind_usage$Action_Date)

  dt_ind_usage <- dt_ind_usage %>%
    mutate(row_num = row_number())


  dt_ind_usage <- dt_ind_usage %>%
    mutate(`Trial Design` = ifelse(
      test = SingleArmNum_N_Y == 1,
      yes = "Single Arm",
      no = ifelse(
        test = Placebo_Controlled_Trial == 1,
        yes = "Placebo-Controlled",
        no = ifelse(
          test = Comparator_Study_Placebo_Observation_or_Active_Comparator_N_Y == 1 & dt_ind_usage$Placebo_Controlled_Trial == 0,
          yes = "Non-Placebo Comparator",
          no = "")
      )
    )
    )


  table_drug_main <- dt_ind_usage %>%
    select(name,
           application_number,
           Indication,
           Indication_brief,
           submission_class_code_description,
           Action_Date,
           Dz,
           Endpoint_shortened,
           EE.Var,
           EE.Raw,
           Effect_Estimate,
           Major.Efficacy.Outcome.Measure,
           N,
           Subjects_Enrolled,
           lb_95CI_proposed,
           Toolshort,
           Accelerated_Approval,
           Mechanism_long
           )


  # Construct the Table of Interest
  table_drug_main <- table_drug_main %>%
    rename(`Therapeutic(s)` = name,
           `Indication (Brief)` = Indication_brief,
           `BLA/NDA` = application_number,
           `Submission Class` = submission_class_code_description,
           `Action Date` = Action_Date,
           `Skin Neoplasia` = Dz,
           `Primary Endpoint` = Endpoint_shortened,
           `Endpoint Variable` = EE.Var,
           `Endpoint Raw Data` = EE.Raw,
           `Endpoint Point Estimate` = Effect_Estimate,
           `Major Efficacy Outcome Measure` = Major.Efficacy.Outcome.Measure,
           `Number of Subjects` = N,
           `Subjects Enrolled` = Subjects_Enrolled,
           `Lower Bound 95% CI (%)` = lb_95CI_proposed,
           `Assessment Tool` = Toolshort,
           `Accelerated Approval` = Accelerated_Approval,
           Mechanism = Mechanism_long) %>%
    arrange(`Action Date`)



  ##########################################################################################################################
  # return data frame of interest
  ##########################################################################################################################

  return(table_drug_main)

}
