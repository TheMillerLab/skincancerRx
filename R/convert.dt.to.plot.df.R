#' Converts the dt from the FDA cutaneous oncology into one that can be used by the `fda.plot()` function
#' @description
#' `convert.dt.to.plot.df()` converts the dt from the FDA cutaneous oncology into one that can be used by the `fda.plot()` function
#' @param data  A data frame of FDA cutaneous oncology data
#'
#' @return a data frame
#' @export
#'
convert.dt.to.plot.df <- function(data){

  ##########################################################################################################################
  # load data
  ##########################################################################################################################
  dt <- data

  ##########################################################################################################################
  # Select Therapies with an indication for Skin Cancer
  ##########################################################################################################################

  ind_usage <- str_detect(dt$submission_class_code_description, "Type") |
    str_detect(dt$submission_class_code_description, "Efficacy")
  skin_cancer_Dz <- c("Melanoma","MCC","SCC","AK","BCC","CTCL","DFSP","KS","Agnostic")

  dt_ind_usage <- dt[ind_usage,]

  dt_ind_usage <- dt_ind_usage %>% drop_na(New_Ind_in_Skin_Ca_Y_N)

  dt_ind_usage$Action_Date <- lubridate::as_date(dt_ind_usage$Action_Date)

  dt_ind_usage <- dt_ind_usage %>%
    mutate(row_num = row_number())

  ##########################################################################################################################
  # Make a vector that lists a pivotal trial's design as "Single Arm", "Placebo-Controlled" or "Active Comparator"
  ##########################################################################################################################
  dt_ind_usage <- dt_ind_usage %>%
    mutate(`Trial Design` = ifelse(SingleArmNum_N_Y == 1,
                                   "Single Arm",
                                   ifelse(Placebo_Controlled_Trial == 1,
                                          "Placebo-Controlled",
                                          ifelse(Comparator_Study_Placebo_Observation_or_Active_Comparator_N_Y == 1 & dt_ind_usage$Placebo_Controlled_Trial == 0,
                                                 "Non-Placebo Comparator",
                                                 ""))))


  ##########################################################################################################################
  # Select Desired Columns
  ##########################################################################################################################
  dt.2 <- dt_ind_usage %>% select(name,
                                             application_number,
                                             Indication_brief,
                                             submission_class_code_description,
                                             Action_Date,
                                             Dz,
                                             Endpoint_shortened,
                                             N,
                                             lb_95CI_proposed,
                                             Toolshort,
                                             Accelerated_Approval,
                                             Mechanism_long)

  ##########################################################################################################################
  # Rename variables
  ##########################################################################################################################

  dt.3 <- dt.2 %>%
    rename(`Therapeutic` = name,
           `Indication` = Indication_brief,
           `BLA/NDA` = application_number,
           `Submission Class` = submission_class_code_description,
           date = Action_Date,
           Disease = Dz,
           `Primary Endpoint` = Endpoint_shortened,
           `Subjects Enrolled` = N,
           `Lower Bound 95% CI (%)` = lb_95CI_proposed,
           `Assessment Tool` = Toolshort,
           `Accelerated Approval` = Accelerated_Approval,
           Mechanism = Mechanism_long)


  ##########################################################################################################################
  # Create Hover Text
  ##########################################################################################################################

  dt.3$hover.a <- paste("<b>Therapeutic(s):</b>",
                        dt.3$Therapeutic)

  dt.3$hover.b <- paste("<b>Mechanism:</b>",
                        dt.3$Mechanism)

  dt.3$hover.c <- paste("<b>Indication:</b>",
                        dt.3$Indication)

  dt.3$hover.d <- paste("<b>Action Date:</b>",
                        dt.3$date)

  dt.3$hover.e <- paste("<b>Primary Endpoint:</b>",
                        dt.3$`Primary Endpoint`)

  dt.3$hover.f <- paste("<b>Subjects Enrolled:</b>",
                        dt.3$`Subjects Enrolled`)

  dt.3$hover <- paste(dt.3$hover.a,
                      dt.3$hover.b,
                      dt.3$hover.c,
                      dt.3$hover.d,
                      dt.3$hover.e,
                      dt.3$hover.f,
                      sep = "<br>")

  ##########################################################################################################################
  # Arrange DF by Date
  ##########################################################################################################################

  dt.4 <- dt.3 %>% dplyr::arrange(date)


  return(dt.4)
}
