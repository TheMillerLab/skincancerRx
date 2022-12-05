#' Creates a data frame that can be used by fda_appropval_timeline_plot to generate an interactive plot
#' @description
#' `fda_approval_timeline_df()` Creates a data frame that can be used by fda_appropval_timeline_plot to generate an interactive plot
#' @param data  A data frame of FDA cutaneous oncology data
#'
#' @return a data frame
#' @export
#'
fda_approval_timeline_df <- function(data){

  ##########################################################################################################################
  # load data
  ##########################################################################################################################
  dt <- data

  # convert dates into the appropriate format for R
  dt$Action_Date <- as.Date(dt$Action_Date)


  dt <- dt %>%
    mutate(row_num = row_number())
  dt_length <- length(dt$row_num)

  # Let's pick only those rows with and indication or usage in skin cancer
  ind_usage <- str_detect(dt$submission_class_code_description, "Type") |
    str_detect(dt$submission_class_code_description, "Efficacy")

  skin_cancer_Dz <- c("Melanoma","MCC","SCC","AK","BCC","CTCL","DFSP","KS","Agnostic")

  dt_ind_usage <- dt[ind_usage,]
  dt_ind_usage <- dt_ind_usage %>%
    drop_na(New_Ind_in_Skin_Ca_Y_N)

  dt_ind_usage_length <- length(dt_ind_usage$name)

  ##########################################################################################################################
  # Let's identify the number of distinct substances and brands approved in skin cancer
  ##########################################################################################################################

  name_unique <- unique(dt$name)
  name_unique_length <- length(name_unique)

  brand_name_unique <- unique(dt$brand_name)
  brand_name_unique_length <- length(unique(dt$brand_name))

  applications_unique <- unique(dt$application_number)
  applications_unique_length <- length(unique(dt$application_number))

  # Identify number of modifications for labeling and MANUF CMC
  submission_label_cmc <- dt %>% filter(submission_class_code == "LABELING" | submission_class_code == "MANUF (CMC)")
  submission_label_cmc_length <- length(submission_label_cmc$name)

  dt_ind_usage$Action_Date <- lubridate::as_date(dt_ind_usage$Action_Date)


  # Select only those columns with
  ind_usage <- str_detect(dt$submission_class_code_description, "Type") |
    str_detect(dt$submission_class_code_description, "Efficacy")

  skin_cancer_Dz <- c("Melanoma","MCC","SCC","AK","BCC","CTCL","DFSP","KS","Agnostic")


  ##########################################################################################################################
  # Create Time Series Data Frame
  ##########################################################################################################################
  time_series_ind_usage <- dt[ind_usage,]

  time_series_ind_usage_skin <- time_series_ind_usage %>%
    drop_na(New_Ind_in_Skin_Ca_Y_N)

  time_series <- time_series_ind_usage_skin %>%
    arrange(Action_Date) %>%
    select(Action_Date, name, Dz, Indication_brief, Mechanism, Mechanism_long, Mechanistic_Class) %>%
    mutate(y = row_number()) %>%
    rename(date = Action_Date)

  time_series$Mechanism <- stringr::str_trim(time_series$Mechanism)
  time_series$Mechanism_long <- stringr::str_trim(time_series$Mechanism_long)
  time_series$Mechanistic_Class <- stringr::str_trim(time_series$Mechanistic_Class)


  ##########################################################################################################################
  # Create Short Indication Variable
  ##########################################################################################################################
  adjuvant <- "adjuvant"
  locally_advanced_metastatic <- "locally advanced or metastatic|Unresectable or Metastatic|Unresectable, Recurrent and/or Metastatic|Unresectable or Metastatic Melanoma"
  refractory <- "refractory|second line"
  locally_advanced <- "locally advanced"
  metastatic <- "metastatic"
  dosing <- "dosing"
  ak <- "actinic kera"
  ctcl <- "Mycosis Fungoides"
  ks <- "kaposi"
  advanced <- "advanced"

  time_series <- time_series %>%
    mutate(
      Indication_short = ifelse(
        test = str_detect(time_series$Indication_brief, regex(adjuvant, ignore_case = TRUE)),
        yes = "Adjuvant",
        no = ifelse(
          test = str_detect(time_series$Indication_brief, regex(refractory, ignore_case = TRUE)),
          yes = "Refractory",
          no = ifelse(
            test = str_detect(time_series$Indication_brief, regex(locally_advanced_metastatic, ignore_case = TRUE)),
            yes = "Locally Advanced or Metastatic",
            no = ifelse(
              test = str_detect(time_series$Indication_brief, regex(locally_advanced, ignore_case = TRUE)),
              yes = "Locally Advanced",
              no = ifelse(
                test = str_detect(time_series$Indication_brief, regex(metastatic, ignore_case = TRUE)),
                yes = "Metastatic",
                no = ifelse(
                  test = str_detect(time_series$Indication_brief, regex(dosing, ignore_case = TRUE)),
                  yes = "Dosing Adjustment",
                  no = ifelse(
                    test = str_detect(time_series$Indication_brief, regex(advanced, ignore_case = TRUE)),
                    yes = "Locally Advanced or Metastatic",
                    no = ifelse(
                      test = str_detect(time_series$Indication_brief, regex("laCSCC", ignore_case = TRUE)),
                      yes = "Locally Advanced",
                      no = ifelse(
                        test = str_detect(time_series$Indication_brief, regex("r/mCSCC", ignore_case = TRUE)),
                        yes = "Locally Advanced or Metastatic",
                        no = ifelse(
                          test = str_detect(time_series$Indication_brief, regex("unresectable", ignore_case = TRUE)),
                          yes = "Locally Advanced",
                          no = "Other"
                        )
                      )
                    )                )
              )
            )
          )
        )
      )
    ))

  time_series <- time_series %>% relocate(Indication_short, .after = Indication_brief)

  return(time_series)

}


