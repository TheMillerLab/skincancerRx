#' Creates a data frame that can be used by response_rate_plot to generate an interactive plot
#' @description
#' `response_rate_df()` Creates a data frame that can be used by response_rate_plot to generate an interactive plot trials
#' @param .data  A data frame of FDA cutaneous oncology data. Defaults to the embedded dataset
#'
#' @return a data frame
#' @export
#'
response_rate_df <- function(.data = skincancerRx_data){

  ##########################################################################################################################
  # load data
  ##########################################################################################################################
  dt <- .data 

  ##########################################################################################################################
  # Format data
  ##########################################################################################################################
  # convert dates into the appropriate format for R
  dt$Action_Date <- as.Date(dt$Action_Date)

#
#  # Filter for only those actions up to August 1, 2021
#  dt <- dt %>%
#    filter(Action_Date <= "2021-08-01")

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

  # Let's identify the number of distinct substances and brands approved in skin cancer
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

  dt_ind_usage <- dt_ind_usage %>% dplyr::arrange(Action_Date)



  ##########################################################################################################################
  # Create a data frame of non comparator trials
  ##########################################################################################################################

  trials_nonComp.pre <- dt_ind_usage %>%
    filter(Comparator_Study_Placebo_Observation_or_Active_Comparator_N_Y == 0)

  trials_nonComp.pre.1 <- trials_nonComp.pre %>%
    dplyr::select(name,
                  application_number,
                  Indication,
                  Indication_brief,
                  submission_class_code_description,
                  Action_Date,
                  Dz,
                  Endpoint_shortened,
                  Effect_Estimate,
                  Response_Rate,
                  N,
                  lb_95CI,
                  ub_95CI,
                  lb_95CI_proposed,
                  Toolshort,
                  Mechanism_long
                  )


  ##########################################################################################################################
  # Remove "RR:" and %
  ##########################################################################################################################
  trials_nonComp.pre.1$Response_Rate <- stringr::str_replace_all(
    string = trials_nonComp.pre.1$Response_Rate,
    pattern = "RR:",
    replacement = ""
  )

  trials_nonComp.1 <- splitstackshape::cSplit(
    indt = trials_nonComp.pre.1,
    splitCols = "Response_Rate",
    sep = "%"
  )

  trials_nonComp <- trials_nonComp.1 %>%
    rename(Response_Rate = Response_Rate_1) %>%
    select(-Response_Rate_2, -Response_Rate_3)

  trials_nonComp$Response_Rate <- stringr::str_trim(trials_nonComp$Response_Rate)
  trials_nonComp$Response_Rate <- as.numeric(trials_nonComp$Response_Rate)

  ##########################################################################################################################
  # Add Data for lb_95 and up_95 CI
  ##########################################################################################################################

  # Two of applications used 2 different null hypotheses for different indications. Thus we need to add two rows to get our data tidy
  ## Vismo
  trials_nonC_vismo_idx <- trials_nonComp$name == "VISMODEGIB"
  trials_nonC_vismo <- trials_nonComp[trials_nonC_vismo_idx,]
  trials_nonC_vismo <- trials_nonC_vismo %>%
    add_row(trials_nonC_vismo)
  trials_nonC_vismo$lb_95CI_proposed[1] <- 10
  trials_nonC_vismo$lb_95CI_proposed[2] <- 20
  trials_nonC_vismo$Indication_brief[1] <- "mBCC"
  trials_nonC_vismo$Indication_brief[2] <- "laBCC"

  ## Cemiplimab
  trials_nonC_cemiplimab_scc_idx <- trials_nonComp$Dz == "SCC" & trials_nonComp$name == "CEMIPLIMAB-RWLC"
  trials_nonC_cemiplimab_scc <- trials_nonComp[trials_nonC_cemiplimab_scc_idx,]
  trials_nonC_cemiplimab_scc <- trials_nonC_cemiplimab_scc %>%
    add_row(trials_nonC_cemiplimab_scc)
  trials_nonC_cemiplimab_scc$lb_95CI_proposed[1] <- 15
  trials_nonC_cemiplimab_scc$lb_95CI_proposed[2] <- 25
  trials_nonC_cemiplimab_scc$Indication_brief[1] <- "mCSCC"
  trials_nonC_cemiplimab_scc$Indication_brief[2] <- "laCSCC"

  ## Romidepsin used two trials, so lets make a row for each one
  trials_nonC_romidepsin_idx <- trials_nonComp$name == "ROMIDEPSIN"
  trials_nonC_romidepsin <- trials_nonComp[trials_nonC_romidepsin_idx,]
  trials_nonC_romidepsin <- trials_nonC_romidepsin %>%
    add_row(trials_nonC_romidepsin)
  trials_nonC_romidepsin$N[1] <- 96
  trials_nonC_romidepsin$N[2] <- 71
  trials_nonC_romidepsin$Response_Rate[1] <- 34
  trials_nonC_romidepsin$Response_Rate[2] <- 35
  trials_nonC_romidepsin$lb_95CI[1] <- 25
  trials_nonC_romidepsin$lb_95CI[2] <- 25
  trials_nonC_romidepsin$ub_95CI[1] <- 45
  trials_nonC_romidepsin$ub_95CI[2] <- 49

  ## vorinostat used two trials, so lets make a row for each one
  trials_nonC_vorinostat_idx <- trials_nonComp$name == "VORINOSTAT"
  trials_nonC_vorinostat <- trials_nonComp[trials_nonC_vorinostat_idx,]
  trials_nonC_vorinostat <- trials_nonC_vorinostat %>%
    add_row(trials_nonC_vorinostat)
  trials_nonC_vorinostat$N[1] <- 74
  trials_nonC_vorinostat$N[2] <- 33
  trials_nonC_vorinostat$Response_Rate[1] <- 29.7
  trials_nonC_vorinostat$Response_Rate[2] <- 24.2
  trials_nonC_vorinostat$lb_95CI[1] <- 19.7
  trials_nonC_vorinostat$lb_95CI[2] <- 11.1
  trials_nonC_vorinostat$ub_95CI[1] <- 41.5
  trials_nonC_vorinostat$ub_95CI[2] <- 42.3

  ## doxil_aa used two trials, so lets make a row for each one
  trials_nonC_doxil_aa_idx <- trials_nonComp$name == "DOXORUBICIN HYDROCHLORIDE" & trials_nonComp$Action_Date == "1995-11-17"
  trials_nonC_doxil_aa <- trials_nonComp[trials_nonC_doxil_aa_idx,]
  trials_nonC_doxil_aa <- trials_nonC_doxil_aa %>%
    add_row(trials_nonC_doxil_aa)
  trials_nonC_doxil_aa$N[1] <- 34
  trials_nonC_doxil_aa$N[2] <- 42
  trials_nonC_doxil_aa$Response_Rate[1] <- 27 #9/34
  trials_nonC_doxil_aa$Response_Rate[2] <- 48 # 20/42
  trials_nonC_doxil_aa$lb_95CI[1] <- 12.9
  trials_nonC_doxil_aa$lb_95CI[2] <- 32.0
  trials_nonC_doxil_aa$ub_95CI[1] <- 44.4
  trials_nonC_doxil_aa$ub_95CI[2] <- 63.6

  ## doxil_ra used two trials, so lets make a row for each one
  trials_nonC_doxil_ra_idx <- trials_nonComp$name == "DOXORUBICIN HYDROCHLORIDE" & trials_nonComp$Action_Date == "2008-06-10"
  trials_nonC_doxil_ra <- trials_nonComp[trials_nonC_doxil_ra_idx,]
  trials_nonC_doxil_ra <- trials_nonC_doxil_ra %>%
    add_row(trials_nonC_doxil_ra)
  trials_nonC_doxil_ra$N[1] <- 17
  trials_nonC_doxil_ra$N[2] <- 11
  trials_nonC_doxil_ra$Response_Rate[1] <-  41.2 # 7/17
  trials_nonC_doxil_ra$Response_Rate[2] <-  36.4 # 4/11
  trials_nonC_doxil_ra$lb_95CI[1] <- 18.4
  trials_nonC_doxil_ra$lb_95CI[2] <- 10.9
  trials_nonC_doxil_ra$ub_95CI[1] <- 67.1
  trials_nonC_doxil_ra$ub_95CI[2] <- 69.2

  ## Intron A used two trials, so lets make a row for each one
  trials_nonC_alfa_2b_idx <- trials_nonComp$name == "INTERFERON ALFA-2B"
  trials_nonC_alfa_2b <- trials_nonComp[trials_nonC_alfa_2b_idx,]
  trials_nonC_alfa_2b <- trials_nonC_alfa_2b %>%
    add_row(trials_nonC_alfa_2b)
  trials_nonC_alfa_2b$N[1] <- 72
  trials_nonC_alfa_2b$N[2] <- 23
  trials_nonC_alfa_2b$Response_Rate[1] <- 29.2 # 21/72
  trials_nonC_alfa_2b$Response_Rate[2] <- 30.4  # 7/23
  trials_nonC_alfa_2b$lb_95CI[1] <- 19.0
  trials_nonC_alfa_2b$lb_95CI[2] <- 13.2
  trials_nonC_alfa_2b$ub_95CI[1] <- 41.1
  trials_nonC_alfa_2b$ub_95CI[2] <- 52.9

  # Now let's remove the vismo and cemiplimab rows from the original df and add these new columns
  trials_nonComp <- trials_nonComp %>%
    filter(name != "VISMODEGIB")
  trials_nonComp <- trials_nonComp %>%
    filter(Action_Date != "2018-09-28")
  trials_nonComp <- trials_nonComp %>%
    filter(name != "ROMIDEPSIN")
  trials_nonComp <- trials_nonComp %>%
    filter(name != "VORINOSTAT")
  trials_nonComp <- trials_nonComp %>%
    filter(name != "DOXORUBICIN HYDROCHLORIDE")
  trials_nonComp <- trials_nonComp %>%
    filter(name != "INTERFERON ALFA-2B")

  # Now let's add the corresponding dfs
  trials_nonComp <- rbind(trials_nonComp,
                          trials_nonC_vismo,
                          trials_nonC_cemiplimab_scc,
                          trials_nonC_romidepsin,
                          trials_nonC_vorinostat,
                          trials_nonC_doxil_aa,
                          trials_nonC_doxil_ra,
                          trials_nonC_alfa_2b)

  # Make the Indication Brief even briefer
  null_avelumab <- trials_nonComp$name == "AVELUMAB"
  trials_nonComp$Indication_brief[null_avelumab] <- "mMCC"

  null_romidepsin <- trials_nonComp$name == "ROMIDEPSIN"
  trials_nonComp$Indication_brief[null_romidepsin] <- "rCTCL"

  null_bexarotene <- trials_nonComp$name == "BEXAROTENE"
  trials_nonComp$Indication_brief[null_bexarotene] <- "rCTCL"

  null_vorinostat <- trials_nonComp$name == "VORINOSTAT"
  trials_nonComp$Indication_brief[null_vorinostat] <- "rCTCL"

  null_methoxsalen <- trials_nonComp$name == "METHOXSALEN"
  trials_nonComp$Indication_brief[null_methoxsalen] <- "rCTCL"

  null_nivolumab <- trials_nonComp$name == "NIVOLUMAB"
  trials_nonComp$Indication_brief[null_nivolumab] <- "u/mMelanoma"

  null_hypo_pembro_melanoma <- trials_nonComp$name == "PEMBROLIZUMAB" & trials_nonComp$Dz == "Melanoma"
  trials_nonComp$Indication_brief[null_hypo_pembro_melanoma] <- "u/mMelanoma"

  null_sonidegib <- trials_nonComp$name == "SONIDEGIB PHOSPHATE"
  trials_nonComp$Indication_brief[null_sonidegib] <- "laBCC"

  null_doxil_aa <- trials_nonComp$name == "DOXORUBICIN HYDROCHLORIDE" & trials_nonComp$Action_Date == "1995-11-17"
  trials_nonComp$Indication_brief[null_doxil_aa] <- "AIDS KS (AA)"

  null_doxil_ra <- trials_nonComp$name == "DOXORUBICIN HYDROCHLORIDE" & trials_nonComp$Action_Date == "2008-06-10"
  trials_nonComp$Indication_brief[null_doxil_ra] <- "AIDS KS"

  null_alfa_2a <- trials_nonComp$name == "INTERFERON ALFA-2A"
  trials_nonComp$Indication_brief[null_alfa_2a] <- "AIDS KS"

  null_alfa_2b <- trials_nonComp$name == "INTERFERON ALFA-2B"
  trials_nonComp$Indication_brief[null_alfa_2b] <- "AIDS KS"

  null_paclitaxel <- trials_nonComp$name == "PACLITAXEL"
  trials_nonComp$Indication_brief[null_paclitaxel] <- "AIDS KS"

  # Rename BEXAROTENE GEL to distinguish the two
  null_bex_gel <- trials_nonComp$Action_Date == "2000-06-28"
  trials_nonComp$name[null_bex_gel] <- "BEXAROTENE GEL"

  null_aldesleukin <- trials_nonComp$name == "ALDESLEUKIN"
  trials_nonComp$Indication_brief[null_aldesleukin] <- "mMelanoma"

  null_imatinib <- trials_nonComp$name == "IMATINIB MESYLATE"
  trials_nonComp$Indication_brief[null_imatinib] <- "u/mDFSP"

  null_trametinib <- trials_nonComp$name == "TRAMETINIB DIMETHYL SULFOXIDE"
  trials_nonComp$Indication_brief[null_trametinib] <- "mMelanoma"

  null_dabrafenib <- trials_nonComp$name == "DABRAFENIB MESYLATE"
  trials_nonComp$Indication_brief[null_dabrafenib] <- "mMelanoma"

  null_dabrafenib <- trials_nonComp$name == "DABRAFENIB MESYLATE"
  trials_nonComp$Indication_brief[null_dabrafenib] <- "mMelanoma"

  null_pembro_tmb <- trials_nonComp$name == "PEMBROLIZUMAB" & trials_nonComp$Action_Date == "2020-06-16"
  trials_nonComp$Indication_brief[null_pembro_tmb] <- "TMB-hi"

  null_pembro_msi <- trials_nonComp$name == "PEMBROLIZUMAB" & trials_nonComp$Action_Date == "2017-05-23"
  trials_nonComp$Indication_brief[null_pembro_msi] <- "MSI-hi"

  null_pembro_mcc <- trials_nonComp$name == "PEMBROLIZUMAB" & trials_nonComp$Action_Date == "2018-12-19"
  trials_nonComp$Indication_brief[null_pembro_mcc] <- "la/mMCC"

  null_cemiplimab_labcc <- trials_nonComp$name == "CEMIPLIMAB-RWLC" & trials_nonComp$Indication_brief == "Locally Advanced Basal Cell Carcinoma"
  trials_nonComp$Indication_brief[null_cemiplimab_labcc] <- "laBCC"

  null_cemiplimab_mbcc <- trials_nonComp$name == "CEMIPLIMAB-RWLC" & trials_nonComp$Indication_brief == "Metastatic Basal Cell Carcinoma"
  trials_nonComp$Indication_brief[null_cemiplimab_mbcc] <- "mBCC"



  # Make numeric
  trials_nonComp$lb_95CI_proposed <- as.numeric(trials_nonComp$lb_95CI_proposed)

  # this is the total number of studies with lb_95 CI identifiable

  trials_nonComp_five_percent <- trials_nonComp %>% filter(lb_95CI_proposed == 5)
  trials_nonComp_five_percent_length <- length(trials_nonComp_five_percent$name)


  trials_nonComp_ten_percent <- trials_nonComp %>% filter(lb_95CI_proposed == 10)
  trials_nonComp_ten_percent_length <- length(trials_nonComp_ten_percent$name)


  trials_nonComp_fifeteen_percent <- trials_nonComp %>% filter(lb_95CI_proposed == 15)
  trials_nonComp_fifeteen_percent_length <- length(trials_nonComp_fifeteen_percent$name)


  trials_nonComp_twenty_percent <- trials_nonComp %>% filter(lb_95CI_proposed == 20)
  trials_nonComp_twenty_percent_length <- length(trials_nonComp_twenty_percent$name)


  trials_nonComp_twentyfive_percent <- trials_nonComp %>% filter(lb_95CI_proposed == 25)
  trials_nonComp_twentyfive_percent_length <- length(trials_nonComp_twentyfive_percent$name)

  # Replace all caps names with lower case
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "AVELUMAB", "Avelumab")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "BEXAROTENE", "Bexarotene")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "BEXAROTENE GEL", "Bexarotene Gel")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "CEMIPLIMAB-RWLC", "Cemiplimab-rwlc")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "METHOXSALEN", "Methoxsalen")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "NIVOLUMAB", "Nivolumab")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "PEMBROLIZUMAB", "Pembrolizumab")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "ROMIDEPSIN", "Romidepsin")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "SONIDEGIB PHOSPHATE", "Sonidegib")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "VISMODEGIB", "Vismodegib")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "VORINOSTAT", "Vorinostat")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "DOXORUBICIN HYDROCHLORIDE", "Doxorubicin")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "POMALIDOMIDE", "Pomalidomide")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "INTERFERON ALFA-2A", "Interferon Alfa-2a")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "INTERFERON ALFA-2B", "Interferon Alfa-2b")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "ALDESLEUKIN", "Aldesleukin")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "IMATINIB MESYLATE", "Imatinib")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "PACLITAXEL", "Paclitaxel")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "TRAMETINIB DIMETHYL SULFOXIDE", "Trametinib")
  trials_nonComp$name <- replace(trials_nonComp$name, trials_nonComp$name == "DABRAFENIB MESYLATE", "Dabrafenib")


  # replace N with the specific number per labeled indication
  avelumab_mMCC <- trials_nonComp$Indication_brief == "mMCC" & trials_nonComp$name == "Avelumab"
  trials_nonComp$N[avelumab_mMCC] <- 88
  trials_nonComp$Response_Rate[avelumab_mMCC] <- 33.0
  trials_nonComp$lb_95CI[avelumab_mMCC] <- 23.3
  trials_nonComp$ub_95CI[avelumab_mMCC] <- 43.8


  vismo_mBCC <- trials_nonComp$Indication_brief == "mBCC" & trials_nonComp$name == "Vismodegib"
  trials_nonComp$N[vismo_mBCC] <- 33
  trials_nonComp$Response_Rate[vismo_mBCC] <- 30.3
  trials_nonComp$lb_95CI[vismo_mBCC] <- 15.6
  trials_nonComp$ub_95CI[vismo_mBCC] <- 48.2

  vismo_laBCC <- trials_nonComp$Indication_brief == "laBCC" & trials_nonComp$name == "Vismodegib"
  trials_nonComp$N[vismo_laBCC] <- 63
  trials_nonComp$Response_Rate[vismo_laBCC] <- 42.9
  trials_nonComp$lb_95CI[vismo_laBCC] <- 30.5
  trials_nonComp$ub_95CI[vismo_laBCC] <- 56.0


  cemi_laCSCC <- trials_nonComp$Indication_brief == "laCSCC" & trials_nonComp$name == "Cemiplimab-rwlc"
  trials_nonComp$N[cemi_laCSCC] <- 33
  trials_nonComp$Response_Rate[cemi_laCSCC] <- 48.5
  trials_nonComp$lb_95CI[cemi_laCSCC] <- 30.8
  trials_nonComp$ub_95CI[cemi_laCSCC] <- 66.5


  cemi_mCSCC <- trials_nonComp$Indication_brief == "mCSCC" & trials_nonComp$name == "Cemiplimab-rwlc"
  trials_nonComp$N[cemi_mCSCC] <- 75
  trials_nonComp$Response_Rate[cemi_mCSCC] <- 46.7
  trials_nonComp$lb_95CI[cemi_mCSCC] <- 35.1
  trials_nonComp$ub_95CI[cemi_mCSCC] <- 58.6

  nivo_mMelanoma <- trials_nonComp$Indication_brief == "u/mMelanoma" & trials_nonComp$name == "Nivolumab"
  trials_nonComp$N[nivo_mMelanoma ] <- 120
  trials_nonComp$Response_Rate[nivo_mMelanoma ] <- 32.0
  trials_nonComp$lb_95CI[nivo_mMelanoma ] <- 23.0
  trials_nonComp$ub_95CI[nivo_mMelanoma ] <- 41.0

  pembro_mMelanoma <- trials_nonComp$Action_Date == "2014-09-14" & trials_nonComp$name == "Pembrolizumab"
  trials_nonComp$N[pembro_mMelanoma ] <- 173
  trials_nonComp$Response_Rate[pembro_mMelanoma ] <- 24.0
  trials_nonComp$lb_95CI[pembro_mMelanoma ] <- 15.0
  trials_nonComp$ub_95CI[pembro_mMelanoma ] <- 34.0

  sonidegib_laBCC <- trials_nonComp$Indication_brief == "laBCC" & trials_nonComp$name == "Sonidegib"
  trials_nonComp$N[sonidegib_laBCC] <- 66 # of note, the total number of laBCC was 194, but this ORR and CI is for the 66 tx'd with 200 mg
  trials_nonComp$Response_Rate[sonidegib_laBCC] <- 57.5 # 38/66 patients
  trials_nonComp$lb_95CI[sonidegib_laBCC] <- 44.7
  trials_nonComp$ub_95CI[sonidegib_laBCC] <- 69.7

  bex_caps_rCTCL <- trials_nonComp$Indication_brief == "rCTCL" & trials_nonComp$name == "Bexarotene"
  trials_nonComp$N[bex_caps_rCTCL] <- 62
  trials_nonComp$Response_Rate[bex_caps_rCTCL] <- 32.2 # 20/62
  trials_nonComp$lb_95CI[bex_caps_rCTCL] <- 20.9
  trials_nonComp$ub_95CI[bex_caps_rCTCL] <- 45.3

  bex_gel_rCTCL <- trials_nonComp$Indication_brief == "rCTCL" & trials_nonComp$name == "Bexarotene Gel"
  trials_nonComp$N[bex_gel_rCTCL] <- 50
  trials_nonComp$Response_Rate[bex_gel_rCTCL] <- 26 # 13/50
  trials_nonComp$lb_95CI[bex_gel_rCTCL] <- 14.6
  trials_nonComp$ub_95CI[bex_gel_rCTCL] <- 40.3

  methoxsalen_rCTCL <- trials_nonComp$Action_Date == "1999-02-25" & trials_nonComp$name == "Methoxsalen"
  trials_nonComp$N[methoxsalen_rCTCL] <- 51
  trials_nonComp$Response_Rate[methoxsalen_rCTCL] <- 33.3 # 17/51
  trials_nonComp$lb_95CI[methoxsalen_rCTCL] <- 20.8
  trials_nonComp$ub_95CI[methoxsalen_rCTCL] <- 47.9

  Pomalyst_HIV_neg <- trials_nonComp$name == "Pomalidomide" & trials_nonComp$Response_Rate == 80
  trials_nonComp$N[Pomalyst_HIV_neg] <- 10
  trials_nonComp$Response_Rate[Pomalyst_HIV_neg] <- 80 # 8/10
  trials_nonComp$lb_95CI[Pomalyst_HIV_neg] <- 44.4
  trials_nonComp$ub_95CI[Pomalyst_HIV_neg] <- 97.5
  trials_nonComp$Indication_brief[Pomalyst_HIV_neg] <- "HIV-neg KS"

  Pomalyst_HIV_pos <- trials_nonComp$name == "Pomalidomide" & trials_nonComp$Response_Rate == 67
  trials_nonComp$N[Pomalyst_HIV_pos] <- 18
  trials_nonComp$Response_Rate[Pomalyst_HIV_pos] <- 67 # 12/18
  trials_nonComp$lb_95CI[Pomalyst_HIV_pos] <- 41.0
  trials_nonComp$ub_95CI[Pomalyst_HIV_pos] <- 86.6
  trials_nonComp$Indication_brief[Pomalyst_HIV_pos] <- "HIV-pos KS"
  #trials_nonComp$Action_Date[Pomalyst_HIV_pos] <- "2020-05-14"






  ##########################################################################################################################
  # Now let's create our own natural jitter for our graph for those actions that happened on the same day
  ##########################################################################################################################

  trials_nonComp$Action_Date_jitter <- trials_nonComp$Action_Date


  ifn_2a <- trials_nonComp$name == "Interferon Alfa-2a"
  trials_nonComp$Action_Date_jitter[ifn_2a] <- "1988-08-21"

  ifn_2b_30 <- trials_nonComp$name == "Interferon Alfa-2b" &trials_nonComp$N == 23
  trials_nonComp$Action_Date_jitter[ifn_2b_30] <- "1989-02-21"

  doxo_42 <- trials_nonComp$name == "Doxorubicin" &trials_nonComp$N == 42
  trials_nonComp$Action_Date_jitter[doxo_42] <- "1996-02-17"

  doxo_11 <- trials_nonComp$name == "Doxorubicin" &trials_nonComp$N == 11
  trials_nonComp$Action_Date_jitter[doxo_11] <- "2008-09-10"

  vorinostat_74 <- trials_nonComp$name == "Vorinostat" & trials_nonComp$N == 74
  trials_nonComp$Action_Date_jitter[vorinostat_74] <- "2006-07-06"

  imatinib <- trials_nonComp$name == "Imatinib"
  trials_nonComp$Action_Date_jitter[imatinib] <- "2007-02-01"

  Romidepsin_96 <- trials_nonComp$name == "Romidepsin" & trials_nonComp$N == 96
  trials_nonComp$Action_Date_jitter[Romidepsin_96] <- "2009-08-05"

  Vismodegib_33 <- trials_nonComp$name == "Vismodegib" & trials_nonComp$N == 33
  trials_nonComp$Action_Date_jitter[Vismodegib_33] <- "2012-04-30"


  Cemiplimab_75 <- trials_nonComp$name == "Cemiplimab-rwlc" & trials_nonComp$N == 75
  trials_nonComp$Action_Date_jitter[Cemiplimab_75] <- "2018-06-28"

  trametinib <- trials_nonComp$name == "Trametinib"
  trials_nonComp$Action_Date_jitter[trametinib] <- "2019-07-06"

  pembro_cscc <- trials_nonComp$name == "Pembrolizumab" & trials_nonComp$Action_Date == "2020-06-24"
  trials_nonComp$Action_Date_jitter[pembro_cscc] <- "2020-08-24"

  cemi_labcc <- trials_nonComp$name == "Cemiplimab-rwlc" & trials_nonComp$Response_Rate == 29.00
  trials_nonComp$Action_Date_jitter[cemi_labcc] <- "2020-11-09"

  Pomalyst_HIV_pos <- trials_nonComp$name == "Pomalidomide" & trials_nonComp$Response_Rate == 67.00
  trials_nonComp$Action_Date_jitter[Pomalyst_HIV_pos] <- "2020-05-01"



  # create a new column that is a merge of Drug Name and Indication_brief for the legend
  trials_nonComp$`Drug Name (Indication)` <- paste(trials_nonComp$name,
                                                   " ",
                                                   "(",
                                                   trials_nonComp$Indication_brief,
                                                   ")",
                                                   sep = "")


  trials_nonComp <- trials_nonComp %>%
    drop_na(Response_Rate)

  trials_nonComp$`Drug Name (Indication)` <- factor(trials_nonComp$`Drug Name (Indication)`,
                                                    levels= c("Interferon Alfa-2a (AIDS KS)",
                                                              "Interferon Alfa-2b (AIDS KS)",
                                                              "Doxorubicin (AIDS KS (AA))",
                                                              "Paclitaxel (AIDS KS)",
                                                              "Aldesleukin (mMelanoma)",
                                                              "Methoxsalen (rCTCL)",
                                                              "Bexarotene (rCTCL)",
                                                              "Bexarotene Gel (rCTCL)",
                                                              "Vorinostat (rCTCL)",
                                                              "Imatinib (u/mDFSP)",
                                                              "Doxorubicin (AIDS KS)",
                                                              "Romidepsin (rCTCL)",
                                                              "Vismodegib (mBCC)",
                                                              "Vismodegib (laBCC)",
                                                              "Pembrolizumab (u/mMelanoma)",
                                                              "Nivolumab (u/mMelanoma)",
                                                              "Sonidegib (laBCC)",
                                                              "Avelumab (mMCC)",
                                                              "Pembrolizumab (MSI-hi)",
                                                              "Cemiplimab-rwlc (mCSCC)",
                                                              "Cemiplimab-rwlc (laCSCC)",
                                                              "Pembrolizumab (la/mMCC)",
                                                              "Trametinib (mMelanoma)",
                                                              "Dabrafenib (mMelanoma)",
                                                              "Pomalidomide (HIV-pos KS)",
                                                              "Pomalidomide (HIV-neg KS)",
                                                              "Pembrolizumab (TMB-hi)",
                                                              "Pembrolizumab (r/mCSCC)",
                                                              "Cemiplimab-rwlc (laBCC)",
                                                              "Cemiplimab-rwlc (mBCC)",
                                                              "Pembrolizumab (laCSCC)"))

  trials_nonComp_length <- length(unique(trials_nonComp$`Drug Name (Indication)`))

  trials_nonComp$Action_Date <- as.Date(trials_nonComp$Action_Date)
  trials_nonComp$Response_Rate <- as.numeric(trials_nonComp$Response_Rate)
  trials_nonComp$lb_95CI_proposed<- as.numeric(trials_nonComp$lb_95CI_proposed)
  trials_nonComp$lb_95CI<- as.numeric(trials_nonComp$lb_95CI)
  trials_nonComp$ub_95CI<- as.numeric(trials_nonComp$ub_95CI)

  trials_nonComp <- trials_nonComp %>% relocate(Response_Rate, .before = lb_95CI)

  trials_nonComp <- trials_nonComp %>% dplyr::arrange(Action_Date)
}
