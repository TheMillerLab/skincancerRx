#' Create a df of the FDA Data that can be used for TimelineR
#' @description
#' `fda.data.for.timeliner()` takes the FDA data set and wrangles it into a DF that can then be used for TimelineR
#' @param .data fda data set
#' @return A ggplotly Timeline for a given data set
#' @export

#'
fda_data_for_timeliner <- function(){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- read.csv("/Users/davidmiller/Dropbox (Partners HealthCare)/mLab/The Evolving Landscape of Regulatory Medicine in Cutaneous Oncology/FDA_cOnc_Data/files/FDA_cONC.csv")

  data <- FDA.Skin.CanceR:::fda_approval_timeline_df(dt)

#  data <- data %>%
#    rename(value = name,
#           description1 = Dz,
#           description2 = Mechanistic_Class,
#           description3 = Indication_short)
#
  data$name <- str_to_title(data$name)

  data$hover.a <- paste("<b>Drug:</b>", data$name)
  data$hover.b <- paste("<b>Date of Approval:</b>", data$date)
  data$hover.c <- paste("<b>Indication:</b>", data$Indication_brief)
  data$hover.d <- paste("<b>Mechanism:</b>", data$Mechanism)
  data$hover <- paste(
    data$hover.a,
    data$hover.b,
    data$hover.c,
    data$hover.d,
    sep = "<br>"
  )

  data$date <- as.Date(data$date)

  ##########################################################################################################################
  # Return the plotly graph of the timeliner
  ##########################################################################################################################
  return(data)
}
