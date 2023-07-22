#' Create a bar graph of FDA Approvals Per Disease
#' @description
#' `fda_actions_per_disease_plot()` Create a bar graph of FDA approvals per type of skin cancer
#'
#' @param .data  A data frame of FDA cutaneous oncology data
#' @param .yaxistitlefont size of y axis title
#' @param .xaxistitlefont size of x axis title
#' @param .plottitlefont size of plot title
#' @param .xaxistextfont size of x axis text
#' @param .yaxistextfont size of y axis text
#' @param .geomtextfont size of geom_text 
#' @param .plotsubtitlefont size of subtitle
#'
#' @return a data frame
#' @export
#'
fda_actions_per_disease_plot <- function(
    .data = skincancerRx_data,
    .yaxistitlefont = 22,
    .xaxistitlefont = 22,
    .plottitlefont = 24,
    .xaxistextfont = 19,
    .yaxistextfont = 22,
    .geomtextfont = 7,
    .plotsubtitlefont = 22
    ){

  ##########################################################################################################################
  # load data
  ##########################################################################################################################
  dt <- .data

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
  # Select Relevant Columns
  ##########################################################################################################################
  dt.1 <- dt_ind_usage %>%
    dplyr::select(name, Dz)

  dt.1$UniversalID <- 1


  actions.per.dz.plot <- ggplot(data = dt.1,
         aes(x = factor(Dz,
                        levels = c("Melanoma", "CTCL", "AK", "KS","BCC","SCC","Agnostic","MCC","DFSP")),
             fill = UniversalID)) +
    geom_bar()+
    theme(axis.text.x = element_text(face="bold",
                                     size=.xaxistextfont, angle=0),
          axis.text.y = element_text(face="bold",
                                     size=.yaxistextfont, angle=0)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line())+
    xlab("Type of Skin Neoplasia") +
    ylab("Number of FDA Actions") +
    geom_text(stat = "count",
              aes(label = ..count..),
              size = .geomtextfont,
              vjust = -0.4) +
    #Format Title#############
  labs(title = "FDA Actions In Cutaneous Oncology",
       subtitle = "Actions Per Disease") +
    theme(plot.title = element_text(hjust=0.5,
                                    size = .plottitlefont,
                                    face = "bold")) +
    theme(plot.subtitle = element_text(hjust = 0.5,
                                       size = .plotsubtitlefont,
                                       face = "italic")) +
    theme(axis.title.y = element_text(face="bold",
                                      size = .yaxistitlefont,
                                      margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(axis.title.x = element_text(face="bold",
                                      size = .xaxistitlefont,
                                      margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(0,50),
                       breaks = c(10,20,30,40,50))

  return(actions.per.dz.plot)
}
