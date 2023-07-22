#' Creates an interactive plot of fda approvals in skin cancer overtime
#' @description
#' `fda_approval_timeseries_plot()` Creates an interactive plot of fda approvals in skin cancer overtime
#'
#' @param .data  DF downstream of fda_approval_timeseries_df. Default is the data processed, but this also allows for customization of original FDA data
#' @param .title character string of the title of the plot
#' @param .startdate character string of data in Y-M-D format that designates origin of the plot, defaults to "1948-01-01"
#' @param .enddate character string of data in Y-M-D format that designates end of the plot, defaults to Sys.Date()
#' @param .legendtextsize numeric value of the legend text font size
#' @param .hoverlabeltextsize numeric value of the hover text font size
#' @param .xaxistickfontsize numeric value of the x axis text font size
#' @param .legend TRUE or FALSE to designate whether or not the plot legend will be present
#' @param .legendorientationy numeric value of the y position of the legend
#' @param .legendorientationx numeric value of the x position of the legend
#'
#' @return an interactive plot
#' @export
#' @examples 
#' fda_skin_cancer_data |> 
#'  filter(Dz == "Melanoma") |> 
#'  fda_approval_timeseries_df() |> 
#'  fda_approval_timeseries_plot()
#' 
#' fda_skin_cancer_data |> 
#'  filter(Dz == "MCC") |> 
#'  fda_approval_timeseries_df() |> 
#'  fda_approval_timeseries_plot(.startdate = "2016-01-01")
#'
fda_approval_timeseries_plot <- function(.data = fda_approval_timeseries_df(),
                                         .title = "<b>FDA Approvals in Cutaneous Oncology<b>",
                                         .startdate = "1948-01-01",
                                         .enddate = Sys.Date(),
                                         .legendtextsize = 14,
                                         .hoverlabeltextsize = 20,
                                         .xaxistickfontsize = 14,
                                         .legend = TRUE,
                                         .legendorientationy = 1,
                                         .legendorientationx = 0.01){
  ##########################################################################################################################
  # load data
  ##########################################################################################################################

  time_series.1 <- .data

  time_series <- time_series.1

  time_series$y <- seq(1:length(time_series.1$y))

  ##########################################################################################################################
  # Create Hover Text
  ##########################################################################################################################
  a <- paste("<b>Therapeutic:</b>", time_series$name)
  b <- paste("<b>Approval Date:</b>", time_series$date)
  c <- paste("<b>Disease:</b>", time_series$Dz)
  d <- paste("<b>Indication:</b>", time_series$Indication_brief)
  e <- paste("<b>Mechanism:</b>", time_series$Mechanism)

  time_series$hover <- paste(a, b, c, d, e,
                              sep = "\n")




  ##########################################################################################################################
  #  Key for the x axis of the plot below
  ##########################################################################################################################


  # Create a date vector for the limits of the x axis, for some reason this is necessary
  dateVec2 <- seq(from = as.Date(.startdate),
                  #to = as.Date("2021-12-31"),
                  to = .enddate,
                  by = "days")

  # The vector below will be used to set the x axis breaks, so that you can control them; in our case I wanted just 4 digit year dates, so I'm using %Y, but in reality, the labels = date_form() will control the presentation of the years
  x_axis_label_FDA_Timeline <- seq(from = as.Date(.startdate, format = "%Y"),
                                   #to = as.Date("2021-12-31", format =  "%Y"),
                                   to = .enddate,
                                   by = "10 years")

  ##########################################################################################################################
  #   Plot of FDA Timeline and skin cancer
  ##########################################################################################################################


  ## of note, we combined two data frames for this plot: "time_series" and "FDA_Timeline"
  ### One way I was able to do this is be creating the data frames to have the same x and y axes variables -> here called "date" and "y"
  #### That way in the ggplot function I placed a "NULL" for the data and used the aes() function with the "date" and "y" as the x and y parameters
  fda.approved.plot <-  ggplot(NULL,
                               aes(x = date,
                                   y = y)) +
    #### Add geometric data visualizations for the time_series data
    geom_point(data = time_series,
               aes(color = Dz,
                   text = hover),
               position=position_jitter(w=0.15,h=0)) +
    #### Add geometric data visualizations for FDA_Timeline data
    #### Format the Plot Title and Axes labels
    labs(x = "Date of FDA Approval",
         y = "Cummulative Number of FDA Approvals",
         title = "FDA Approvals in Cutaneous Oncology",
         subtitle = "") +
    theme(plot.title = element_text(hjust=0.5,
                                    face = "bold",
                                    size = 18)) +
    theme(plot.subtitle = element_text(hjust=0.5,
                                       face = "italic",
                                       size = 17)) +
    theme(axis.title.y = element_text(face="bold",
                                      size = 14,
                                      margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(axis.title.x = element_text(face="bold",
                                      size = 14,
                                      margin = margin(t = 10, r = 0, b = 0, l = 0)))+
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    ##### Format the Y Axis
    scale_y_continuous(limits = c(0, max(time_series$y) + 5),
                     #  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130),
                       expand = c(0,0)) + # #eliminates the space between Y axis and x axis
    ##### Format the X Axis
    scale_x_date(breaks = x_axis_label_FDA_Timeline, # this is the name of a vector i created to set the x-axis, otherwise R was deciding for me
                 labels = scales::date_format("%Y"), # This will control the format the above vector will be displayed "%Y" will be a 4 digit date
                 limits = c(min(dateVec2), max=max(dateVec2))) + # here I had to create a separate vector called "dateVec2" to set the limits on the x axis (that vector is above)
    #### Format the panel of the graphic
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line()) +
    theme(legend.position = "none")  +
    theme(plot.margin = margin(0,0,0,0,"cm"),
          panel.spacing.x = unit(0, "cm")) +
    coord_cartesian(clip = "off")



  ggplotly(fda.approved.plot ,
           tooltip = c("text")) %>%
    layout(title = .title,
           titlefont = list(size = 28, color = "black",
                            family = "Arial")) %>%
    layout(showlegend = .legend,
           xaxis = list(tickfont = list(size = .xaxistickfontsize)))  %>%
    layout(hoverlabel = list(font=list(size=.hoverlabeltextsize))) %>%
    layout(margin = list(
      t = 120,
      b = 20,
      r = 20,
      l = 20
    )) %>%
    layout(legend = list(
      font = list(
        size = .legendtextsize
      )
    )) %>%
    layout(legend = list(
      title=list(
        text="<b>Type of Skin Cancer</b>"
      )
    )) %>%
    layout(legend = list(orientation = "V", x = .legendorientationx, y = .legendorientationy))



}

