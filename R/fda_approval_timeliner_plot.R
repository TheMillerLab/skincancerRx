#' Create a plotly interactive timeline of the FDA approvals for skin cancer
#' @description fda_approval_timeliner_plot()` takes the aggregated data frame from fda_data_for_timeliner to produce a plotly data visualization the FDA approvals for skin cancer
#' 
#' @param .data is a data frame down stream of fda_data_for_timeliner. It defaults to the embedded dataset transformed by fda_data_for_timeliner, but allows for customization if the default is not used
#' @param .title Character string for the title of the character string
#' @param .startbuffer numeric value that represents the number of days prior to the first approved therapy on the data visualization
#' @param .endbuffer numeric value that represents the number of days after to the last approved therapy on the data visualization
#' @param .labelwrap numeric value that represents the length in charcters that will trigger a wrap of the text of each drug on the data visualization
#' @param .textbuffer numeric value that represents the distance away from the vetical line segment on the timeline 
#' @param .topboundary numeric value that represents the distance from the top of data visualization to the title
#' @param .bottomboundary numeric value that represents the distance from the bottom of the data visualization to the legend
#' @param .legend TRUE or FALSE to show legend or not
#' @param .geomtextsize numeric value of the geom_text font size
#' @param .legendtextsize numeric value of the legend text font size
#' @param .hoverlabeltextsize numeric value of the hover text font size
#' @param .xaxistickfontsize numeric value of the x axis text font size
#' 
#' @return A ggplotly Timeline of the FDA data set
#' 
#' @export
#' @examples 
#' fda_skin_cancer_data |> 
#'  filter(Dz == "Melanoma") |> 
#'  fda_data_for_timeliner() |> 
#'  fda_approval_timeliner_plot()

fda_approval_timeliner_plot <- function(.data = fda_approval_timeliner_df(),
                           .title = "<b>FDA Approvals in Cutaneous Oncology<b>",
                           .startbuffer = 1000,
                           .endbuffer = 1000,
                           .labelwrap = 100,
                           .textbuffer = 0.5,
                           .topboundary = 100,
                           .bottomboundary = 0,
                           .geomtextsize = 4,
                           .legendtextsize = 12,
                           .hoverlabeltextsize = 20,
                           .xaxistickfontsize = 14,
                           .legend = TRUE){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  timeline <- .data
  
  ##########################################################################################################################
  # Create the Line Segments that will populate the Timeline
  ##########################################################################################################################
  # Each row in the data frame created by `combine_timeliner_dfs` is an event that will  populate the Timeline Thus, we
  ## need to generate a line segment that will attach that event to the center line of the Timeline We need a method that
  ### allows for a different number of line segments that is completely dependent on the number of observations that are found
  #### in each individual patient story
  
  # Therefore let's create a R object that is the length of the dataframe (since this will vary from patient to patient)
  timeline.x <- 1:nrow(timeline)
  # Create an empty integer object that we will fill with a for loop
  out <- vector(mode = "integer")
  # Use a for loop to populate the above integer object that will serve as the number of repeating line segments
  for(i in 1:25) {  # thus there will be four line segments
    out[i] <- i*2  ## that are spaced out by a factor of 11 (that actual length of the line segments will be determined later)
  }
  # Now that we have a object "out" we want to repeat it twice as this will serve as segments above and below the central line of the timeline
  # Create a R object that repeats the above df "out" twice out a length of the patient's timeline "timeline.x"
  timeline.rep <- base::rep(x = out,
                            each = 2,
                            len = length(timeline.x))
  # Because we want lines of the same length on either side of the central timeline line, we create a numeric R object of
  ## -1 and 1 that is the length of the patient's timeline and that we will use to multiply to the length of the segment later
  timeline.neg_rep <- rep_len(c(-1,1),
                              length.out = length(timeline.x))
  # Create a new R object that is a df combining "timeline.x", "timeline.rep", "timeline.neg_rep"
  vec <- data.frame(timeline.c = timeline.x,
                    timeline.d = timeline.rep,
                    timeline.e = timeline.neg_rep)
  # Add a vector to "vec" that is the product of timeline.rep and timeline.neg_rep which be used as a base for the length
  ## of the segments above and below the central timeline line
  vec$timeline.f <- timeline.rep*timeline.neg_rep
  # Add another vector to "vec" that now takes the vector we just made "timeline.f" and add "50" to it, as 50 is the center
  ## of a 100 pixel plot. This should only have positive numbers at this point in a range of 1-100.
  vec$timeline.line.coord <- vec$timeline.f+50
  # Add that above vector "timeline.text.coord", which has the coordinates of where the lines will go to the "timeline"
  ## dataframe
  timeline$y <- vec$timeline.line.coord
  
  ##########################################################################################################################
  # Create the Coordinates for the text labels for the timeline
  ##########################################################################################################################
  # We want the text of each timeline to be offset a bit from the line segment, so let's create a new vector that is a
  ## multiple of "timeline.e" by some amount (here 1)
  vec$timeline.text.coord.pre <- .textbuffer*vec$timeline.e # .textbuffer is set to 0 as a default
  # now add vec$timeline.text.coord.pre  with vec$timeline.line.coord to get the position on the timeline where the text
  ## will go
  vec$timeline.text.coord <- vec$timeline.text.coord.pre + vec$timeline.line.coord
  # Add this vector "timeline.text.coord" to the "timeline" data frame and call it "label"
  timeline$label <- vec$timeline.text.coord
  
  ##########################################################################################################################
  # Develop the "x-axis' of the time
  ##########################################################################################################################
  # Let's begin by defining the start of the timeline
  start_date.a <- timeline %>% slice_head()
  # Let's build in a buffer before the start date
  start_date <- start_date.a$date - .startbuffer
  # Let's define the end of the timeline
  end_date.a <- timeline %>%
    slice_tail()
  # Let's build in a day buffer after the end date
  end_date <- end_date.a$date + .endbuffer
  # Create an R object that is a Date vector that spans the start_date to the end_date by "days"
  dateVec <- seq(from = start_date,
                 to = end_date,
                 by = "days")
  # Create an R object that is a Date vector that spans the start_date to the end_date by "4 months" which will serve as
  ## the labels for the graphy
  x_axis_label <- seq(from = start_date,
                      to = end_date,
                      by = "10 years")
  
  
  ##########################################################################################################################
  # Create the Timeliner Plot
  ##########################################################################################################################
  # Create a blank data frame that will serve as the substrate for the graph
  plot.timeliner.blank <- data.frame()
  # Build the TimelineR plot using ggplot2
  plot.timeliner <- ggplot(plot.timeliner.blank) +
    ##### Define the x and y boundaries as 0 - 100
    xlim(0, 100) +
    ylim(0, 100) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank())+
    theme(axis.text.x = element_text(
      face = "bold", size = 12, vjust = -0.5
      ),
          axis.text.y = element_blank(),
          axis.ticks.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank()) +
    ##### Set a horizontal line at 50, midway through the plot which serves as the timeline
    geom_hline(yintercept = 50) +
    ##### Set a horizontal line at 100, to force graph to be full size
    geom_hline(yintercept = .topboundary, color = "white") + # .topboundary can be adjusted, but is defaulted at 100
    ##### Set a horizontal line at 0, to force graph to be full size
    geom_hline(yintercept = .bottomboundary, color="white") + # .bottomboundary can be adjusted, but is defaulted at 0
    ##### Graph the vertical segments that span the midline to the text label
    geom_segment(data = timeline,
                 aes(x = date,
                     xend = date,
                     y = y,
                     yend = 50
                 )) +
    ##### Graph the text labels
    geom_text(data = timeline,
              aes(x = date,
                  y = label,
                  label = stringr::str_wrap(name, .labelwrap),
                  text = hover,
                  color = Mechanism), #choose description 1 or 2 depending on which you want colored
              size = .geomtextsize,
              position = position_nudge()) +
    ##### Format the X Axis
    scale_x_date(breaks = x_axis_label, # this is the name of a vector i created to set the x-axis, otherwise R was deciding for me
                 labels = scales::date_format("%m/%Y"), # This will control the format the above vector will be displayed "%Y" will be a 4 digit date
                 limits = c(min(dateVec), max=max(dateVec)))  +  # here I had to create a separate vector called "dateVec" to set the limits on the x
    #### Format Legend
    theme(
      legend.position = c(0.41, 0.99),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(1, 1, 1, 1),
      legend.title = element_text(size= 14, face = "bold"),
      legend.key = element_rect(size = 10),
      legend.text = element_text(size = .legendtextsize, face = "bold")) +
    scale_y_continuous(expand = c(0.01, 0.01))  # #eliminates the space between Y axis and x axis
  
  
  ##########################################################################################################################
  # Convert the ggplot object to a plotly graph
  ##########################################################################################################################
  plot.timeliner.plotly <- plotly::ggplotly(p = plot.timeliner,
                                            tooltip = "text") %>%
    layout(title = list(text = paste0(.title)),
           titlefont = list(size = 28,
                            color = "black",
                            family = "Arial")) %>%
    layout(subtitle = "testing") %>%
    layout(showlegend = .legend) %>%
    layout(hoverlabel = list(font=list(size=.hoverlabeltextsize))) %>%
    layout(xaxis = list(tickfont = list(size = .xaxistickfontsize))) %>%
    layout(margin = list(
      l = 10,
      r = 10,
      b = -10,
      t = 41)) %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.05))
  
  ##########################################################################################################################
  # Return the plotly graph of the timeliner
  ##########################################################################################################################
  return(plot.timeliner.plotly)
}
