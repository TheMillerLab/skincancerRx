#' Creates an interactive plot of response rates for single arm trials
#' @description
#' `response_rate_plot()` Creates an interactive plot of response rates for single arm trials
#'
#' @param .data  A data frame downstream of the function `response_rate_df()`. Defaults to that data set, but allows for customization
#' @param .responseratedotsize numeric value of the size of the dot for the response rate seen on the graph, defaults to "4"
#' @param .hoverlabeltextsize numeric value of the size of the hover text, defaults to "20"
#' @param .lowerboundtextpositionX character string of the x axis position of the text at the bottom of the graph "Lower Bound 95CI Proposed by the Sponsor", defaults to "2013-11-25"
#' @param .lowerboundtextpositionY numeric value of the y axis position of the text "Lower Bound 95CI Proposed by the Sponsor", defaults to 1.5
#' @param .lowerboundfontsize  numeric value of the size of the text "Lower Bound 95CI Proposed by the Sponsor", defaults to 20
#' @param .xlabelpositionX  character string of the x axis position of the text at the bottom of the graph "X", defaults to "2010-01-01"
#' @param .xlabelpositionY numeric value of the y axis position of the text at the bottom of the graph "X", defaults to 1.55
#' @param .xsize numeric value of the size of the "X" at the bottom of the graph, defaults to 6
#' @param .pointpositionX  character string of the x axis position of the point (or filled circle) at the bottom of the graph which accompanies the "Response Rate...", defaults to "2010-01-01"
#' @param .pointpositionY numeric value of the y axis position of the the point at the bottom of the graph which accompanies the "Response Rate...", defaults to 5.5
#' @param .pointsize numeric value of the size of the point (or filled circle) at the bottom of the graph, defaults to 4.2
#' @param .responsepositionX character string of the x axis position of the text "Response Rate in the Label_Modifying Trial(s)", defaults to "2014-01-01"
#' @param .responsepositionY numeric value of the y axis position of the text "Response Rate in the Label_Modifying Trial(s)", defaults to 5.5
#' @param .responsefontsize numeric value of the font size of the text "Response Rate in the Label_Modifying Trial(s)", defaults to 4
#' @param .titlefontsize numeric value of the font size of the title of the graph", defaults to 28
#' @param .legend TRUE for present; FALSE to omit
#'
#' @return an interactive plot
#' @export
#'
response_rate_plot <- function(
    .data = skincancerRx::skincancerRx_data |> skincancerRx::response_rate_df(),
    .responseratedotsize = 4,
    .hoverlabeltextsize = 20,
    .lowerboundtextpositionX = "2013-11-25",
    .lowerboundtextpositionY = 1.5,
    .lowerboundfontsize = 4,
    .xlabelpositionX = "2010-01-01",
    .xlabelpositionY = 1.55,
    .xsize = 6,
    .pointpositionX = "2010-01-01",
    .pointpositionY = 5.5,
    .pointsize = 4.2,
    .responsepositionX = "2014-01-01",
    .responsepositionY = 5.5,
    .responsefontsize = 4,
    .titlefontsize = 28,
    .legend = FALSE
    ){

  ##########################################################################################################################
  # load data
  ##########################################################################################################################
  dt <- .data

  ##########################################################################################################################
  # Create Hover Text
  ##########################################################################################################################
  a <- paste("<b>Drug:</b>", dt$name, sep = " ")
  b <- paste("<b>Approval Date:</b>", dt$Action_Date, sep = " ")
  c <- paste("<b>Response Rate: </b>", dt$Response_Rate, "%", sep = "")
  d <- paste("<b>Upperbound 95% CI: </b>", dt$ub_95CI, "%", sep = "")
  e <- paste("<b>Lowerboud 95% CI: </b>", dt$lb_95CI, "%", sep = "")
  f <- paste("<b>Sponsor-proposed Null: </b>", dt$lb_95CI_proposed, "%", sep = "")
  g <- paste("<b>Indication:</b>", dt$Indication_brief)
  dt$hover <- paste(a, g, b, c, d, e, f, sep = "<br>")

  efficacy.plot <- ggplot(data = NULL) +
    geom_segment(data = dt,
                 mapping = aes(x = Action_Date_jitter,
                               xend = Action_Date_jitter,
                               y = lb_95CI,
                               yend = ub_95CI),
                 linetype = "dashed") +
    geom_point(data = dt,
               mapping = aes(x = Action_Date_jitter,
                             y = lb_95CI_proposed,
                             color = `Drug Name (Indication)`),
               size= 4.0,
               shape = 4) +
    geom_point(data = dt,
               mapping = aes(x = Action_Date_jitter,
                             y = lb_95CI,
                             color = `Drug Name (Indication)`),
               shape = 95,
               size = .responseratedotsize) +
    geom_point(data = dt,
               mapping = aes(x = Action_Date_jitter,
                             y = ub_95CI,
                             color = `Drug Name (Indication)`),
               shape = 95,
               size = 4) +
    geom_point(data = dt,
               mapping = aes(x = Action_Date_jitter,
                             y = Response_Rate,
                             color = `Drug Name (Indication)`,
                             text = hover),
               size = 3.5) +
    theme(plot.title = element_text(hjust=0.5,
                                    size = 16,
                                    face = "bold")) +
    theme(axis.title.y = element_text(face="bold",
                                      size = 14,
                                      margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(axis.title.x = element_text(face="bold",
                                      size = 14,
                                      margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    labs(y = "Response Rate",
         x = "Date of FDA Approval",
         title = "Efficacy Assessment in Non-Comparator Trials") +
    theme(axis.title.x = element_text(face = "bold",
                                      size = 14)) +
    theme(axis.title.y = element_text(face = "bold",
                                      size = 14)) +
    theme(axis.text.x = element_text(size = 14,
                                     face = "bold")) +
    theme(axis.text.y = element_text(size = 14,
                                     face = "bold")) +
    theme(axis.line = element_line()) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
    theme(legend.position = "none") +
    annotate(geom = "text",
             x = as.Date(.lowerboundtextpositionX),
             y = .lowerboundtextpositionY,
             label = " = Lower Bound 95CI Proposed by the Sponsor",
             hjust = 1,
             size = .lowerboundfontsize) +
    annotate(geom = "text",
             x = as.Date(.xlabelpositionX),
             y = .xlabelpositionY, # 1.55,
             label = "X",
             color="black",
             fill="black",
             size = .xsize) + # 6
    annotate(geom = "point",
             x = as.Date(.pointpositionX),
             y = .pointpositionY,
             size = .pointsize, #4.2
             alpha=0.8,
             color="black", fill="black") +
    annotate(geom = "text",
             x = as.Date(.responsepositionX),
             y = .responsepositionY,
             label = " = Response Rate in the Label-Modifying Trial(s)",
             hjust = 1,
             size = .responsefontsize)


  ggplotly(efficacy.plot, tooltip = c("text")) %>%
    layout(title = "<b>Efficacy Assessment in Non-Comparator Trials<b>",
           titlefont = list(size = .titlefontsize, color = "black", family = "Arial")) %>%
    layout(showlegend = .legend,
           xaxis = list(tickfont = list(size = 16)))  %>%
    layout(hoverlabel = list(font=list(size= .hoverlabeltextsize)))  %>%
    layout(margin = list(
      t =120,
      b = 0,
      r = 100,
      l = 0
    ))


}
