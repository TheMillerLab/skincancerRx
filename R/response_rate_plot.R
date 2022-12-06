#' Creates an interactive plot of response rates for single arm trials
#' @description
#' `response_rate_plot()` Creates an interactive plot of response rates for single arm trials
#' @param .data  A data frame downstream of the function `response_rate_df()`. Defaults to that data set, but allows for customization
#'
#' @return an interactive plot
#' @export
#'
response_rate_plot <- function(.data = skincancerRx::skincancerRx_data |> skincancerRx::response_rate_df()){

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
               size = 4) +
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
    #theme(legend.title = element_text(size = 12,
    #                                  face = "bold")) +
    #theme(legend.text = element_text(size = 10.3,
    #                                 face = "bold"))+
    #theme(legend.direction = "vertical",
    #      legend.box = "horizontal") +
    #guides(col = guide_legend(ncol = 4)) +
    annotate(geom = "text",
             x = min(dt$Action_Date_jitter) + 9900,
             y = 1.5,
             label = " = Lower Bound 95CI Proposed by the Sponsor",
             hjust = 0.5) +
    annotate("text",
             x = min(dt$Action_Date_jitter) + 8350,
             y = 1.55,
             label = "X",
             color="black",
             fill="black",
             size = 6) +
    annotate(geom = "point",
             x = min(dt$Action_Date_jitter) + 8350,
             y = 5.5,
             size = 4.2,
             alpha=0.8,
             color="black", fill="black") +
    annotate(geom = "text",
             x = min(dt$Action_Date_jitter) + 9950,
             y = 5.5,
             label = " = Response Rate in the Label-Modifying Trial(s)",
             hjust = 0.5)


  ggplotly(efficacy.plot, tooltip = c("text")) %>%
    layout(title = "<b>Efficacy Assessment in Non-Comparator Trials<b>",
           titlefont = list(size = 28, color = "black", family = "Arial")) %>%
    layout(showlegend = FALSE,
           xaxis = list(tickfont = list(size = 16)))  %>%
    layout(hoverlabel = list(font=list(size=20)))  %>%
    layout(margin = list(
      t =120,
      b = 0,
      r = 100,
      l = 0
    ))


}
