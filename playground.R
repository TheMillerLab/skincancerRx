skincancerRx_data |> 
#  filter(Dz == "Melanoma") |> 
  fda_approval_timeliner_df() |> 
  fda_approval_timeliner_plot()

fda_approval_timeliner_plot(
  .geomtextsize = 2,
  .legendtextsize = 2,
  .hoverlabeltextsize = 10
)

fda_approval_timeseries_plot(
  .legendorientationy = 1.2
)

skincancerRx_data |> 
  filter(Dz == "MCC") |> 
  fda_approval_timeseries_df() |> 
  fda_approval_timeseries_plot()

skincancerRx_data |> 
  filter(Dz == "MCC") |> 
  fda_approval_timeseries_df() |> 
  fda_approval_timeseries_plot(.startdate = "2016-01-01")


response_rate_plot(
 # .xlabelpositionX = "1999-01-01",
  #.xlabelpositionY = 100
)

library(skincancerRx)

skincancerRx::fda_actions_per_disease_plot()
