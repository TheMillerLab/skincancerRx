skincancerRx_data |> 
#  filter(Dz == "Melanoma") |> 
  fda_approval_timeliner_df() |> 
  fda_approval_timeliner_plot()

fda_approval_timeliner_plot()

fda_approval_timeseries_plot()

skincancerRx_data |> 
#  filter(Dz == "MCC") |> 
  fda_approval_timeseries_df() |> 
  fda_approval_timeseries_plot()

skincancerRx_data |> 
  filter(Dz == "MCC") |> 
  fda_approval_timeseries_df() |> 
  fda_approval_timeseries_plot(.startdate = "2016-01-01")

