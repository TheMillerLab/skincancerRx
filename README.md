<img src='data-raw/skincancerRx_bubble.png' align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

## Overview  
<font color = 'orange' ><b>skincanerRx</b></font> an R package designed to visualize FDA approvals for skin cancer. The data used in this package was first published as a preprint at [themillerlab.io](https://www.themillerlab.io/publication/impact_of_an_evolving_regulatory_landscape_on_skin_cancer_drug_devlopment/) and subsequently accepted at [Dermatology Online Journal](https://escholarship.org/uc/item/9j53k98k). In addition, the functions in skincancerRx power the [FDA Approvals in Skin Cancer](https://themillerlab.shinyapps.io/fda_approvals_in_cutaneous_oncology/) shiny app. 

<font color = 'orange' ><b>skincancerRx</b></font> provides a set of verbs that wrangle, process and graph data related to FDA approvals in skin caner:  

  | Verbs | Function |
  | :-------- |    :-----   |
  |fda_actions_per_disease_plot( )  | generates a bar chart of the number of FDA approvals per type of skin cancer |
  |fda_approval_timeliner_df( ) | creates a data frame of FDA approval data that can then be used in the `fda_approval_timeliner_plot()` function |
  |fda_approval_timeliner_plot( ) | creates a data visualization of FDA approvals in skin cancer |
  |fda_approval_timeseries_df( ) | creates a data frame of FDA approval data that can then be used in the `fda_approval_timeseries_plot()` function |
  |fda_approval_timeseries_plot( ) | creates a data visualization of FDA approvals in skin cancer |
  |response_rate_df( ) | creates a data frame of FDA approval data that can then be used in the `response_rate_plot()` function |
  |response_rate_plot( ) | creates a data visualization of response rates of therapies approved via non-comparator trials |
  |table_rx_skin_cancer( ) | creates a table of FDA approvals in skin cancer |

## Software Dependencies
<font color = 'orange' ><b>skincancerRx</b></font> is written in R (version 4.0.0), organized using roxygen2, and utilizes the following packages dplyr, tidyr, readr, stringr, magrittr, plotly, splitstackshape.

## Data Sources  
To evaluate the evidence used to support labeled claims in skin cancer we reviewed FDA New Drug Application (NDA) or Biological License Application (BLA) reviews, and the US product labels, that are indexed on the FDA website (https://www.accessdata.fda.gov/scripts/cder/daf/index.cfm) . In addition to the product labels and NDA and BLA reviews, data were also obtained from OpenFDA (https://download.open.fda.gov/drug/drugsfda/drug-drugsfda-0001-of-0001.json.zip).

## Installation

### Development version

To get a bug fix or to use a feature from the development version, you can install 
the development version of <font color = 'orange' ><b>skincancerRx</b></font> from GitHub.

`devtools::install_github("TheMillerLab/skincancerRx")`


## Usage
`library(skincancerRx)`

## Getting help
If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/TheMillerLab/skincancerRx/issues).

## Disclaimer and Acknowledgements
<font color = 'orange' ><b>skincancerRx</b></font> is for research purposes only. No clinical decisions should be made with the information obtained from its output.
