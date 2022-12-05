## code to prepare `FDA-cOnc Data` simulated dataset goes here
skincancerRx_data <- readr::read_csv("data-raw/FDA_cONC.csv")

usethis::use_data(skincancerRx_data, overwrite = TRUE)
