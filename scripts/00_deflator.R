
# Data for GDP deflator for fines -----

library("dplyr")

WDI_BRA <- readr::read_csv("data/API_BRA_DS2_en_csv_v2_4514539.csv", 
                           skip = 4)

WDI_BRA_defl <- WDI_BRA |> 
  mutate(iso3c = `Country Code`, ind = `Indicator Code`) |> 
  select(iso3c, ind, `1995`:`2021`) |> 
  filter(ind == "NY.GDP.DEFL.ZS") |> 
  tidyr::pivot_longer(cols = `1995`:`2021`) |> 
  rename(year = name, gdp_defl = value) |> 
  mutate(year = as.numeric(year)) |>  
  select(-ind)

saveRDS(WDI_BRA_defl, "data/gdp_defl.rds")
