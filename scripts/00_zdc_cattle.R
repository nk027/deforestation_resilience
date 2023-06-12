
# Data on treatment by ZDC cattle agreement by Levy et al (2022) -----
# taken from https://zenodo.org/record/5105746

library("dplyr")
library("readxl")
library("readr")
library("tidyr")

d_zdc_cattle <- read_delim("./data/ZDC_cattle/Levyetal2023_marketsharefiguredata.csv")

d_zdc_cattle <- d_zdc_cattle |> 
  transmute(muni_id = code_ibge, year, g4_ms, tac_ms, zdc_ms = g4_ms + tac_ms)

grid_zdc <- expand.grid(unique(d_zdc_cattle$muni_id), 
                        min(d_zdc_cattle$year, na.rm = T):2021) |> 
  rename(muni_id = Var1, year = Var2) |> 
  arrange(muni_id, year)

# Data extended to 2021 by assuming constant G4, TAC share after 2018
d_zdc_cattle <- grid_zdc |> 
  left_join(d_zdc_cattle, by = c("muni_id", "year")) |> 
  group_by(muni_id) |> 
  mutate(g4_ms = replace(g4_ms, year > 2018, g4_ms[year == 2018]), 
         tac_ms = replace(tac_ms, year > 2018, tac_ms[year == 2018]), 
         zdc_ms = replace(zdc_ms, year > 2018, zdc_ms[year == 2018])) |> 
  ungroup()

saveRDS(d_zdc_cattle, "data/ZDC_cattle.rds")
