
# Data on treatment by Soy Moratorium by Gollnow et al (2022) -----

library("dplyr")
library("readxl")
library("readr")
library("tidyr")


d_soy_moratorium <- read_delim("./data/SM_treatment/SM_treatment_gollnow.csv")

d_sm <- expand.grid(unique(d_soy_moratorium$muni_id), 
                    min(d_soy_moratorium$year_treated, na.rm = T):2021) |> 
  rename(muni_id = Var1, year = Var2) |> 
  arrange(muni_id, year) |> 
  left_join(d_soy_moratorium, by = "muni_id") |> 
  mutate(sm_pot = 1,
         sm_treat = 0,
         sm_treat = replace(sm_treat, year >= year_treated, 1)) |> 
  select(muni_id, year, sm_pot, sm_treat)


saveRDS(d_sm, "data/SM_treatment.rds")
