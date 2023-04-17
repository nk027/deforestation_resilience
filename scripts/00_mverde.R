
# Data on inclusion in municipo verde program -----

library("dplyr")
library("readxl")
library("readr")
library("tidyr")


d_mverde <- read_delim("./data/municipio_verde/municipio_verde_list.csv",  
                       locale = locale(encoding = "windows-1252"))
d_mverde$NOME <- stringr::str_to_title(gsub("[\r\n]", " ", d_mverde$NOME))
names(d_mverde) <- c("state", "muni", "year_mv_entry", "year_mv_exit")


d_mv <- expand.grid(unique(d_mverde$muni), 
                           min(d_mverde$year_mv_entry, na.rm = T):max(d_mverde$year_mv_entry, na.rm = T)) |> 
  rename(muni = Var1, year = Var2) |> 
  arrange(muni, year) |> 
  left_join(d_mverde, by = "muni") |> 
  tidyr::replace_na(list(year_mv_exit = Inf)) |> 
  relocate(state, .before = muni) |> 
  mutate(mverde = 0, 
         mverde = replace(mverde, year >= year_mv_entry, 1), 
         mverde = replace(mverde, year >= year_mv_exit, 0)) |> 
  select(state, muni, year, mverde)



saveRDS(d_mv, "data/mverde_participation.rds")
