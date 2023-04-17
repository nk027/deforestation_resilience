# Merge the data to use in visualizations ---

library("sf")
library("dplyr")

d <- readRDS("data/land_use_change.rds") |>
  full_join(readRDS("data/spatial.rds"), by = c("muni", "state"))
d <- d |> st_set_geometry(d$geom)

d <- d |> left_join(readRDS("data/fines.rds"), by = c("muni", "state", "year")) |>
  tidyr::replace_na(list(n_fined = 0, brl_fined = 0, n_payments = 0, brl_paid = 0,
    n_cancelled = 0, brl_cancelled = 0, n_outlier_cancelled = 0, brl_outlier_cancelled = 0))

d <- d |> left_join(readRDS("data/pa_df.rds") |>
  filter(year >= 2000), by = c("year", "state", "muni_id", "muni"))

d <- d |> left_join(readRDS("data/gdp.rds"), by = c("year", "state", "muni_id", "muni"))

d <- d |> left_join(readRDS("data/pop.rds"), by = c("year", "state", "muni_id", "muni"))

d <- d |> left_join(readRDS("data/cattle_head.rds") |> select(muni_id, year, cattle), 
                    by = c("year", "muni_id"))

d <- d |> left_join(readRDS("data/soy_tons.rds") |> filter(crop == "soy") |>
                      transmute(muni_id, muni, state, year, soy_ton = value),
                    by = c("year", "state", "muni_id", "muni")) |>
  tidyr::replace_na(list(soy_ton = 0))

d <- d |> left_join(readRDS("data/soy_brl.rds") |> filter(crop == "soy") |>
                      transmute(muni_id, muni, state, year, soy_brl = value),
                    by = c("year", "state", "muni_id", "muni")) |>
  tidyr::replace_na(list(soy_brl = 0))

d <- d |> left_join(readRDS("data/spei.rds") |> group_by(muni_id, year) |>
                      summarise(spei_dry = ifelse(min(value) < -2, 1, 0), spei_wet = ifelse(max(value) > 2, 1, 0)),
                    by = c("year", "muni_id")) |> 
  tidyr::replace_na(list(spei_dry = 0, spei_wet = 0))

d <- d |> left_join(readRDS("data/gdp_defl.rds") |> transmute(year, gdp_defl = gdp_defl / 100), 
                    by = "year")

d <- d |> mutate(muni_temp = tolower(muni)) |> 
  left_join(readRDS("data/mverde_participation.rds") |> 
              mutate(muni_temp = tolower(muni)) |> 
              select(-muni), 
            by = c("state", "muni_temp", "year")) |> 
  tidyr::replace_na(list(mverde = 0)) |> 
  select(-muni_temp)

d <- d |> 
  left_join(readRDS("data/SM_treatment.rds"), by = c("muni_id", "year")) |> 
  tidyr::replace_na(list(sm_pot = 0, sm_treat = 0))

d <- d |> arrange(year, state, muni)

dd <- d |> group_by(muni_id) |> mutate(
  gdp = gdp / gdp_defl,
  gdp_pc = gdp_pc / gdp_defl, 
  soy_brl = soy_brl / gdp_defl, 
  brl_fined = brl_fined / gdp_defl, 
  brl_paid = brl_paid / gdp_defl, 
  brl_cancelled = brl_cancelled / gdp_defl, 
  brl_outlier_cancelled = brl_outlier_cancelled / gdp_defl, 
  gva_agric = gva_agric / gdp_defl, 
  gva_ind = gva_ind / gdp_defl, 
  gva_serv = gva_serv / gdp_defl, 
  gva_public = gva_public / gdp_defl, 
  gva_total = gva_total / gdp_defl, 
  tax_total = tax_total / gdp_defl
) |> mutate(
  area_ha = area_km2 * 100,
  forest_loss = ifelse(forest_loss == 0, NA, forest_loss),
  forest_init = forest[year == 2003],
  soy_price = ifelse(soy_ton == 0, 0, soy_brl / soy_ton),
  n_fined_wo_out = n_fined - n_outlier_cancelled, 
  brl_fined_wo_out = brl_fined - brl_outlier_cancelled,
  intensity_v = brl_fined / pmax(1, forest_loss),
  intensity_n = n_fined / pmax(1, forest_loss),
  intensity_v_wo_canc = (brl_fined - brl_cancelled) / pmax(1, forest_loss),
  intensity_n_wo_canc = (n_fined - n_cancelled) / pmax(1, forest_loss),
  intensity_v_wo_out = (brl_fined - brl_outlier_cancelled) / pmax(1, forest_loss),
  intensity_n_wo_out = (n_fined - n_outlier_cancelled) / pmax(1, forest_loss)
)

saveRDS(dd |> st_drop_geometry(), "data/df_merged.rds") # Small sized
# saveRDS(dd, "data/df_spatial.rds") # Large, used for maps
