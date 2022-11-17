
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

d <- d |> arrange(year, state, muni)

dd <- d |> group_by(muni_id) |> mutate(
  forest_loss_lag = lag(forest_loss, order_by = year),
  forest_loss_lag2 = lag(forest_loss, 2, order_by = year),
  n_fined_lag = lag(n_fined, order_by = year),
  brl_fined_lag = lag(brl_fined, order_by = year),
  brl_paid_lag = lag(brl_paid, order_by = year),
  n_fined_lead = lead(n_fined, order_by = year),
  brl_fined_lead = lead(brl_fined, order_by = year),
  brl_paid_lead = lead(brl_paid, order_by = year)
) |> mutate(
  area_ha = area_km2 * 100,
  forest_loss = ifelse(forest_loss == 0, NA, forest_loss),
  intensity_v = brl_fined / pmax(1, forest_loss),
  intensity_n = n_fined / pmax(1, forest_loss),
  intensity_v_wo_canc = (brl_fined - brl_cancelled) / pmax(1, forest_loss),
  intensity_n = (n_fined - n_cancelled) / pmax(1, forest_loss),
  intensity_v_wo_out = (brl_fined - brl_outlier_cancelled) / pmax(1, forest_loss),
  intensity_n = (n_fined - n_outlier_cancelled) / pmax(1, forest_loss)
)

saveRDS(dd |> st_drop_geometry(), "data/df_merged.rds") # Small sized
# saveRDS(dd, "data/df_spatial.rds") # Large, used for maps
