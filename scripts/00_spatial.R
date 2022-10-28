
# Spatial data -----
library("sf")
library("dplyr")

states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
            "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
            "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
            "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
            "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
            "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
            "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")
ufs <- structure(names(states), names = states)
legal_amazon <- c("AC", "AP", "AM",  "MA", "MT", "PA", "RO", "RR", "TO")


# Plain shapefile ---
sh <- read_sf("data/shp") |>
  transmute(muni_id = as.integer(CD_MUN), muni = NM_MUN, state = SIGLA, area_km2 = AREA_KM2)

saveRDS(sh |> st_drop_geometry(), "data/spatial.rds")

# Protected areas ---
pa <- rbind(read_sf("data/protected_areas/WDPA_WDOECM_Sep2022_Public_BRA_shp_0"),
            read_sf("data/protected_areas/WDPA_WDOECM_Sep2022_Public_BRA_shp_1"),
            read_sf("data/protected_areas/WDPA_WDOECM_Sep2022_Public_BRA_shp_2")) |>
  filter(STATUS != "Proposed", MARINE == "0") |>
  transmute(name = NAME, designation = DESIG_ENG, type = GOV_TYPE,
            year = as.integer(pmax(1926, STATUS_YR)), pa_area_km2 = REP_AREA) |> # One's designated by Jesus
  st_transform(st_crs(sh))

pa_amazon <- pa |> st_intersection(sh |> filter(state %in% legal_amazon) |> st_bbox() |> st_as_sfc())

pa_int <- sh |> filter(state %in% legal_amazon) |> st_intersection(pa_amazon)

saveRDS(pa_int, "data/pa_raw.rds")
pa_int <- readRDS("data/pa_raw.rds")

pa_int <- pa_int |> rename(pa_total_area_km2 = pa_area_km2) |>
  mutate(pa_area_km2 = as.numeric(st_area(pa_int) / 1e6), 
         pa_area_km2 = ifelse(pa_area_km2 > area_km2, area_km2, pa_area_km2)) |>
  rename(pa_year = year, pa_type = type, pa_designation = designation, pa_name = name)


saveRDS(pa_int, "data/pa.rds")
pa_int <- readRDS("data/pa.rds")


pa_df <- pa_int |> 
  st_drop_geometry() |> 
  select(-pa_name, -pa_designation, -pa_total_area_km2) |> 
  group_by(muni_id, muni, state, area_km2, pa_type, pa_year) |> 
  summarise(pa_area_km2 = sum(pa_area_km2, na.rm = TRUE)) |> 
  tidyr::pivot_wider(names_from = pa_type, values_from = pa_area_km2) |>
  rename(pa_fed_km2 = `Federal or national ministry or agency`, 
         pa_ind_km2 = `Indigenous peoples`, 
         pa_oth_km2 = `Not Reported`, 
         pa_sub_km2 = `Sub-national ministry or agency`, 
         pa_loc_km2 = `Local communities`) |> 
  ungroup()


pa_int <- pa_int |> 
  arrange(muni_id, pa_year)
pa_tot <- c()
for(m_id in unique(pa_int$muni_id)) {
  temp <- pa_int |> 
    filter(muni_id == m_id)
  if(nrow(temp) > 1) {
    for(yy in 2:nrow(temp)) {
      temp[yy, "geometry"] <- st_make_valid(st_set_precision(
        st_union(temp[yy - 1, "geometry"], temp[yy, "geometry"]), 
        1e6))
    }  
  }
  temp <- temp |> 
    mutate(pa_area_km2 = as.numeric(st_area(temp) / 1e6)) |> 
    st_drop_geometry()
  pa_tot <- rbind(pa_tot, temp)
}

pa_tot <- pa_tot |> 
  arrange(muni_id, pa_year, pa_area_km2) |> 
  group_by(muni_id, muni, state, area_km2, pa_year) |> 
  slice_tail(n = 1) |> 
  ungroup() |> 
  transmute(muni_id, muni, state, pa_year,
            pa_tot_km2 =  ifelse(pa_area_km2 > area_km2, area_km2, pa_area_km2)) 

pa_df <- left_join(pa_df, pa_tot, by = c("muni_id", "muni", "state", "pa_year"))

muni_grid <- expand.grid("muni_id" = unique(sh$muni_id), "pa_year" = min(pa_df$pa_year):2020) |> 
  left_join(sh |> st_drop_geometry() |> select(-area_km2), by = "muni_id") |> 
  relocate(pa_year, .after = state)

pa_df <- left_join(muni_grid, pa_df |> select(-muni, -state, -area_km2), by = c("muni_id", "pa_year")) |> 
  tidyr::replace_na(list(pa_fed_km2 = 0, pa_ind_km2 = 0, pa_oth_km2 = 0, 
                         pa_sub_km2 = 0, pa_loc_km2 = 0)) |> 
  arrange(muni_id, pa_year) |> 
  group_by(muni_id) |> 
  mutate(across(pa_fed_km2:pa_loc_km2, ~cumsum(.x)),
         pa_tot_km2 = zoo::na.locf(pa_tot_km2, na.rm = FALSE)) |> 
  tidyr::replace_na(list(pa_tot_km2 = 0)) |> 
  ungroup() |> 
  rename(year = pa_year)

saveRDS(pa_df, "data/pa_df.rds")
pa_df <- readRDS("data/pa_df.rds")
