
# Agriculture -----
library("dplyr")
library("readr")
library("tidyr")

# There's four tables stacked on top of each other (added a name for the last column) ---
d_soy <- readr::read_csv("data/agriculture/soy_production.csv", skip = 3)
# The format is bothersome, this works
colnames(d_soy) <- c("muni_id", "muni", paste0(rep(c("total_", "soy_"), 22),
  colnames(d_soy)[unlist(lapply(seq(3, 45, by = 2), \(x) rep(x, 2)))]))
# Split off the state from the extended municipality name
d_soy <- d_soy |> mutate(state = gsub(".* .([A-Z]{2}).$", "\\1", muni),
  muni = gsub("(^.*) .[A-Z]{2}.$", "\\1", muni))

# These are points of interest, as they indicate one of the four talbes
poi <- which(grepl("Tabela 5457", d_soy$muni_id))
d_soy <- d_soy |> mutate(muni_id = as.integer(muni_id)) # Now we can change this

# Split off the four tables ---
# Planted areas
d_soy_pla <- d_soy[seq(2, poi[1] - 2), ] |>
  tidyr::pivot_longer(total_2000:soy_2021) |>
  transmute(muni_id, muni, state, crop = gsub("(^.*)_[0-9]*$", "\\1", name),
    year = as.integer(gsub("^.*_([0-9]*)$", "\\1", name)),
    value = as.numeric(gsub("-", "0", value))) |>
  tidyr::replace_na(list(value = 0))
# Harvested areas
d_soy_har <- d_soy[seq(poi[1] + 5, poi[2] - 2), ] |>
  tidyr::pivot_longer(total_2000:soy_2021) |>
  transmute(muni_id, muni, state, crop = gsub("(^.*)_[0-9]*$", "\\1", name),
    year = as.integer(gsub("^.*_([0-9]*)$", "\\1", name)),
    value = as.numeric(gsub("-", "0", value))) |>
  tidyr::replace_na(list(value = 0))
# Tons of production
d_soy_ton <- d_soy[seq(poi[2] + 5, poi[3] - 2), ] |>
  tidyr::pivot_longer(total_2000:soy_2021) |>
  transmute(muni_id, muni, state, crop = gsub("(^.*)_[0-9]*$", "\\1", name),
    year = as.integer(gsub("^.*_([0-9]*)$", "\\1", name)),
    value = as.numeric(gsub("-", "0", value))) |>
  tidyr::replace_na(list(value = 0))
# BRL of production
d_soy_brl <- d_soy[seq(poi[3] + 5, 22270), ] |>
  tidyr::pivot_longer(total_2000:soy_2021) |>
  transmute(muni_id, muni, state, crop = gsub("(^.*)_[0-9]*$", "\\1", name),
    year = as.integer(gsub("^.*_([0-9]*)$", "\\1", name)),
    value = as.numeric(gsub("-", "0", value))) |>
  tidyr::replace_na(list(value = 0))

saveRDS(d_soy_pla, "data/soy_planted.rds")
saveRDS(d_soy_har, "data/soy_harvested.rds")
saveRDS(d_soy_ton, "data/soy_tons.rds")
saveRDS(d_soy_brl, "data/soy_brl.rds")

# The columns are cleaned manually (just pasted cattle and the year together) ---
d_cattle <- read_csv("data/agriculture/cattle_herds.csv", col_types = "icnnnnnnnnnnnnnnnnnnnnnn") |>
  select(muni_id = code, muni = municipio, cattle_2000:cattle_2021) |>
  tidyr::pivot_longer(cattle_2000:cattle_2021) |>
  tidyr::replace_na(list(value = 0)) |>
  filter(!is.na(muni)) |> 
  transmute(muni_id, state = gsub(".* .([A-Z]{2}).$", "\\1", muni),
    muni = gsub("(^.*) .[A-Z]{2}.$", "\\1", muni),
    year = as.integer(gsub(".*_([0-9]{4})$", "\\1", name)), cattle = value)

saveRDS(d_cattle, "data/cattle_head.rds")
