
# Data on land cover and transitions -----
library("dplyr")
library("readxl")
library("readr")
library("tidyr")

# Land cover in hectare ---
d_luc <- readxl::read_excel("data/land_use/land-cover-municipio_collection7.xlsx",
                            sheet = "COBERTURA_COL7") |>
  select(id = feature_id, muni = municipality, state = UF, biome, class_id,
         class = level_2, subclass = level_3, color, `2000`:`2021`) |>
  tidyr::pivot_longer(cols = `2000`:`2021`, names_to = "year") |>
  mutate(year = as.integer(year))

# Land transitions in hectare ---
# XLS is inefficient in terms of RAM, so I export the sheet manually to CSV instead
d_lut <- readr::read_csv("data/land_use/land-cover-municipio_collection7.csv",
                         col_types = "i---iccccccii--------------------------n---n-n-nnn-nnn--nn---nn-nnn-n-nnnn") |>
  transmute(id = feature_id, class_id, class = as.factor(level_2),
            subclass = as.factor(level_3), color = as.factor(color),
            from = from_class, to = to_class, `2000-2001`, `2001-2002`, `2002-2003`,
            `2003-2004`, `2004-2005`, `2005-2006`, `2006-2007`, `2007-2008`, `2008-2009`,
            `2009-2010`, `2010-2011`, `2011-2012`, `2012-2013`, `2013-2014`, `2014-2015`,
            `2015-2016`, `2016-2017`, `2017-2018`, `2018-2019`, `2019-2020`, `2020-2021`) |>
  tidyr::pivot_longer(cols = `2000-2001`:`2020-2021`, names_to = "year") |>
  mutate(year = as.integer(substr(year, 6, 9)))

# Prepare to merge the cover and change and get to a wide format ---
d_luc <- d_luc |> filter(subclass %in% c("Forest Loss", "Forest Formation", "Pasture", # Keep these
                                         "Temporary Crops", "Perennial Crops", "Urban Infrastructure", "Mining", "Forest Plantation")) |>
  select(id, muni, state, biome, year, subclass, value) |>
  tidyr::pivot_wider(names_from = subclass, values_from = value,
                     values_fn = \(x) sum(x, na.rm = TRUE), values_fill = 0) |>
  transmute(id, muni, state, biome, year,
            forest = `Forest Formation`, forestry = `Forest Plantation`,
            croplands = `Temporary Crops` + `Perennial Crops`, pasture = `Pasture`,
            urban = `Urban Infrastructure`, mining = `Mining`)
d_lut <- d_lut |> filter(from == 3, to != 3) |>  group_by(id, year) |>
  summarise(forest_loss = sum(value, na.rm = TRUE))
d_lucc <- d_luc |> left_join(d_lut, by = c("year", "id")) |>
  tidyr::replace_na(list(forest_loss = 0))

# We care about the Amazon and Cerrado (and a bit of Pantanal) ---
d_lucc <- d_lucc |> mutate(biome = substr(tolower(biome), 1, 3)) |>
  filter(biome %in% c("ama", "cer", "pan"))

# Now we want one row per municipality & year, first ignoring biomes
d_lucc_all <- d_lucc |> group_by(muni, state, year) |>
  summarise(across(forest:forest_loss, sum, na.rm = TRUE))

# Next, we keep the Amazon specific columns
d_lucc_biomes <- d_lucc |> select(-id) |> filter(biome == "ama") |>
  tidyr::pivot_wider(names_from = biome, values_from = forest:forest_loss,
                     values_fn = \(x) sum(x, na.rm = TRUE), values_fill = 0)

d_lucc <- d_lucc_all |> left_join(d_lucc_biomes, by = c("muni", "state", "year")) |>
  mutate(across(forest_ama:forest_loss_ama, ~ ifelse(is.na(.), 0, .)))

saveRDS(d_lucc, "data/land_use_change.rds")
d_lucc <- readRDS("data/land_use_change.rds")
