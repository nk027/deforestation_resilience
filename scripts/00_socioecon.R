
# Population and GDP per capita -----
library("dplyr")
library("readr")

# Estimates from 2001 to 2021, without 2007 and 2010 ---
d_pop0 <- read_csv("data/socioecon/population.csv", skip = 3, col_types = "icnnnnnnnnnnnnnnnnnnn") |>
  select(muni_id = `Cód.`, muni = Município, `2001`:`2021`) |>
  # There's a lot of additional information after the last row (Brasília)
  filter(row_number() <= which(muni_id == 5300108)) |>
  # Pull the state from the extended municipality name
  mutate(state = gsub(".* .([A-Z]{2}).$", "\\1", muni),
    muni = gsub("(^.*) .[A-Z]{2}.$", "\\1", muni)) |>
  tidyr::pivot_longer(`2001`:`2021`, names_to = "year", values_to = "population") |>
  mutate(year = as.integer(year))
d_pop07 <- read_csv("data/socioecon/population07.csv", skip = 3, col_types = "icn") |>
  select(muni_id = `Cód.`, muni = Município, `2007`) |>
  filter(row_number() <= which(muni_id == 5300108)) |>
  transmute(muni_id, state = gsub(".* .([A-Z]{2}).$", "\\1", muni),
    muni = gsub("(^.*) .[A-Z]{2}.$", "\\1", muni), year = 2007L, population = `2007`)
d_pop10 <- read_csv("data/socioecon/population10.csv", skip = 4, col_types = "icn") |>
  select(muni_id = `Cód.`, muni = Município, `Total`) |>
  filter(row_number() <= which(muni_id == 5300108)) |>
  transmute(muni_id, state = gsub(".* .([A-Z]{2}).$", "\\1", muni),
    muni = gsub("(^.*) .[A-Z]{2}.$", "\\1", muni), year = 2010L, population = `Total`)
# Merge all of them
d_pop <- rbind(d_pop0, d_pop07, d_pop10) |> arrange(state, muni, year)

# We need to fill Mojuí dos Campos from 2001:2011
m <- lm(population ~ year, data = d_pop |> filter(muni_id == 1504752))
d_pop <- d_pop |> rows_upsert(tibble(
  muni_id = 1504752L, muni = "Mojuí dos Campos", state = "PA", year = 2001:2011,
  population = predict(m, newdata = tibble(year = 2001:2011))), by = c("muni_id", "year"))
# And Ipiranga do Norte & Itanhangá in from 2001:2003
m <- lm(population ~ year, data = d_pop |> filter(muni_id == 5104526))
d_pop <- d_pop |> rows_upsert(tibble(muni_id = 5104526L, year = 2001:2003,
  population = predict(m, newdata = tibble(year = 2001:2003))), by = c("muni_id", "year"))
m <- lm(population ~ year, data = d_pop |> filter(muni_id == 5104542))
d_pop <- d_pop |> rows_upsert(tibble(muni_id = 5104542L, year = 2001:2003,
  population = predict(m, newdata = tibble(year = 2001:2003))), by = c("muni_id", "year"))

saveRDS(d_pop, "data/pop.rds")

# GDP ---
d_gdp <- read_csv("data/socioecon/gdp.csv", col_types = "icicccnnnnnnnn") |>
  dplyr::select(state = State, muni_id = `Municipality Code`, muni = Municipality, year = Year,
    gva_agric = `Gross Value Added of Agriculture at current prices (R$ 1000)`,
    gva_ind = `Gross Value Added of Industry at current prices (R$ 1000)`,
    gva_serv = `Gross Value Added of Sercives at current prices (R$ 1000) except Administration, defense, education and public health and social security`,
    gva_public = `Gross Value Added of Administration, defense, education and public health and social security at current prices (R$ 1000)`,
    gva_total = `Total Gross Added Value at current prices (R$ 1000)`,
    tax_total = `Taxes, net of subsidies on products at current prices (R$ 1000)`,
    gdp = `GDP at current prices (R$ 1000)`,
    gdp_pc = `GDP per capita at current prices (R$ 1,00)`)
# One municipality was renamed inbetween
d_gdp <- d_gdp |> mutate(muni = ifelse(muni_id == 1708254, "Tabocão", muni))

# Muni names in 2020 are different format, fix with hack
muni_names <- d_gdp |> 
  filter(year == 2019) |> 
  select(muni_id, muni)

d_gdp <- d_gdp |> 
  select(-muni) |> 
  left_join(muni_names, by = c("muni_id")) |> 
  relocate(muni, .after = muni_id)

# We need to fill Mojuí dos Campos from 2002:2012 (we use GPC/cap from its origin)
d_gdp <- d_gdp |> rows_upsert(tibble(muni_id = 1504752L,
  muni = "Mojuí dos Campos", state = "PA", year = 2002:2012,
  d_gdp |> filter(muni_id == 1506807, year %in% 2002:2012) |> dplyr::select(gdp_pc)),
  by = c("muni_id", "year"))
# And Ipiranga do Norte & Itanhangá in from 2002:2004
d_gdp <- d_gdp |> rows_upsert(tibble(muni_id = 5104526L,
  muni = "Ipiranga do Norte", state = "MT", year = 2001:2004,
  d_gdp |> filter(muni_id == 5104526L, year == 2005) |> dplyr::select(gdp_pc)),
  by = c("muni_id", "year"))
d_gdp <- d_gdp |> rows_upsert(tibble(muni_id = 5104542L,
  muni = "Itanhangá", state = "MT", year = 2001:2004,
  d_gdp |> filter(muni_id == 5104542L, year == 2005) |> dplyr::select(gdp_pc)),
  by = c("muni_id", "year"))


# We use national growth rates for 2021 to extrapolate at muni level
# Take values from World Bank, for gva_public we take gva_total
WDI_BRA <- read_csv("data/socioecon/API_BRA_DS2_en_csv_v2_4514539.csv", 
                    skip = 4)

WDI_BRA_2021 <- WDI_BRA |> 
  transmute(iso3c = `Country Code`, ind = `Indicator Code`, `2020`, `2021`) |> 
  filter(ind %in% c("NV.AGR.TOTL.CN", "NV.IND.TOTL.CN", "NV.SRV.TOTL.CN", 
                    "NY.GDP.FCST.CN", "NY.TAX.NIND.CN", 
                    "NY.GDP.MKTP.CN", "NY.GDP.PCAP.CN")) |> 
  transmute(iso3c, ind, g_rate = (`2021` - `2020`) / `2020`) |> 
  tidyr::pivot_wider(names_from = ind, values_from = g_rate) |> 
  transmute(gva_agric = NV.AGR.TOTL.CN, gva_ind = NV.IND.TOTL.CN, 
            gva_serv = NV.SRV.TOTL.CN, gva_public = NY.GDP.FCST.CN, 
            gva_total = NY.GDP.FCST.CN, tax_total = NY.TAX.NIND.CN, 
            gdp = NY.GDP.MKTP.CN, gdp_pc = NY.GDP.PCAP.CN) |> 
  tidyr::pivot_longer(gva_agric:gdp_pc) |> 
  rename(g_rate = value)
extra_soc <- d_gdp |> filter(year == 2020) |> 
  tidyr::pivot_longer(cols = gva_agric:gdp_pc) |> 
  left_join(WDI_BRA_2021, by = "name") |> 
  mutate(value = value * (1 + g_rate), 
         year = 2021) |> 
  select(-g_rate) |> 
  tidyr::pivot_wider(names_from = name, values_from = value)
d_gdp <- rbind(d_gdp, extra_soc)


saveRDS(d_gdp, "data/gdp.rds")


# GDP deflator ---
WDI_BRA_defl <- WDI_BRA |> 
  mutate(iso3c = `Country Code`, ind = `Indicator Code`) |> 
  select(iso3c, ind, `1995`:`2021`) |> 
  filter(ind == "NY.GDP.DEFL.ZS") |> 
  pivot_longer(cols = `1995`:`2021`) |> 
  rename(year = name, gdp_defl = value) |> 
  mutate(year = as.numeric(year)) |>  
  select(-ind)
  
saveRDS(WDI_BRA_defl, "data/gdp_defl.rds")
