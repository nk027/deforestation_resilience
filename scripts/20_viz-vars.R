
# Maps on forest loss and fines ---
library("sf")
library("dplyr")
library("ggplot2")
library("tmap")

states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
  "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
  "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
  "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
  "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
  "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
  "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")
ufs <- structure(names(states), names = states)
legal_amazon <- c("AC", "AP", "AM",  "MA", "MT", "PA", "RO", "RR", "TO")


sh <- readRDS("data/df_spatial.rds")
sh <- sh |> filter(state %in% legal_amazon, year > 2002) |>
  mutate(forest_loss_pct = forest_loss / area_ha)

t <- sh |> # filter(year %in% c(2011:2021)) |>
  rename(`Forest loss` = forest_loss) |>
  tm_shape() +
  tm_fill("Forest loss", style = "fixed", palette = "viridis", showNA = FALSE,
    breaks = c(0, 10000, 25000, 50000, 100000, 150000, 200000)) +
  tm_polygons(col = "#eeeeee") +
  tm_facets(by = "year", free.coords = FALSE) +
  tm_layout(frame = FALSE, fontfamily = "Noto Sans",
    outer.bg.color = "transparent", bg.color = "transparent",
    inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins = c(0, 0, 0, 0),
    legend.position = c("left", "bottom"), legend.frame = FALSE,
    legend.text.size = .8, legend.title.size = 1.2,
    panel.label.size = 1.2, panel.label.height = 1)

tmap_save(t, "outputs/forest_loss.png", device = png, width = 10, height = 10)


t <- sh |> # filter(year %in% c(2011:2021)) |>
  rename(`Fine number` = n_fined) |>
  tm_shape() +
  tm_fill("Fine number", style = "fixed", palette = "viridis", showNA = FALSE,
    breaks = c(0, 1, 10, 50, 100, 200, 1000)) +
  tm_polygons(col = "#eeeeee") +
  tm_facets(by = "year", free.coords = FALSE) +
  tm_layout(frame = FALSE, fontfamily = "Noto Sans",
    outer.bg.color = "transparent", bg.color = "transparent",
    inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins = c(0, 0, 0, 0),
    legend.position = c("left", "bottom"), legend.frame = FALSE,
    legend.text.size = .8, legend.title.size = 1.2,
    panel.label.size = 1.2, panel.label.height = 1)

tmap_save(t, "outputs/fine_count.png", device = png, width = 10, height = 10)
