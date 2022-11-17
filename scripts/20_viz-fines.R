
# Visualise the fines ---
library("sf")
library("dplyr")
library("ggplot2")

states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
  "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
  "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
  "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
  "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
  "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
  "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")
ufs <- structure(names(states), names = states)
legal_amazon <- c("AC", "AP", "AM",  "MA", "MT", "PA", "RO", "RR", "TO")

d_fines <- readRDS("data/fines_raw.rds")
d_fines <- d_fines |>
  filter(state %in% legal_amazon) |>
  mutate(
    year_fined = as.integer(format(date_fined, "%Y")),
    year_paid = as.integer(format(date_paid, "%Y")),
    month_year_fined = format(date_fined, "%m/%Y"),
    month_year_paid = format(date_paid, "%m/%Y"))


# Summaries -----

# In which year were fines handed out that received payments in 2022? ---
# Depending on specification between 2009 and 2010 or 2010 and 2011, we use 2010
d_fines |> filter(year_paid == 2022, value_fined != 0) |>
  group_by(year_fined) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(share = prop.table(n), share_sum = cumsum(share)) |>
  print(n = 30)
d_fines |> filter(year_paid == 2022) |>
  group_by(id_ai) |>
  slice_head(n = 1) |>
  group_by(year_fined) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(share = prop.table(n), share_sum = cumsum(share)) |>
  print(n = 30)

# How many of the fines were paid since 2019? ---

# Received at least one payment: ~2%
d_fines |> filter(year_fined > 2018, value_fined != 0,
  !(id_ai == "9099708 - E" | (name %in%
    c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
    muni == "Porto Velho"))) |>
  summarise(count = n())
d_fines |> filter(year_fined > 2018, value_fined != 0, value_paid != 0,
  !(id_ai == "9099708 - E" | (name %in%
    c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
    muni == "Porto Velho"))) |>
  summarise(count = n())

# Approved but not necessarily received payment yet: ~2.5%
d_fines |> filter(year_fined > 2018, value_fined != 0,
  !(id_ai == "9099708 - E" | (name %in%
    c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
    muni == "Porto Velho"))) |>
  group_by(status_cat) |>
  summarise(n = n()) |>
  arrange(-n) |>
  ungroup() |>
  mutate(share = prop.table(n))

# Value paid in relation to fined value: 0.03%
d_fines |> filter(year_fined > 2018) |> filter(!(id_ai == "9099708 - E" | (name %in%
  c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
  muni == "Porto Velho"))) |>
  summarise(fined = sum(value_fined, na.rm = TRUE), paid = sum(value_paid, na.rm = TRUE)) |>
  mutate(share = paid / fined)


# Plots -----

# Plots on forest loss, fines and their values, and related intensities ---
df_merged <- readRDS("data/df_merged.rds") |> st_drop_geometry()

df_plot_int <- df_merged |> filter(state %in% legal_amazon, year > 2000) |>
  select(year, state, muni, forest_loss, brl_fined, brl_cancelled,
    brl_outlier_cancelled, n_fined, n_cancelled, n_outlier_cancelled) |>
  group_by(year) |>
  summarise(
    forest_loss = sum(forest_loss, na.rm = TRUE),
    brl_fined = sum(brl_fined, na.rm = TRUE),
    brl_cancelled = sum(brl_cancelled, na.rm = TRUE),
    brl_outlier_cancelled = sum(brl_outlier_cancelled, na.rm = TRUE),
    n_fined = sum(n_fined, na.rm = TRUE),
    n_cancelled = sum(n_cancelled, na.rm = TRUE),
    n_outlier_cancelled = sum(n_outlier_cancelled, na.rm = TRUE)) |>
  transmute(year,
    forest_loss = replace(forest_loss, forest_loss == 0, NA),
    brl_fined, n_fined,
    brl_fined_canc = brl_fined - brl_cancelled,
    brl_fined_out_canc = brl_fined - brl_outlier_cancelled,
    n_fined_canc = n_fined - n_cancelled,
    n_fined_out_canc = n_fined - n_outlier_cancelled,
    int_v = brl_fined / pmax(1, forest_loss),
    int_v_canc = (brl_fined - brl_cancelled) / pmax(1, forest_loss),
    int_v_out_canc = (brl_fined - brl_outlier_cancelled) / pmax(1, forest_loss),
    int_n = n_fined / pmax(1, forest_loss),
    int_n_canc = (n_fined - n_cancelled) / pmax(1, forest_loss),
    int_n_out_canc = (n_fined - n_outlier_cancelled) / pmax(1, forest_loss))

yoi <- c(2003, 2011, 2016, 2019) # years to mark on x-axis

# Figure for the main text ---
cairo_pdf("outputs/fine_n_intensity_separate.pdf", height = 8, width = 8, pointsize = 16, family = "Noto Sans")
op <- par(mar = c(0, 3, 2, 0), family = "Noto Sans")

par(fig = c(0, 1, .7, 1)) # First plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[2]] / 1e3, na.rm = TRUE))))
poi <- c(0, min(df_plot_int[[2]] / 1e3, na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[2]] / 1e3, na.rm = TRUE), max(df_plot_int[[2]] / 1e3, na.rm = TRUE))
axis(2, cex.axis = 1, at = poi, labels = round(poi), las = 1)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
# Barplot
rect(df_plot_int[[1]] - .35, 0, df_plot_int[[1]] + .35, df_plot_int[[2]] / 1e3, col = "#008040")
title("Forest loss (1,000 ha)", cex.main = 1.1, family = "Merriweather Black")

par(fig = c(0, 1, .4, .7), new = TRUE) # Second plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[8]], na.rm = TRUE))))
poi <- c(0, min(df_plot_int[[8]], na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[8]], na.rm = TRUE), max(df_plot_int[[8]], na.rm = TRUE))
axis(2, cex.axis = 1, at = poi, round(poi), las = 1)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
# Barplot
rect(df_plot_int[[1]] - .35, 0, df_plot_int[[1]] + .35, df_plot_int[[8]], col = "#004080")
title("Environmental fines (#)", cex.main = 1.1, family = "Merriweather Black")

par(fig = c(0, 1, 0, .4), new = TRUE, mar = c(2, 3, 2, 0)) # Third plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, 1.1 * (max(df_plot_int[[14]] * 1e3, na.rm = TRUE))))
axis(1, at = yoi, labels = TRUE, cex.axis = 1, pos = 0)
poi <- c(0, min(df_plot_int[[14]] * 1e3, na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[14]] * 1e3, na.rm = TRUE), max(df_plot_int[[14]] * 1e3, na.rm = TRUE))
axis(2, cex.axis = 1, at = poi, labels = round(poi, 2), las = 1)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
# Line chart
lines(df_plot_int[[1]], df_plot_int[[14]] * 1e3, lty = 1, lwd = 3, col = "#008080")
points(df_plot_int[[1]], df_plot_int[[14]] * 1e3, pch = 16, lwd = 1.5, cex = 1, col = "#006060")
# Add labels to the x-axis
text(2007, .25, labels = "Lula", cex = .8, col = "#000000")
arrows(x0 = 2003.25, x1 = 2010.75, y0 = 0, y1 = 0, length = 0.1)
text(2013.5, .25, labels = "Rousseff", cex = .8, col = "#000000")
arrows(x0 = 2011.25, x1 = 2015.75, y0 = 0, y1 = 0, length = 0.1)
text(2017.5, .25, labels = "Temer", cex = .8, col = "#000000")
arrows(x0 = 2016.25, x1 = 2018.75, y0 = 0, y1 = 0, length = 0.1)
text(2020.5, .25, labels = "Bolsonaro", cex = .8, col = "#000000")
arrows(x0 = 2019.25, x1 = 2023, y0 = 0, y1 = 0, length = 0.1)
title("Fine intensity (# / 1,000 ha)", cex.main = 1.1, family = "Merriweather Black")

dev.off()

# Supplement variant with values instead of counts
cairo_pdf("outputs/fine_v_intensity_separate.pdf", height = 8, width = 8, pointsize = 16, family = "Noto Sans")
op <- par(mar = c(0, 3, 2, 0), family = "Noto Sans")

par(fig = c(0, 1, .7, 1)) # First plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[2]] / 1e3, na.rm = TRUE))))
poi <- c(0, min(df_plot_int[[2]] / 1e3, na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[2]] / 1e3, na.rm = TRUE), max(df_plot_int[[2]] / 1e3, na.rm = TRUE))
axis(2, cex.axis = 1, at = poi, labels = round(poi, 0), las = 1)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
rect(df_plot_int[[1]] - .35, 0, df_plot_int[[1]] + .35, df_plot_int[[2]] / 1e3, col = "#008040")
title("Forest loss (1,000 ha)", cex.main = 1.1, family = "Merriweather Black")

par(fig = c(0, 1, .4, .7), new = TRUE) # Second plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[6]] / 1e6, na.rm = TRUE))))
poi <- c(0, min(df_plot_int[[6]] / 1e6, na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[6]] / 1e6, na.rm = TRUE), max(df_plot_int[[6]] / 1e6, na.rm = TRUE))
axis(2, cex.axis = 1, at = poi, round(poi), las = 1)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
rect(df_plot_int[[1]] - .35, 0, df_plot_int[[1]] + .35, df_plot_int[[6]] / 1e6, col = "#004080")
title("Environmental fines (mio BRL)", cex.main = 1.1, family = "Merriweather Black")

par(fig = c(0, 1, 0, .4), new = TRUE, mar = c(2, 3, 2, 0)) # Third plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, 1.1 * (max(df_plot_int[[11]], na.rm = TRUE))))
axis(1, at = yoi, labels = TRUE, cex.axis = 1, pos = 0)
poi <- c(0, # y-axis marks
  mean(df_plot_int[[11]], na.rm = TRUE), max(df_plot_int[[11]], na.rm = TRUE))
axis(2, cex.axis = 1, at = poi, labels = round(poi), las = 1)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
lines(df_plot_int[[1]], df_plot_int[[11]], lty = 1, lwd = 3, col = "#008080")
points(df_plot_int[[1]], df_plot_int[[11]], pch = 16, lwd = 1.5, cex = 1, col = "#006060")
# Add labels to the x-axis
text(2007, 100, labels = "Lula", cex = .8, col = "#000000")
arrows(x0 = 2003.25, x1 = 2010.75, y0 = 0, y1 = 0, length = 0.1)
text(2013.5, 100, labels = "Rousseff", cex = .8, col = "#000000")
arrows(x0 = 2011.25, x1 = 2015.75, y0 = 0, y1 = 0, length = 0.1)
text(2017.5, 100, labels = "Temer", cex = .8, col = "#000000")
arrows(x0 = 2016.25, x1 = 2018.75, y0 = 0, y1 = 0, length = 0.1)
text(2020.5, 100, labels = "Bolsonaro", cex = .8, col = "#000000")
arrows(x0 = 2019.25, x1 = 2023, y0 = 0, y1 = 0, length = 0.1)
title("Fine intensity (BRL / ha)", cex.main = 1.1, family = "Merriweather Black")

dev.off()


# Supplementary Information -----

# Details about status and payment of fines ---
v_fined_out_canc_status <- d_fines |>
  filter(!(id_ai == "9099708 - E" | (name %in%
    c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
    muni == "Porto Velho"))) |>
  group_by(year_fined, status_cat) |>
  summarise(fined = sum(value_fined, na.rm = TRUE), paid = sum(value_paid, na.rm = TRUE))

n_fined_out_canc_status <- d_fines |>
  filter(!(id_ai == "9099708 - E" | (name %in%
    c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
    muni == "Porto Velho"))) |>
  filter(value_fined != 0) |>
  group_by(year_fined, status_cat) |>
  count()

# Fine status
cairo_pdf("outputs/fines_n_fined_status.pdf", 10, 5, pointsize = 12)
n_fined_out_canc_status |> filter(year_fined >= 2000, year_fined < 2022) |>
  ggplot(aes(x = year_fined, y = n, fill = status_cat)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year fined", y = "Number of fines", title = "Fine status") +
  theme_minimal() +
  scale_fill_manual(values = ggthemes::colorblind_pal()(8)[-1]) +
  theme(
    text = element_text(family = "Noto Sans"),
    legend.position = "bottom", legend.title = element_blank(),
    plot.title = element_text(size = 24, family = "Merriweather"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 12))
dev.off()

# Fine status by value
cairo_pdf("outputs/fines_v_fined_status.pdf", 10, 5, pointsize = 12)
v_fined_out_canc_status |> filter(year_fined >= 2000, year_fined < 2022) |>
  ggplot(aes(x = year_fined, y = fined / 1e6, fill = status_cat)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year fined", y = "BRL fined (mio)", title = "Fine status (by value)") +
  theme_minimal() +
  scale_fill_manual(values = ggthemes::colorblind_pal()(8)[-1]) +
  theme(
    text = element_text(family = "Noto Sans"),
    legend.position = "bottom", legend.title = element_blank(),
    plot.title = element_text(size = 24, family = "Merriweather"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 12))
dev.off()

# Which fines are paid at what time
v_dated <- d_fines |> filter(year_fined >= 2000, year_paid >= 2000, year_paid < 2022, year_fined < 2022,
  !(id_ai == "9099708 - E" | (name %in%
  c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
  muni == "Porto Velho"))) |>
  mutate(`Year fined` = cut(year_fined, c(1999, 2003, 2008, 2013, 2018, 2021, Inf))) |>
  group_by(year_paid, `Year fined`) |>
  summarise(n_payments = n(), brl_paid = sum(value_paid, na.rm = TRUE)) |>
  ungroup()
v_dated$`Year fined` <- factor(v_dated$`Year fined`,
  labels = c("2000-2003", "2004-2008", "2009-2013", "2014-2018", "2019-2021"))

cairo_pdf("outputs/fines_n_paid_by_fined_year.pdf", 10, 5, pointsize = 12)
v_dated |>
  ggplot(aes(x = year_paid, y = n_payments, fill = `Year fined`)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year paid", y = "Number of Payments", title = "Fine payments") +
  theme_minimal() +
  scale_fill_manual(values = ggthemes::colorblind_pal()(8)[-1]) +
  theme(
    text = element_text(family = "Noto Sans"),
    legend.position = "bottom", legend.title = element_text(size = 16),
    plot.title = element_text(size = 24, family = "Merriweather"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 12))
dev.off()

cairo_pdf("outputs/fines_v_paid_by_fined_year.pdf", 10, 5, pointsize = 12)
v_dated |>
  ggplot(aes(x = year_paid, y = brl_paid / 1e6, fill = `Year fined`)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year paid", y = "BRL paid (mio)", title = "Fine payments (by value)") +
  theme_minimal() +
  scale_fill_manual(values = ggthemes::colorblind_pal()(8)[-1]) +
  theme(
    text = element_text(family = "Noto Sans"),
    legend.position = "bottom", legend.title = element_text(size = 16),
    plot.title = element_text(size = 24, family = "Merriweather"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 12))
dev.off()


# Maps on forest loss and fines ---
library("tmap")

sh <- readRDS("data/df_spatial.rds")
sh <- sh |> filter(state %in% legal_amazon, year > 2000) |>
  mutate(forest_loss_pct = forest_loss / area_ha)

t <- sh |> filter(year %in% c(2011:2021)) |>
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
    legend.text.size = 1, legend.title.size = 1.2,
    panel.label.size = 1.2, panel.label.height = 1)

tmap_save(t, "outputs/forest_loss.png", device = png, width = 8, height = 6)


t <- sh |> filter(year %in% c(2011:2021)) |>
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
    legend.text.size = 1, legend.title.size = 1.2,
    panel.label.size = 1.2, panel.label.height = 1)

tmap_save(t, "outputs/fine_count.png", device = png, width = 8, height = 6)
