
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

df_merged <- readRDS("data/df_merged.rds") |> filter(year > 2000)
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

# Relevance of  São Félix do Xingu and Altamira, PA
fl_1 <- df_merged |> filter(grepl("São Félix do Xingu", muni)) |> pull(forest_loss)
fl_2 <- df_merged |> filter(grepl("Altamira$", muni)) |> pull(forest_loss)
fl_3 <- df_merged |> filter(grepl("Novo Progresso", muni)) |> pull(forest_loss)

fl_a <- df_merged |> group_by(year) |>
  summarise(fl = sum(forest_loss, na.rm = TRUE)) |> pull(fl)
plot(x = 2001:2021, (fl_1 + fl_2) / fl_a, ylim = c(0, .1), type = "l") # 5.2% mean, 9.6% last
abline(h = c(0.05, .1), lty = 3, col = "gray")
lines(2001:2021, fl_1 / fl_a, col = 2) # 3% mean, 5.3% last
lines(2001:2021, fl_2 / fl_a, col = 3) # 2.2% mean, 4.3 % last
lines(2001:2021, fl_3 / fl_a, col = "darkgray") # 1.2% mean, 1.6 % last
legend("topleft", legend = c("São Félix do Xingu", "Altamira"), lty = 1, col = 2:3)

# Cumulative loss
f <- df_merged |> group_by(year) |>
  summarise(f = sum(forest, na.rm = TRUE)) |> pull(f)
plot.new()
plot.window(c(2001, 2021), c(0.9, 1))
axis(1, 2001:2021, pos = 0); axis(2)
rect(seq(2000.55, 2020.55), 0, seq(2001.45, 2021.45), f / f[1], col = "darkgreen")

# Plots -----

# Plots on forest loss, fines and their values, and related intensities ---
 df_merged <- readRDS("data/df_merged.rds") |> st_drop_geometry()

df_plot_int <- df_merged |> filter(state %in% legal_amazon, year > 2000) |>
  dplyr::select(year, state, muni, forest_loss, brl_fined, brl_cancelled,
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
cairo_pdf("outputs/fine_n_intensity_separate.pdf", height = 4, width = 4,
  pointsize = 12, family = "Noto Sans")
op <- par(mar = c(0, 2.5, 1.2, 0), family = "Noto Sans")

par(fig = c(0, 1, .7, 1)) # First plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[2]] / 1e3, na.rm = TRUE))))
poi <- c(min(df_plot_int[[2]] / 1e3, na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[2]] / 1e3, na.rm = TRUE), max(df_plot_int[[2]] / 1e3, na.rm = TRUE))
axis(2, cex.axis = .8, at = c(0, poi),
  labels = c(NA, format(round(poi), big.mark = ",")), las = 1, hadj = .8)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
# Barplot
rect(df_plot_int[[1]] - .35, 0, df_plot_int[[1]] + .35, df_plot_int[[2]] / 1e3, col = "#008040")
title("Forest loss (1,000 ha)", cex.main = 1, adj = 0, family = "Merriweather Black")

par(fig = c(0, 1, .4, .7), new = TRUE) # Second plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[8]], na.rm = TRUE))))
poi <- c(min(df_plot_int[[8]], na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[8]], na.rm = TRUE), max(df_plot_int[[8]], na.rm = TRUE))
axis(2, cex.axis = .8, at = c(0, poi),
  labels = c(NA, format(round(poi), big.mark = ",")), las = 1, hadj = .8)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
# Barplot
rect(df_plot_int[[1]] - .35, 0, df_plot_int[[1]] + .35, df_plot_int[[8]], col = "#004080")
title("Environmental fines (#)", cex.main = 1, adj = 0, family = "Merriweather Black")

par(fig = c(0, 1, 0, .4), new = TRUE, mar = c(1.3, 2.5, 1.2, 0)) # Third plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, 1.1 * (max(df_plot_int[[14]] * 1e3, na.rm = TRUE))))
axis(1, at = yoi, labels = c("'03", "'11", "'16", "'19"), cex.axis = .8, pos = 0, padj = -0.6)
poi <- c(min(df_plot_int[[14]] * 1e3, na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[14]] * 1e3, na.rm = TRUE), max(df_plot_int[[14]] * 1e3, na.rm = TRUE))
axis(2, cex.axis = .8, at = c(0, poi), labels = c(NA, round(poi, 2)), las = 1, hadj = .8)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
# Line chart
lines(df_plot_int[[1]], df_plot_int[[14]] * 1e3, lty = 1, lwd = 2, col = "#008080")
points(df_plot_int[[1]], df_plot_int[[14]] * 1e3, pch = 16, lwd = 1, cex = 1, col = "#006060")
# Add labels to the x-axis
# text(2007, .25, labels = "Lula", cex = .8, col = "#000000")
arrows(x0 = 2003.25, x1 = 2010.75, y0 = 0, y1 = 0, length = 0.05)
# text(2013.5, .25, labels = "Rousseff", cex = .8, col = "#000000")
arrows(x0 = 2011.25, x1 = 2015.75, y0 = 0, y1 = 0, length = 0.05)
# text(2017.5, .25, labels = "Temer", cex = .8, col = "#000000")
arrows(x0 = 2016.25, x1 = 2018.75, y0 = 0, y1 = 0, length = 0.05)
# text(2020.5, .25, labels = "Bolsonaro", cex = .8, col = "#000000")
arrows(x0 = 2019.25, x1 = 2023, y0 = 0, y1 = 0, length = 0.05)
title("Fine intensity (#/1,000 ha)", cex.main = 1, adj = 0, family = "Merriweather Black")

dev.off() # Store

# Supplementary variant with values instead of counts ---
cairo_pdf("outputs/fine_v_intensity_separate.pdf", height = 4, width = 4, pointsize = 12, family = "Noto Sans")

op <- par(mar = c(0, 2.5, 1.2, 0), family = "Noto Sans")

par(fig = c(0, 1, .7, 1)) # First plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[2]] / 1e3, na.rm = TRUE))))
poi <- c(min(df_plot_int[[2]] / 1e3, na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[2]] / 1e3, na.rm = TRUE), max(df_plot_int[[2]] / 1e3, na.rm = TRUE))
axis(2, cex.axis = .8, at = c(0, poi),
  labels = c(NA, format(round(poi), big.mark = ",")), las = 1, hadj = .8)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
# Barplot
rect(df_plot_int[[1]] - .35, 0, df_plot_int[[1]] + .35, df_plot_int[[2]] / 1e3, col = "#008040")
title("Forest loss (1,000 ha)", cex.main = 1, adj = 0, family = "Merriweather Black")

par(fig = c(0, 1, .4, .7), new = TRUE) # Second plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[6]] / 1e6, na.rm = TRUE))))
poi <- c(min(df_plot_int[[6]] / 1e6, na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[6]] / 1e6, na.rm = TRUE), max(df_plot_int[[6]] / 1e6, na.rm = TRUE))
axis(2, cex.axis = .8, at = c(0, poi),
  labels = c(NA, format(round(poi), big.mark = ",")), las = 1, hadj = .8)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
rect(df_plot_int[[1]] - .35, 0, df_plot_int[[1]] + .35, df_plot_int[[6]] / 1e6, col = "#004080")
title("Environmental fines (mio BRL)", cex.main = 1, adj = 0, family = "Merriweather Black")

par(fig = c(0, 1, 0, .4), new = TRUE, mar = c(1.3, 2.5, 1.2, 0)) # Third plot
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, 1.1 * (max(df_plot_int[[11]], na.rm = TRUE))))
axis(1, at = yoi, labels = c("'03", "'11", "'16", "'19"), cex.axis = .8, pos = 0, padj = -0.6)
poi <- c(min(df_plot_int[[11]], na.rm = TRUE), # y-axis marks
  mean(df_plot_int[[11]], na.rm = TRUE), max(df_plot_int[[11]], na.rm = TRUE))
axis(2, cex.axis = .9, at = c(poi),
  labels = c(format(round(poi), big.mark = ",")), las = 1, hadj = .8)
rect(yoi[1], -1e6, yoi[2], 1e6, density = NA, border = NA, col = "grey90")
rect(yoi[3], -1e6, yoi[4], 1e6, density = NA, border = NA, col = "grey90")
segments(yoi, -1e6, yoi, 1e6, col = "grey40", lty = 1)
abline(h = poi, col = "grey40", lty = 3)
abline(h = 0, col = "black")
lines(df_plot_int[[1]], df_plot_int[[11]], lty = 1, lwd = 2, col = "#008080")
points(df_plot_int[[1]], df_plot_int[[11]], pch = 16, lwd = 1, cex = 1, col = "#006060")
# Add labels to the x-axis
# text(2007, .25, labels = "Lula", cex = .8, col = "#000000")
arrows(x0 = 2003.25, x1 = 2010.75, y0 = 0, y1 = 0, length = 0.05)
# text(2013.5, .25, labels = "Rousseff", cex = .8, col = "#000000")
arrows(x0 = 2011.25, x1 = 2015.75, y0 = 0, y1 = 0, length = 0.05)
# text(2017.5, .25, labels = "Temer", cex = .8, col = "#000000")
arrows(x0 = 2016.25, x1 = 2018.75, y0 = 0, y1 = 0, length = 0.05)
# text(2020.5, .25, labels = "Bolsonaro", cex = .8, col = "#000000")
arrows(x0 = 2019.25, x1 = 2023, y0 = 0, y1 = 0, length = 0.05)
title("Fine intensity (BRL/ha)", cex.main = 1, adj = 0, family = "Merriweather Black")

dev.off() # Store


# Extra info on fines -----

# deflate fine values first

gdp_defl <- readRDS("data/gdp_defl.rds") |> transmute(year, gdp_defl = gdp_defl / 100)

d_fines <- d_fines |>
  left_join(gdp_defl |> rename(year_fined = year, defl_fined = gdp_defl),
                                by = c("year_fined")) |>
  left_join(gdp_defl |> rename(year_paid = year, defl_paid = gdp_defl),
            by = c("year_paid")) |>
  mutate(value_fined = value_fined / defl_fined,
         value_paid = value_paid / defl_paid)

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

# Simplify the status for the main visualisation
n_fined_simplified <- d_fines |>
  filter(!(id_ai == "9099708 - E" | (name %in%
    c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
    muni == "Porto Velho"))) |>
  filter(value_fined != 0)
ha <- list(
  approved = c("Approved - In payment"),
  waiting = c("Judicious", "Waiting for approval"),
  cancelled = c("Written off", "Written off - limitation period", "Cancelled"),
  other = c("Other")
)
for(i in seq_along(ha))
  n_fined_simplified$status_cat[n_fined_simplified$status_cat %in% ha[[i]]] <- names(ha)[i]

cairo_pdf("outputs/fines_n_fined_status-summarised.pdf", 10, 5, pointsize = 12)
n_fined_simplified |> filter(year_fined >= 2000, year_fined < 2022) |>
  group_by(year_fined, status_cat) |>
  count() |>
  ggplot(aes(x = year_fined, y = n, fill = status_cat)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year fined", y = "Number of fines", title = "Fine status") +
  theme_minimal() +
  scale_fill_manual(values = ggthemes::colorblind_pal()(5)[-1]) +
  theme(
    text = element_text(family = "Noto Sans"),
    legend.position = c(.92, .9),
    legend.background = element_rect(color = "white"),
    legend.title = element_blank(),
    plot.title = element_text(size = 24, family = "Merriweather Black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 12))
dev.off()

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
    plot.title = element_text(size = 24, family = "Merriweather Black"),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 12))
dev.off()


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
    plot.title = element_text(size = 24, family = "Merriweather Black"),
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
    plot.title = element_text(size = 24, family = "Merriweather Black"),
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
    plot.title = element_text(size = 24, family = "Merriweather Black"),
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
    plot.title = element_text(size = 24, family = "Merriweather Black"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 12))
dev.off()
