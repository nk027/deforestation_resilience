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
  mutate(year_fined = as.integer(format(date_fined, "%Y")),
         year_paid = as.integer(format(date_paid, "%Y")), 
         month_year_fined = format(date_fined, "%m/%Y"), 
         month_year_paid = format(date_paid, "%m/%Y")) 

# In which year were fines handed out that received payments in 2022?
# Depending on specification between 2009 and 2010 or 2010 and 2011, take 2010
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

# How much fines were paid since 2019? 

# Received at least one payment: ~2%
d_fines |> filter(year_fined > 2018, value_fined != 0,
                  !(id_ai == "9099708 - E" | (name %in% c("GUILHERME GALVANE BATISTA", 
                                                          "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", 
                                                          "BUNGE ALIMENTOS SA") & 
                                                muni == "Porto Velho"))) |> 
  summarise(count = n())
d_fines |> filter(year_fined > 2018, value_fined != 0, value_paid != 0,
                  !(id_ai == "9099708 - E" | (name %in% c("GUILHERME GALVANE BATISTA", 
                                                          "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", 
                                                          "BUNGE ALIMENTOS SA") & 
                                                muni == "Porto Velho"))) |> 
  summarise(count = n())

# Approved but not necessarily received payment yet: ~2.5%
d_fines |> 
  filter(year_fined > 2018, value_fined != 0, 
         !(id_ai == "9099708 - E" | (name %in% c("GUILHERME GALVANE BATISTA", 
                                                 "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", 
                                                 "BUNGE ALIMENTOS SA") & 
                                       muni == "Porto Velho"))) |> 
  group_by(status_cat) |> 
  summarise(n = n()) |> 
  arrange(-n) |> 
  ungroup() |> 
  mutate(share = prop.table(n))

# Value paid in relation to fined value: 0.03%
d_fines |> filter(year_fined > 2018) |> 
  filter(!(id_ai == "9099708 - E" | (name %in% c("GUILHERME GALVANE BATISTA", 
                                                 "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", 
                                                 "BUNGE ALIMENTOS SA") & 
                                       muni == "Porto Velho"))) |> 
  summarise(fined = sum(value_fined, na.rm = T), 
            paid = sum(value_paid, na.rm = T)) |> 
  mutate(share = paid / fined)



##### 
# Plots on forest loss, fines and their values, and related intensities

df_merged <- readRDS("data/df_merged.rds")

df_plot_int <- df_merged |>  filter(state %in% legal_amazon, year > 2000) |> 
  select(year, state, muni, forest_loss, 
         brl_fined, brl_cancelled, brl_outlier_cancelled, 
         n_fined, n_cancelled, n_outlier_cancelled) |> 
  group_by(year) |> 
  summarise(forest_loss = sum(forest_loss, na.rm = T), 
            brl_fined = sum(brl_fined, na.rm = T), 
            brl_cancelled = sum(brl_cancelled, na.rm = T),
            brl_outlier_cancelled = sum(brl_outlier_cancelled, na.rm = T),
            n_fined = sum(n_fined, na.rm = T),
            n_cancelled = sum(n_cancelled, na.rm = T),
            n_outlier_cancelled = sum(n_outlier_cancelled, na.rm = T)) |> 
  transmute(year, forest_loss = replace(forest_loss, forest_loss == 0, NA), 
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

# Main text, Figure 1
pdf("outputs/fine_n_intensity_separate.pdf", 
    height = 9, width = 13.5, pointsize = 16)
op <- par(mar = c(2, 5, 0.6, 2), mfrow = c(3, 1))
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[2]] / 1e3, na.rm = T))))
axis(1, at = c(2001, 2003, 2011, 2016, 2019, 2021), labels = F)
axis(2, at = c(0, 1250, 2500, max(df_plot_int[[2]] / 1e3, na.rm = T)), 
     labels = c(0, 1250, 2500, round(max(df_plot_int[[2]] / 1e3, na.rm = T))),
     cex.axis = 1.25)
rect(2003, 0, 2011, max(df_plot_int[[2]] / 1e3 + 200, na.rm = T), density = NA, border = NA, col = "grey90")
rect(2016, 0, 2019, max(df_plot_int[[2]] / 1e3 + 200, na.rm = T), density = NA, border = NA, col = "grey90")
abline(h = c(0, 1250, 2500, max(df_plot_int[[2]] / 1e3, na.rm = T)), 
       lty = 3, col = "grey40")
abline(v = c(2001, 2003, 2011, 2016, 2019, 2021), lty = 3, col = "grey40")
lines(df_plot_int[[1]], df_plot_int[[2]] / 1e3, lty = 1, lwd = 2, col = "#008080")
points(df_plot_int[[1]], df_plot_int[[2]] / 1e3, pch = 4, lwd = 1.25, cex = 1.5)
mtext("Forest Loss (1,000ha)",side=2, line=4, padj=1.25, cex=0.9)

plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[8]], na.rm = T))))
axis(1, at = c(2001, 2003, 2011, 2016, 2019, 2021), labels = F)
axis(2, at = c(0, 3000, 6000, max(df_plot_int[[8]], na.rm = T)), 
     labels = c(0, 3000, 6000, round(max(df_plot_int[[8]], na.rm = T))),
     cex.axis = 1.25)
rect(2003, 0, 2011, max(df_plot_int[[8]] + 500, na.rm = T), density = NA, border = NA, col = "grey90")
rect(2016, 0, 2019, max(df_plot_int[[8]] + 500, na.rm = T), density = NA, border = NA, col = "grey90")
abline(h = c(0, 3000, 6000, max(df_plot_int[[8]], na.rm = T)), 
       lty = 3, col = "grey40")
abline(v = c(2001, 2003, 2011, 2016, 2019, 2021), lty = 3, col = "grey40")
lines(df_plot_int[[1]], df_plot_int[[8]], lty = 1, lwd = 2, col = "#000080")
points(df_plot_int[[1]], df_plot_int[[8]], pch = 4, lwd = 1.25, cex = 1.5)
mtext("Number of Fines",side=2, line=4, padj=1.25, cex=0.9)

plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[14]] * 1e3, na.rm = T))))
axis(1, at = c(2001, 2003, 2011, 2016, 2019, 2021), labels = T, cex.axis = 1.25)
axis(2, at = c(0, 2, 4, max(df_plot_int[[14]] * 1e3, na.rm = T)), 
     labels = c(0, 2, 4, round(max(df_plot_int[[14]] * 1e3, na.rm = T), 2)),
     cex.axis = 1.25)
rect(2003, 0, 2011, max(df_plot_int[[14]] * 1e3 + 200, na.rm = T), density = NA, border = NA, col = "grey90")
rect(2016, 0, 2019, max(df_plot_int[[14]] * 1e3 + 200, na.rm = T), density = NA, border = NA, col = "grey90")
abline(h = c(0, 2, 4, max(df_plot_int[[14]] * 1e3, na.rm = T)), 
       lty = 3, col = "gray40")
abline(v = c(2001, 2003, 2011, 2016, 2019, 2021), lty = 3, col = "gray40")
lines(df_plot_int[[1]], df_plot_int[[14]] * 1e3, lty = 1, lwd = 2, col = "#008000")
points(df_plot_int[[1]], df_plot_int[[14]] * 1e3, pch = 4, lwd = 1.25, cex = 1.5)
text(2007, 0.5, labels = "Lula", cex = 1.25, col = "#000000")
arrows(x0 = 2003.25, x1 = 2010.75, y0 = 0.25, y1 = 0.25, length = 0.1)
text(2013.5, 0.5, labels = "Rousseff", cex = 1.25, col = "#000000")
arrows(x0 = 2011.25, x1 = 2015.75, y0 = 0.25, y1 = 0.25, length = 0.1)
text(2017.5, 0.5, labels = "Temer", cex = 1.25, col = "#000000")
arrows(x0 = 2016.25, x1 = 2018.75, y0 = 0.25, y1 = 0.25, length = 0.1)
text(2020, 0.5, labels = "Bolsonaro", cex = 1.25, col = "#000000")
arrows(x0 = 2019.25, x1 = 2023, y0 = 0.25, y1 = 0.25, length = 0.1)
mtext("Fine Intensity (#/1,000 ha)", side=2, line=4, padj=1.25, cex=0.9, adj = 0)
dev.off()


# Supplementary Information, Figure S3
pdf("outputs/fine_v_intensity_separate.pdf", 
    height = 9, width = 13.5, pointsize = 16)
op <- par(mar = c(2, 5, 0.6, 2), mfrow = c(3, 1))
plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[2]] / 1e3, na.rm = T))))
axis(1, at = c(2001, 2003, 2011, 2016, 2019, 2021), labels = FALSE)
axis(2, at = c(0, 1250, 2500, max(df_plot_int[[2]] / 1e3, na.rm = T)), 
     labels = c(0, 1250, 2500, round(max(df_plot_int[[2]] / 1e3, na.rm = T))),
     cex.axis = 1.25)
rect(2003, 0, 2011, max(df_plot_int[[2]] / 1e3 + 200, na.rm = T), density = NA, border = NA, col = "grey90")
rect(2016, 0, 2019, max(df_plot_int[[2]] / 1e3 + 200, na.rm = T), density = NA, border = NA, col = "grey90")
abline(h = c(0, 1250, 2500, max(df_plot_int[[2]] / 1e3, na.rm = T)), 
       lty = 3, col = "grey40")
abline(v = c(2001, 2003, 2011, 2016, 2019, 2021), lty = 3, col = "grey40")
lines(df_plot_int[[1]], df_plot_int[[2]] / 1e3, lty = 1, lwd = 2, col = "#008080")
points(df_plot_int[[1]], df_plot_int[[2]] / 1e3, pch = 4, lwd = 1.25, cex = 1.5)
mtext("Forest Loss (1,000ha)",side=2, line=4, padj=1.25, cex=1)

plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[6]] / 1e6, na.rm = T))))
axis(1, at = c(2001, 2003, 2011, 2016, 2019, 2021), labels = FALSE)
axis(2, at = c(0, 1000, 2000, max(df_plot_int[[6]] / 1e6, na.rm = T)), 
     labels = c(0, 1000, 2000, round(max(df_plot_int[[6]] / 1e6, na.rm = T))), 
     cex.axis = 1.25)
rect(2003, 0, 2011, max(df_plot_int[[6]] / 1e6 + 200, na.rm = T), density = NA, border = NA, col = "grey90")
rect(2016, 0, 2019, max(df_plot_int[[6]] / 1e6 + 200, na.rm = T), density = NA, border = NA, col = "grey90")
abline(h = c(0, 1000, 2000, max(df_plot_int[[6]] / 1e6, na.rm = T)), 
       lty = 3, col = "grey40")
abline(v = c(2001, 2003, 2011, 2016, 2019, 2021), lty = 3, col = "grey40")
lines(df_plot_int[[1]], df_plot_int[[6]] / 1e6, lty = 1, lwd = 2, col = "#000080")
points(df_plot_int[[1]], df_plot_int[[6]] / 1e6, pch = 4, lwd = 1.25, cex = 1.5)
mtext("BRL fined (mio)",side=2, line=4, padj=1.25, cex=1)

plot.new()
plot.window(xlim = c(2001, 2021), ylim = c(0, ceiling(max(df_plot_int[[11]], na.rm = T))))
axis(1, at = c(2001, 2003, 2011, 2016, 2019, 2021), cex.axis = 1.25)
axis(2, at = c(0, 750, 1500, max(df_plot_int[[11]], na.rm = T)), 
     labels = c(0, 750, 1500, round(max(df_plot_int[[11]], na.rm = T))), 
     cex.axis = 1.25)
rect(2003, 0, 2011, max(df_plot_int[[11]] + 200, na.rm = T), density = NA, border = NA, col = "grey90")
rect(2016, 0, 2019, max(df_plot_int[[11]] + 200, na.rm = T), density = NA, border = NA, col = "grey90")
abline(h = c(0, 750, 1500, max(df_plot_int[[11]], na.rm = T)), 
       lty = 3, col = "gray40")
abline(v = c(2001, 2003, 2011, 2016, 2019, 2021), lty = 3, col = "gray40")
lines(df_plot_int[[1]], df_plot_int[[11]], lty = 1, lwd = 2, col = "#008000")
points(df_plot_int[[1]], df_plot_int[[11]], pch = 4, lwd = 1.25, cex = 1.5)
text(2007, 150, labels = "Lula", cex = 1.25, col = "#000000")
arrows(x0 = 2003.25, x1 = 2010.75, y0 = 75, y1 = 75, length = 0.1)
text(2013.5, 150, labels = "Rousseff", cex = 1.25, col = "#000000")
arrows(x0 = 2011.25, x1 = 2015.75, y0 = 75, y1 = 75, length = 0.1)
text(2017.5, 150, labels = "Temer", cex = 1.25, col = "#000000")
arrows(x0 = 2016.25, x1 = 2018.75, y0 = 75, y1 = 75, length = 0.1)
text(2020, 150, labels = "Bolsonaro", cex = 1.25, col = "#000000")
arrows(x0 = 2019.25, x1 = 2023, y0 = 75, y1 = 75, length = 0.1)
mtext("Fine Intensity (BRL/ha)",side=2, line=4, padj=1.25, cex=1)
dev.off()



#####
# Supplementary Information plots: Details about status and payment of fines

v_fined_out_canc_status <- d_fines |> 
  filter(!(id_ai == "9099708 - E" | (name %in% c("GUILHERME GALVANE BATISTA", 
                                                 "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", 
                                                 "BUNGE ALIMENTOS SA") & 
                                       muni == "Porto Velho"))) |> 
  group_by(year_fined, status_cat) |> 
  summarise(fined = sum(value_fined, na.rm = T), 
            paid = sum(value_paid, na.rm = T))

n_fined_out_canc_status <- d_fines |> 
  filter(!(id_ai == "9099708 - E" | (name %in% c("GUILHERME GALVANE BATISTA", 
                                                 "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", 
                                                 "BUNGE ALIMENTOS SA") & 
                                       muni == "Porto Velho"))) |> 
  filter(value_fined != 0) |> 
  group_by(year_fined, status_cat) |>  
  count()




# Supplementary Information, Figure S1
pdf("outputs/fines_n_fined_status.pdf", 
    12, 7, pointsize = 16)
ggplot(n_fined_out_canc_status |> filter(year_fined >= 2000, year_fined < 2022), 
       aes(x = year_fined, y = n, fill = status_cat)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_bw() +
  labs(x = "", y = "Number of Fines") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 12))
dev.off()

# Supplementary Information, Figure S4
pdf("outputs/fines_v_fined_status.pdf", 
    12, 7, pointsize = 16)
ggplot(v_fined_out_canc_status |> filter(year_fined >= 2000, year_fined < 2022), 
       aes(x = year_fined, y = fined / 1e6, fill = status_cat)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_bw() +
  labs(x = "", y = "BRL fined (mio)") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 12))
dev.off()


#####
# Which fines are paid at what time
v_dated <- d_fines |> 
  filter(year_fined >= 2000, year_paid >= 2000, year_paid < 2022, year_fined < 2022, 
         !(id_ai == "9099708 - E" | (name %in% c("GUILHERME GALVANE BATISTA", 
                                                 "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", 
                                                 "BUNGE ALIMENTOS SA") & 
                                       muni == "Porto Velho"))) |> 
  group_by(year_paid, year_fined) |>
  summarise(n_payments = n(), brl_paid = sum(value_paid, na.rm = TRUE)) |> 
  mutate(`Year fined` = cut(year_fined, c(1999, 2003, 2008, 2013, 2018, 2021, Inf))) |> 
  ungroup()

v_dated$`Year fined` <- factor(v_dated$`Year fined`, labels = c("2000-2003", "2004-2008", "2009-2013", 
                                                                "2014-2018", "2019-2021"))



# Supplementary Information, Figure S2
pdf("outputs/fines_n_paid_by_fined_year.pdf", 
    12, 7, pointsize = 16)
v_dated |>
  ggplot(aes(x = year_paid, y = n_payments, fill = `Year fined`)) +
  geom_bar(position = "stack", stat = "identity") + 
  theme_bw() +
  labs(x = "", y = "Number of Payments") + 
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 16), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 12))
dev.off()


# Supplementary Information, Figure S5
pdf("outputs/fines_v_paid_by_fined_year.pdf", 
    12, 7, pointsize = 16)
v_dated |>
  ggplot(aes(x = year_paid, y = brl_paid / 1e6, fill = `Year fined`)) +
  geom_bar(position = "stack", stat = "identity") + 
  theme_bw() +
  labs(x = "", y = "BRL paid (mio)") + 
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 16), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 12))
dev.off()

