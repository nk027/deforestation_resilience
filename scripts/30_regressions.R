# File for running regressions ---

library("dplyr")
library("stargazer")
library("ivreg")
library("lmtest")
library("sandwich")
library("car")
library("multcomp")

states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
            "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
            "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
            "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
            "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
            "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
            "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")
ufs <- structure(names(states), names = states)
legal_amazon <- c("AC", "AP", "AM",  "MA", "MT", "PA", "RO", "RR", "TO")

df_reg <- readRDS("data/df_merged.rds") 

df_reg <- df_reg |> arrange(muni_id, year) |> group_by(muni_id) |> 
  mutate(
    forest_loss = ifelse(is.na(forest_loss), 0, forest_loss),
    forest_share_init = forest_init / area_ha,
    forest_ama_share = forest_ama / forest, 
    forest_all_ama = ifelse(forest_ama_share == 1, 1, 0),
    lula = year %in% 2003:2010, 
    rousseff = year %in% 2011:2015, 
    temer = year %in% 2016:2018,
    bolsonaro = year %in% 2019:2021,
    lr_comb = lula + rousseff,
    post_covid = year > 2019,
    pre_covid = (1 - lr_comb) - post_covid,
    forest = lag(forest, order_by = year),
    forest_ama = lag(forest_ama, order_by = year),
    population = lag(population, order_by = year),
    gdp_pc = lag(gdp_pc, order_by = year),
    soy_price = lag(soy_price, order_by = year),
    cattle = lag(cattle, order_by = year),
    pa_ind_km2 = lag(pa_ind_km2, order_by = year),
    pa_tot_km2 = lag(pa_tot_km2, order_by = year),
    n_fined_wo_out = lag(n_fined_wo_out, order_by = year),
    brl_fined_wo_out = lag(brl_fined_wo_out, order_by = year),
    n_fined_wo_out_lag2 = lag(n_fined_wo_out, order_by = year),
    brl_fined_wo_out_lag2 = lag(brl_fined_wo_out, order_by = year),
    n_fined_wo_out_lag3 = lag(n_fined_wo_out_lag2, order_by = year),
    brl_fined_wo_out_lag3 = lag(brl_fined_wo_out_lag2, order_by = year)
  ) |> 
  filter(year >= 2003, year <= 2021, state %in% legal_amazon)


#####
# Baseline results

# without year fixed effects
# number of fines
ols_mod1 <- lm(log(1 + forest_loss) ~
                 log(forest) +
                 log(population) + log(gdp_pc) +
                 log(1 + soy_price) + log(1 + cattle) +
                 spei_dry +
                 log(1 + pa_ind_km2) +
                 log(1 + n_fined_wo_out) * lula +
                 log(1 + n_fined_wo_out) * rousseff + 
                 log(1 + n_fined_wo_out) * temer + 
                 factor(muni_id)
               , data = df_reg)

# Lula + Rousseff vs Temer + Bolsonaro
ols_mod2 <- lm(log(1 + forest_loss) ~
                 log(forest) +
                 log(population) + log(gdp_pc) +
                 log(1 + soy_price) + log(1 + cattle) +
                 spei_dry +
                 log(1 + pa_ind_km2) +
                 log(1 + n_fined_wo_out) * lr_comb +
                 factor(muni_id)
               , data = df_reg)

# value of fines
ols_mod3 <- lm(log(1 + forest_loss) ~
                 log(forest) +
                 log(population) + log(gdp_pc) +
                 log(1 + soy_price) + log(1 + cattle) +
                 spei_dry +
                 log(1 + pa_ind_km2) +
                 log(1 + brl_fined_wo_out) * lula +
                 log(1 + brl_fined_wo_out) * rousseff + 
                 log(1 + brl_fined_wo_out) * temer + 
                 factor(muni_id)
               , data = df_reg)

# Lula + Rousseff vs Temer + Bolsonaro
ols_mod4 <- lm(log(1 + forest_loss) ~
                 log(forest) +
                 log(population) + log(gdp_pc) +
                 log(1 + soy_price) + log(1 + cattle) +
                 spei_dry +
                 log(1 + pa_ind_km2) +
                 log(1 + brl_fined_wo_out) * lr_comb + 
                 factor(muni_id)
               , data = df_reg)

### with year fixed effects
# number of fines
ols_mod5 <- lm(log(1 + forest_loss) ~
                 log(forest) +
                 log(population) + log(gdp_pc) +
                 log(1 + soy_price) + log(1 + cattle) +
                 spei_dry +
                 log(1 + pa_ind_km2) +
                 log(1 + n_fined_wo_out) * lula +
                 log(1 + n_fined_wo_out) * rousseff + 
                 log(1 + n_fined_wo_out) * temer + 
                 factor(muni_id) + factor(year)
               , data = df_reg)

# Lula + Rousseff vs Temer + Bolsonaro
ols_mod6 <- lm(log(1 + forest_loss) ~
                 log(forest) +
                 log(population) + log(gdp_pc) +
                 log(1 + soy_price) + log(1 + cattle) +
                 spei_dry +
                 log(1 + pa_ind_km2) +
                 log(1 + n_fined_wo_out) * lr_comb + 
                 factor(muni_id) + factor(year)
               , data = df_reg)

# value of fines
ols_mod7 <- lm(log(1 + forest_loss) ~
                 log(forest) +
                 log(population) + log(gdp_pc) +
                 log(1 + soy_price) + log(1 + cattle) +
                 spei_dry +
                 log(1 + pa_ind_km2) +
                 log(1 + brl_fined_wo_out) * lula +
                 log(1 + brl_fined_wo_out) * rousseff + 
                 log(1 + brl_fined_wo_out) * temer + 
                 factor(muni_id) + factor(year)
               , data = df_reg)

# Lula + Rousseff vs Temer + Bolsonaro
ols_mod8 <- lm(log(1 + forest_loss) ~
                 log(forest) +
                 log(population) + log(gdp_pc) +
                 log(1 + soy_price) + log(1 + cattle) +
                 spei_dry +
                 log(1 + pa_ind_km2) +
                 log(1 + brl_fined_wo_out) * lr_comb + 
                 factor(muni_id) + factor(year)
               , data = df_reg)

# cluster standard errors
o1 <- coeftest(ols_mod1, vcov = vcovCL, cluster = ~muni_id)
o2 <- coeftest(ols_mod2, vcov = vcovCL, cluster = ~muni_id)
o3 <- coeftest(ols_mod3, vcov = vcovCL, cluster = ~muni_id)
o4 <- coeftest(ols_mod4, vcov = vcovCL, cluster = ~muni_id)
o5 <- coeftest(ols_mod5, vcov = vcovCL, cluster = ~muni_id)
o6 <- coeftest(ols_mod6, vcov = vcovCL, cluster = ~muni_id)
o7 <- coeftest(ols_mod7, vcov = vcovCL, cluster = ~muni_id)
o8 <- coeftest(ols_mod8, vcov = vcovCL, cluster = ~muni_id)

vcov_o2 <- vcovCL(ols_mod2, cluster = ~muni_id) 
summary(glht(ols_mod2, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                   `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o2)) 

vcov_o6 <- vcovCL(ols_mod6, cluster = ~muni_id) 
summary(glht(ols_mod6, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                   `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o6, coef = na.omit(coef(ols_mod6)))) 

vcov_o4 <- vcovCL(ols_mod4, cluster = ~muni_id) 
summary(glht(ols_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` +
                                   `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o4)) 

vcov_o8 <- vcovCL(ols_mod8, cluster = ~muni_id) 
summary(glht(ols_mod8, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                   `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o8, coef = na.omit(coef(ols_mod8)))) 

# Results with clustered standard errors
stargazer(o1, o2, o5, o6, o3, o4, o7, o8,
          keep.stat = c("n", "rsq", "adj.rsq"),
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE, 
          font.size = "footnotesize", header = FALSE, 
          dep.var.labels = "Forest loss", 
          title = "OLS regression -- Baseline results",
          add.lines = list(c("Year FE", "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes"), 
                           c("Unit FE", rep("Yes", 8))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (no.)", "Fines (value)",
                               "Lula", "Rousseff", "Temer",
                               "pre-2016",
                               "Fines (no.) x Lula", "Fines (no.) x Rousseff", 
                               "Fines (no.) x Temer", "Fines (no.) x pre-2016", 
                               "Fines (value) x Lula", "Fines (value) x Rousseff", 
                               "Fines (value) x Temer", "Fines (value) x pre-2016"))


#####
# Results with second lag of fine variables

# without year fixed effects
# number of fines
ols_lag2_mod1 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + n_fined_wo_out_lag2) * lr_comb +
                      factor(muni_id)
                    , data = df_reg)

# value of fines
ols_lag2_mod2 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + brl_fined_wo_out_lag2) * lr_comb + 
                      factor(muni_id)
                    , data = df_reg)

### with year fixed effects
# number of fines
ols_lag2_mod3 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + n_fined_wo_out_lag2) * lr_comb + 
                      factor(muni_id) + factor(year)
                    , data = df_reg)

# value of fines
ols_lag2_mod4 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + brl_fined_wo_out_lag2) * lr_comb + 
                      factor(muni_id) + factor(year)
                    , data = df_reg)

# cluster standard errors
o_lag2_1 <- coeftest(ols_lag2_mod1, vcov = vcovCL, cluster = ~muni_id)
o_lag2_2 <- coeftest(ols_lag2_mod2, vcov = vcovCL, cluster = ~muni_id)
o_lag2_3 <- coeftest(ols_lag2_mod3, vcov = vcovCL, cluster = ~muni_id)
o_lag2_4 <- coeftest(ols_lag2_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_lag2_1 <- vcovCL(ols_lag2_mod1, cluster = ~muni_id) 
summary(glht(ols_lag2_mod1, linfct =  c("`log(1 + n_fined_wo_out_lag2)` + 
                                        `log(1 + n_fined_wo_out_lag2):lr_comb` = 0"), 
             vcov = vcov_o_lag2_1)) 

vcov_o_lag2_3 <- vcovCL(ols_lag2_mod3, cluster = ~muni_id) 
summary(glht(ols_lag2_mod3, linfct =  c("`log(1 + n_fined_wo_out_lag2)` + 
                                        `log(1 + n_fined_wo_out_lag2):lr_comb` = 0"), 
             vcov = vcov_o_lag2_3, coef = na.omit(coef(ols_lag2_mod3)))) 

vcov_o_lag2_2 <- vcovCL(ols_lag2_mod2, cluster = ~muni_id) 
summary(glht(ols_lag2_mod2, linfct =  c("`log(1 + brl_fined_wo_out_lag2)` + 
                                        `log(1 + brl_fined_wo_out_lag2):lr_comb` = 0"), 
             vcov = vcov_o_lag2_2)) 

vcov_o_lag2_4 <- vcovCL(ols_lag2_mod4, cluster = ~muni_id) 
summary(glht(ols_lag2_mod4, linfct =  c("`log(1 + brl_fined_wo_out_lag2)` + 
                                        `log(1 + brl_fined_wo_out_lag2):lr_comb` = 0"), 
             vcov = vcov_o_lag2_4, coef = na.omit(coef(ols_lag2_mod4)))) 

# Results with clustered standard errors
stargazer(o_lag2_1, o_lag2_3, o_lag2_2, o_lag2_4, 
          keep.stat = c("n", "rsq", "adj.rsq"),
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression -- Second lag of fines",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (t-2) (no.)", "Fines (t-2) (value)",
                               "pre-2016",
                               "Fines (t-2) (no.) x pre-2016",
                               "Fines (t-2) (value) x pre-2016"))


#####
# Results with third lag of fine variables

# without year fixed effects
# number of fines
ols_lag3_mod1 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + n_fined_wo_out_lag3) * lr_comb +
                      factor(muni_id)
                    , data = df_reg)

# value of fines
ols_lag3_mod2 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + brl_fined_wo_out_lag3) * lr_comb + 
                      factor(muni_id)
                    , data = df_reg)

### with year fixed effects
# number of fines
ols_lag3_mod3 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + n_fined_wo_out_lag3) * lr_comb + 
                      factor(muni_id) + factor(year)
                    , data = df_reg)

# value of fines
ols_lag3_mod4 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + brl_fined_wo_out_lag3) * lr_comb + 
                      factor(muni_id) + factor(year)
                    , data = df_reg)

# cluster standard errors
o_lag3_1 <- coeftest(ols_lag3_mod1, vcov = vcovCL, cluster = ~muni_id)
o_lag3_2 <- coeftest(ols_lag3_mod2, vcov = vcovCL, cluster = ~muni_id)
o_lag3_3 <- coeftest(ols_lag3_mod3, vcov = vcovCL, cluster = ~muni_id)
o_lag3_4 <- coeftest(ols_lag3_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_lag3_1 <- vcovCL(ols_lag3_mod1, cluster = ~muni_id) 
summary(glht(ols_lag3_mod1, linfct =  c("`log(1 + n_fined_wo_out_lag3)` + 
                                        `log(1 + n_fined_wo_out_lag3):lr_comb` = 0"), 
             vcov = vcov_o_lag3_1)) 

vcov_o_lag3_3 <- vcovCL(ols_lag3_mod3, cluster = ~muni_id) 
summary(glht(ols_lag3_mod3, linfct =  c("`log(1 + n_fined_wo_out_lag3)` + 
                                        `log(1 + n_fined_wo_out_lag3):lr_comb` = 0"), 
             vcov = vcov_o_lag3_3, coef = na.omit(coef(ols_lag3_mod3)))) 

vcov_o_lag3_2 <- vcovCL(ols_lag3_mod2, cluster = ~muni_id) 
summary(glht(ols_lag3_mod2, linfct =  c("`log(1 + brl_fined_wo_out_lag3)` + 
                                        `log(1 + brl_fined_wo_out_lag3):lr_comb` = 0"), 
             vcov = vcov_o_lag3_2)) 

vcov_o_lag3_4 <- vcovCL(ols_lag3_mod4, cluster = ~muni_id) 
summary(glht(ols_lag3_mod4, linfct =  c("`log(1 + brl_fined_wo_out_lag3)` + 
                                        `log(1 + brl_fined_wo_out_lag3):lr_comb` = 0"), 
             vcov = vcov_o_lag3_4, coef = na.omit(coef(ols_lag3_mod4)))) 

# Results with clustered standard errors
stargazer(o_lag3_1, o_lag3_3, o_lag3_2, o_lag3_4, 
          keep.stat = c("n", "rsq", "adj.rsq"),
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with fines -- Third lag of fines",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (t-3) (no.)", "Fines (t-3) (value)",
                               "pre-2016",
                               "Fines (t-3) (no.) x pre-2016", 
                               "Fines (t-3) (value) x pre-2016"))


#####
# Results with first year of government excluded

# without year fixed effects
# number of fines
ols_fy_mod1 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb +
                    factor(muni_id)
                  , data = df_reg |> filter(!year %in% c(2003, 2011, 2016, 2019)))

# value of fines
ols_fy_mod2 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    factor(muni_id)
                  , data = df_reg |> filter(!year %in% c(2003, 2011, 2016, 2019)))

### with year fixed effects
# number of fines
ols_fy_mod3 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb + 
                    factor(muni_id) + factor(year)
                  , data = df_reg |> filter(!year %in% c(2003, 2011, 2016, 2019)))

# value of fines
ols_fy_mod4 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    factor(muni_id) + factor(year)
                  , data = df_reg |> filter(!year %in% c(2003, 2011, 2016, 2019)))

# cluster standard errors
o_fy_1 <- coeftest(ols_fy_mod1, vcov = vcovCL, cluster = ~muni_id)
o_fy_2 <- coeftest(ols_fy_mod2, vcov = vcovCL, cluster = ~muni_id)
o_fy_3 <- coeftest(ols_fy_mod3, vcov = vcovCL, cluster = ~muni_id)
o_fy_4 <- coeftest(ols_fy_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_fy_1 <- vcovCL(ols_fy_mod1, cluster = ~muni_id) 
summary(glht(ols_fy_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_fy_1)) 

vcov_o_fy_3 <- vcovCL(ols_fy_mod3, cluster = ~muni_id) 
summary(glht(ols_fy_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_fy_3, coef = na.omit(coef(ols_fy_mod3)))) 

vcov_o_fy_2 <- vcovCL(ols_fy_mod2, cluster = ~muni_id) 
summary(glht(ols_fy_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_fy_2)) 

vcov_o_fy_4 <- vcovCL(ols_fy_mod4, cluster = ~muni_id) 
summary(glht(ols_fy_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_fy_4, coef = na.omit(coef(ols_fy_mod4)))) 

# Results with clustered standard errors
stargazer(o_fy_1, o_fy_3, o_fy_2, o_fy_4,
          keep.stat = c("n", "rsq", "adj.rsq"),
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with fines -- Results excluding first year of government",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (no.)", "Fines (value)",
                               "pre-2016",
                               "Fines (no.) x pre-2016", 
                               "Fines (value) x pre-2016"))


#####
# OLS fixed effects with pre-post split with respect to COVID-19 pandemic

# without year fixed effects
# number of fines
ols_cov_mod1 <- lm(log(1 + forest_loss) ~
                     log(forest) +
                     log(population) + log(gdp_pc) +
                     log(1 + soy_price) + log(1 + cattle) +
                     spei_dry +
                     log(1 + pa_ind_km2) +
                     log(1 + n_fined_wo_out) * lr_comb +
                     log(1 + n_fined_wo_out) * pre_covid +
                     factor(muni_id)
                   , data = df_reg)

# value of fines
ols_cov_mod2 <- lm(log(1 + forest_loss) ~
                     log(forest) +
                     log(population) + log(gdp_pc) +
                     log(1 + soy_price) + log(1 + cattle) +
                     spei_dry +
                     log(1 + pa_ind_km2) +
                     log(1 + brl_fined_wo_out) * lr_comb +
                     log(1 + brl_fined_wo_out) * pre_covid +
                     factor(muni_id)
                   , data = df_reg)

### with year fixed effects
# number of fines
ols_cov_mod3 <- lm(log(1 + forest_loss) ~
                     log(forest) +
                     log(population) + log(gdp_pc) +
                     log(1 + soy_price) + log(1 + cattle) +
                     spei_dry +
                     log(1 + pa_ind_km2) +
                     log(1 + n_fined_wo_out) * lr_comb +
                     log(1 + n_fined_wo_out) * pre_covid +
                     factor(muni_id) + factor(year)
                   , data = df_reg)

# value of fines
ols_cov_mod4 <- lm(log(1 + forest_loss) ~
                     log(forest) +
                     log(population) + log(gdp_pc) +
                     log(1 + soy_price) + log(1 + cattle) +
                     spei_dry +
                     log(1 + pa_ind_km2) +
                     log(1 + brl_fined_wo_out) * lr_comb +
                     log(1 + brl_fined_wo_out) * pre_covid + 
                     factor(muni_id) + factor(year)
                   , data = df_reg)

# cluster standard errors
o_cov1 <- coeftest(ols_cov_mod1, vcov = vcovCL, cluster = ~muni_id)
o_cov2 <- coeftest(ols_cov_mod2, vcov = vcovCL, cluster = ~muni_id)
o_cov3 <- coeftest(ols_cov_mod3, vcov = vcovCL, cluster = ~muni_id)
o_cov4 <- coeftest(ols_cov_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_cov1 <- vcovCL(ols_cov_mod1, cluster = ~muni_id) 
summary(glht(ols_cov_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                       `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_cov1)) 

vcov_o_cov3 <- vcovCL(ols_cov_mod3, cluster = ~muni_id) 
summary(glht(ols_cov_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                       `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_cov3, coef = na.omit(coef(ols_cov_mod3))))

vcov_o_cov2 <- vcovCL(ols_cov_mod2, cluster = ~muni_id) 
summary(glht(ols_cov_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                       `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_cov2)) 

vcov_o_cov4 <- vcovCL(ols_cov_mod4, cluster = ~muni_id) 
summary(glht(ols_cov_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                       `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_cov4, coef = na.omit(coef(ols_cov_mod4)))) 

stargazer(o_cov1, o_cov3, o_cov2, o_cov4,
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with fines, with COVID periods",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (no.)", "Fines (value)",
                               "pre-2016", "pre-COVID",
                               "Fines (no.) x pre-2016", "Fines (no.) x pre-COVID",
                               "Fines (value) x pre-2016", "Fines (value) x pre-COVID"))


#####
# Results for municipalities with at least 10% forest cover

# without year fixed effects
# number of fines
ols_10_mod1 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb +
                    factor(muni_id)
                  , data = df_reg |> filter(forest_share_init > 0.1))

# value of fines
ols_10_mod2 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    factor(muni_id)
                  , data = df_reg |> filter(forest_share_init > 0.1))

### with year fixed effects
# number of fines
ols_10_mod3 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb + 
                    factor(muni_id) + factor(year)
                  , data = df_reg |> filter(forest_share_init > 0.1))

# value of fines
ols_10_mod4 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    factor(muni_id) + factor(year)
                  , data = df_reg |> filter(forest_share_init > 0.1))


# cluster standard errors
o_10_1 <- coeftest(ols_10_mod1, vcov = vcovCL, cluster = ~muni_id)
o_10_2 <- coeftest(ols_10_mod2, vcov = vcovCL, cluster = ~muni_id)
o_10_3 <- coeftest(ols_10_mod3, vcov = vcovCL, cluster = ~muni_id)
o_10_4 <- coeftest(ols_10_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_10_1 <- vcovCL(ols_10_mod1, cluster = ~muni_id) 
summary(glht(ols_10_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_10_1)) 

vcov_o_10_3 <- vcovCL(ols_10_mod3, cluster = ~muni_id) 
summary(glht(ols_10_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_10_3, coef = na.omit(coef(ols_10_mod3)))) 

vcov_o_10_2 <- vcovCL(ols_10_mod2, cluster = ~muni_id) 
summary(glht(ols_10_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_10_2)) 

vcov_o_10_4 <- vcovCL(ols_10_mod4, cluster = ~muni_id) 
summary(glht(ols_10_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_10_4, coef = na.omit(coef(ols_10_mod4)))) 

# Results with clustered standard errors
stargazer(o_10_1, o_10_3, o_10_2, o_10_4,
          keep.stat = c("n", "rsq", "adj.rsq"),
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with fines -- Results for municipalities with at least 10\\% initial forest cover",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (no.)", "Fines (value)",
                               "pre-2016",
                               "Fines (no.) x pre-2016", 
                               "Fines (value) x pre-2016"))


#####
# Results with protected areas total also included

# without year fixed effects
# number of fines
ols_pa_mod1 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) + log(1 + pa_tot_km2) +
                    log(1 + n_fined_wo_out) * lr_comb +
                    factor(muni_id)
                  , data = df_reg)

# value of fines
ols_pa_mod2 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) + log(1 + pa_tot_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    factor(muni_id)
                  , data = df_reg)

### with year fixed effects
# number of fines
ols_pa_mod3 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) + log(1 + pa_tot_km2) +
                    log(1 + n_fined_wo_out) * lr_comb + 
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# value of fines
ols_pa_mod4 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) + log(1 + pa_tot_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# cluster standard errors
o_pa1 <- coeftest(ols_pa_mod1, vcov = vcovCL, cluster = ~muni_id)
o_pa2 <- coeftest(ols_pa_mod2, vcov = vcovCL, cluster = ~muni_id)
o_pa3 <- coeftest(ols_pa_mod3, vcov = vcovCL, cluster = ~muni_id)
o_pa4 <- coeftest(ols_pa_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_pa1 <- vcovCL(ols_pa_mod1, cluster = ~muni_id) 
summary(glht(ols_pa_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_pa1)) 

vcov_o_pa3 <- vcovCL(ols_pa_mod3, cluster = ~muni_id) 
summary(glht(ols_pa_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_pa3, coef = na.omit(coef(ols_pa_mod3)))) 

vcov_o_pa2 <- vcovCL(ols_pa_mod2, cluster = ~muni_id) 
summary(glht(ols_pa_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_pa2)) 

vcov_o_pa4 <- vcovCL(ols_pa_mod4, cluster = ~muni_id) 
summary(glht(ols_pa_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_pa4, coef = na.omit(coef(ols_pa_mod4)))) 

s_pa1 <- stargazer(o_pa1, o_pa3, o_pa2, o_pa4,
                   keep.stat = c("n", "rsq", "adj.rsq"),
                   omit = c("year", "muni_id"), type = "latex",
                   column.sep.width = "-10pt", no.space = TRUE,
                   font.size = "footnotesize", header = FALSE,
                   dep.var.labels = "Forest loss", 
                   title = "OLS regression with fines, with protected areas added",
                   add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                                    c("Unit FE", rep("Yes", 4))),
                   covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                                        "SPEI dry", "Indigenous areas", "Protected areas", 
                                        "Fines (no.)", "Fines (value)",
                                        "pre-2016",
                                        "Fines (no.) x pre-2016",
                                        "Fines (value) x pre-2016"))


#####
# Results with control for municipo verde program 

# without year fixed effects
# number of fines
ols_mv_mod1 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb +
                    mverde +
                    factor(muni_id)
                  , data = df_reg)

# value of fines
ols_mv_mod2 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    mverde +
                    factor(muni_id)
                  , data = df_reg)

### with year fixed effects
# number of fines
ols_mv_mod3 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb + 
                    mverde +
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# value of fines
ols_mv_mod4 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    mverde +
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# cluster standard errors
o_mv1 <- coeftest(ols_mv_mod1, vcov = vcovCL, cluster = ~muni_id)
o_mv2 <- coeftest(ols_mv_mod2, vcov = vcovCL, cluster = ~muni_id)
o_mv3 <- coeftest(ols_mv_mod3, vcov = vcovCL, cluster = ~muni_id)
o_mv4 <- coeftest(ols_mv_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_mv1 <- vcovCL(ols_mv_mod1, cluster = ~muni_id) 
summary(glht(ols_mv_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_mv1)) 

vcov_o_mv3 <- vcovCL(ols_mv_mod3, cluster = ~muni_id) 
summary(glht(ols_mv_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_mv3, coef = na.omit(coef(ols_mv_mod3)))) 

vcov_o_mv2 <- vcovCL(ols_mv_mod2, cluster = ~muni_id) 
summary(glht(ols_mv_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_mv2)) 

vcov_o_mv4 <- vcovCL(ols_mv_mod4, cluster = ~muni_id) 
summary(glht(ols_mv_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_mv4, coef = na.omit(coef(ols_mv_mod4)))) 

stargazer(o_mv1, o_mv3, o_mv2, o_mv4,
          keep.stat = c("n", "rsq", "adj.rsq"),
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with fines, with dummy for priority municipalities",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (no.)", "Fines (value)",
                               "pre-2016", "Priority Municipality",
                               "Fines (no.) x pre-2016",
                               "Fines (value) x pre-2016"))

#####
# Results with control for soy moratorium treatment 

# without year fixed effects
# number of fines
ols_sm_mod1 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb +
                    sm_treat +
                    factor(muni_id)
                  , data = df_reg)

# value of fines
ols_sm_mod2 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    sm_treat +
                    factor(muni_id)
                  , data = df_reg)

### with year fixed effects
# number of fines
ols_sm_mod3 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb + 
                    sm_treat +
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# value of fines
ols_sm_mod4 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    sm_treat +
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# cluster standard errors
o_sm1 <- coeftest(ols_sm_mod1, vcov = vcovCL, cluster = ~muni_id)
o_sm2 <- coeftest(ols_sm_mod2, vcov = vcovCL, cluster = ~muni_id)
o_sm3 <- coeftest(ols_sm_mod3, vcov = vcovCL, cluster = ~muni_id)
o_sm4 <- coeftest(ols_sm_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_sm1 <- vcovCL(ols_sm_mod1, cluster = ~muni_id) 
summary(glht(ols_sm_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_sm1)) 

vcov_o_sm3 <- vcovCL(ols_sm_mod3, cluster = ~muni_id) 
summary(glht(ols_sm_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_sm3, coef = na.omit(coef(ols_sm_mod3)))) 

vcov_o_sm2 <- vcovCL(ols_sm_mod2, cluster = ~muni_id) 
summary(glht(ols_sm_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_sm2)) 

vcov_o_sm4 <- vcovCL(ols_sm_mod4, cluster = ~muni_id) 
summary(glht(ols_sm_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_sm4, coef = na.omit(coef(ols_sm_mod4)))) 

stargazer(o_sm1, o_sm3, o_sm2, o_sm4,
          keep.stat = c("n", "rsq", "adj.rsq"),
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with fines, with dummy for soy moratorium municipalities",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (no.)", "Fines (value)",
                               "pre-2016", "SM treated",
                               "Fines (no.) x pre-2016", "Fines (value) x pre-2016"))


#####
# Results with dummy for Amazon biome-only municipalities

# without year fixed effects
# number of fines
ols_ad_mod1 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    forest_all_ama +
                    log(1 + n_fined_wo_out) * lr_comb +
                    factor(muni_id)
                  , data = df_reg)

# value of fines
ols_ad_mod2 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    forest_all_ama +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    factor(muni_id)
                  , data = df_reg)

### with year fixed effects
# number of fines
ols_ad_mod3 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    forest_all_ama +
                    log(1 + n_fined_wo_out) * lr_comb + 
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# value of fines
ols_ad_mod4 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    forest_all_ama +
                    log(1 + brl_fined_wo_out) * lr_comb + 
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# cluster standard errors
o_ad1 <- coeftest(ols_ad_mod1, vcov = vcovCL, cluster = ~muni_id)
o_ad2 <- coeftest(ols_ad_mod2, vcov = vcovCL, cluster = ~muni_id)
o_ad3 <- coeftest(ols_ad_mod3, vcov = vcovCL, cluster = ~muni_id)
o_ad4 <- coeftest(ols_ad_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_ad1 <- vcovCL(ols_ad_mod1, cluster = ~muni_id) 
summary(glht(ols_ad_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_ad1, coef = na.omit(coef(ols_ad_mod1)))) 

vcov_o_ad3 <- vcovCL(ols_ad_mod3, cluster = ~muni_id) 
summary(glht(ols_ad_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                      `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_ad3, coef = na.omit(coef(ols_ad_mod3)))) 

vcov_o_ad2 <- vcovCL(ols_ad_mod2, cluster = ~muni_id) 
summary(glht(ols_ad_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_ad2, coef = na.omit(coef(ols_ad_mod2)))) 

vcov_o_ad4 <- vcovCL(ols_ad_mod4, cluster = ~muni_id) 
summary(glht(ols_ad_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                      `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_ad4, coef = na.omit(coef(ols_ad_mod4)))) 

stargazer(o_ad1, o_ad3, o_ad2, o_ad4,
          keep.stat = c("n", "rsq", "adj.rsq"),
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with fines, with dummy for municipalities in Amazon biome",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (no.)", "Fines (value)",
                               "pre-2016", "Amazon biome",
                               "Fines (no.) x pre-2016", "Fines (value) x pre-2016"))


#####
# Results with triple interaction for municipalities fully in Amazon biome

# without year fixed effects
# number of fines
ols_trip_mod1 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + n_fined_wo_out) * lr_comb  * forest_all_ama +
                      factor(muni_id)
                    , data = df_reg)

# value of fines
ols_trip_mod2 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + brl_fined_wo_out) * lr_comb  * forest_all_ama + 
                      factor(muni_id)
                    , data = df_reg)

### with year fixed effects
# number of fines
ols_trip_mod3 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + n_fined_wo_out) * lr_comb * forest_all_ama  + 
                      factor(muni_id) + factor(year)
                    , data = df_reg)

# value of fines
ols_trip_mod4 <- lm(log(1 + forest_loss) ~
                      log(forest) +
                      log(population) + log(gdp_pc) +
                      log(1 + soy_price) + log(1 + cattle) +
                      spei_dry +
                      log(1 + pa_ind_km2) +
                      log(1 + brl_fined_wo_out) * lr_comb  * forest_all_ama + 
                      factor(muni_id) + factor(year)
                    , data = df_reg)

# cluster standard errors
o_trip1 <- coeftest(ols_trip_mod1, vcov = vcovCL, cluster = ~muni_id)
o_trip2 <- coeftest(ols_trip_mod2, vcov = vcovCL, cluster = ~muni_id)
o_trip3 <- coeftest(ols_trip_mod3, vcov = vcovCL, cluster = ~muni_id)
o_trip4 <- coeftest(ols_trip_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_trip1 <- vcovCL(ols_trip_mod1, cluster = ~muni_id) 
# Test of effect of number of fines under Temer+Bolsonaro in Amazon biome
summary(glht(ols_trip_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                        `log(1 + n_fined_wo_out):forest_all_ama` = 0"), 
             vcov = vcov_o_trip1, coef = na.omit(coef(ols_trip_mod1)))) 
# Test of effect of number of fines under Lula+Rousseff in non-Amazon biome
summary(glht(ols_trip_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                        `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_trip1, coef = na.omit(coef(ols_trip_mod1)))) 
# Test of effect of number of fines under Lula+Rousseff in Amazon biome
summary(glht(ols_trip_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                        `log(1 + n_fined_wo_out):forest_all_ama` + 
                                        `log(1 + n_fined_wo_out):lr_comb:forest_all_ama` = 0"), 
             vcov = vcov_o_trip1, coef = na.omit(coef(ols_trip_mod1)))) 

# Test of effect of number of fines under Temer+Bolsonaro in Amazon biome
vcov_o_trip3 <- vcovCL(ols_trip_mod3, cluster = ~muni_id) 
summary(glht(ols_trip_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                        `log(1 + n_fined_wo_out):forest_all_ama` = 0"), 
             vcov = vcov_o_trip3, coef = na.omit(coef(ols_trip_mod3)))) 
# Test of effect of number of fines under Lula+Rousseff in non-Amazon biome
summary(glht(ols_trip_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                        `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_trip3, coef = na.omit(coef(ols_trip_mod3)))) 
# Test of effect of number of fines under Lula+Rousseff in Amazon biome
summary(glht(ols_trip_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                        `log(1 + n_fined_wo_out):forest_all_ama` + 
                                        `log(1 + n_fined_wo_out):lr_comb:forest_all_ama` = 0"), 
             vcov = vcov_o_trip3, coef = na.omit(coef(ols_trip_mod3)))) 

# Test of effect of value of fines under Temer+Bolsonaro in Amazon biome
vcov_o_trip2 <- vcovCL(ols_trip_mod2, cluster = ~muni_id) 
summary(glht(ols_trip_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                        `log(1 + brl_fined_wo_out):forest_all_ama` = 0"), 
             vcov = vcov_o_trip2, coef = na.omit(coef(ols_trip_mod2))))
# Test of effect of value of fines under Lula+Rousseff in non-Amazon biome
summary(glht(ols_trip_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                        `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_trip2, coef = na.omit(coef(ols_trip_mod2)))) 
# Test of effect of value of fines under Lula+Rousseff in Amazon biome
summary(glht(ols_trip_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                        `log(1 + brl_fined_wo_out):forest_all_ama` + 
                                        `log(1 + brl_fined_wo_out):lr_comb:forest_all_ama` = 0"), 
             vcov = vcov_o_trip2, coef = na.omit(coef(ols_trip_mod2)))) 

# Test of effect of value of fines under Temer+Bolsonaro in Amazon biome
vcov_o_trip4 <- vcovCL(ols_trip_mod4, cluster = ~muni_id) 
summary(glht(ols_trip_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                        `log(1 + brl_fined_wo_out):forest_all_ama` = 0"), 
             vcov = vcov_o_trip4, coef = na.omit(coef(ols_trip_mod4)))) 
# Test of effect of value of fines under Lula+Rousseff in non-Amazon biome
summary(glht(ols_trip_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                        `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_trip4, coef = na.omit(coef(ols_trip_mod4)))) 
# Test of effect of value of fines under Lula+Rousseff in Amazon biome
summary(glht(ols_trip_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                        `log(1 + brl_fined_wo_out):forest_all_ama` + 
                                        `log(1 + brl_fined_wo_out):lr_comb:forest_all_ama` = 0"), 
             vcov = vcov_o_trip4, coef = na.omit(coef(ols_trip_mod4)))) 

stargazer(o_trip1, o_trip3, o_trip2, o_trip4,
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with triple interaction for Amazon biome",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", "Fines (no.)", "Fines (value)",
                               "pre-2016", "Amazon Biome", 
                               "Fines (no.) x pre-2016", "Fines (no.) x Amazon Biome",
                               "Fines (value) x pre-2016", "Fines (value) x Amazon Biome",
                               "pre-2016 x Amazon Biome", 
                               "Fines (no.) x pre-2016 x Amazon Biome",
                               "Fines (value) x pre-2016 x Amazon Biome"))


#####
# Results with triple interaction for priority municipalilities program 

# without year fixed effects
# number of fines
ols_mv_trip_mod1 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb * mverde +
                    factor(muni_id)
                  , data = df_reg)

# value of fines
ols_mv_trip_mod2 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb * mverde + 
                    factor(muni_id)
                  , data = df_reg)


### with year fixed effects
# number of fines
ols_mv_trip_mod3 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + n_fined_wo_out) * lr_comb * mverde + 
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# value of fines
ols_mv_trip_mod4 <- lm(log(1 + forest_loss) ~
                    log(forest) +
                    log(population) + log(gdp_pc) +
                    log(1 + soy_price) + log(1 + cattle) +
                    spei_dry +
                    log(1 + pa_ind_km2) +
                    log(1 + brl_fined_wo_out) * lr_comb * mverde + 
                    factor(muni_id) + factor(year)
                  , data = df_reg)

# cluster standard errors
o_mv_trip1 <- coeftest(ols_mv_trip_mod1, vcov = vcovCL, cluster = ~muni_id)
o_mv_trip2 <- coeftest(ols_mv_trip_mod2, vcov = vcovCL, cluster = ~muni_id)
o_mv_trip3 <- coeftest(ols_mv_trip_mod3, vcov = vcovCL, cluster = ~muni_id)
o_mv_trip4 <- coeftest(ols_mv_trip_mod4, vcov = vcovCL, cluster = ~muni_id)

vcov_o_mv_trip1 <- vcovCL(ols_mv_trip_mod1, cluster = ~muni_id) 
# Test of effect of number of fines under Temer+Bolsonaro in priority municipalities
summary(glht(ols_mv_trip_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                           `log(1 + n_fined_wo_out):mverde` = 0"), 
             vcov = vcov_o_mv_trip1, coef = na.omit(coef(ols_mv_trip_mod1)))) 
# Test of effect of number of fines under Lula+Rousseff in non-priority municipalities
summary(glht(ols_mv_trip_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                           `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_mv_trip1, coef = na.omit(coef(ols_mv_trip_mod1)))) 
# Test of effect of number of fines under Lula+Rousseff in priority municipalities
summary(glht(ols_mv_trip_mod1, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                        `log(1 + n_fined_wo_out):mverde` + 
                                        `log(1 + n_fined_wo_out):lr_comb:mverde` = 0"), 
             vcov = vcov_o_mv_trip1, coef = na.omit(coef(ols_mv_trip_mod1)))) 

# Test of effect of number of fines under Temer+Bolsonaro in priority municipalities
vcov_o_mv_trip3 <- vcovCL(ols_mv_trip_mod3, cluster = ~muni_id) 
summary(glht(ols_mv_trip_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                           `log(1 + n_fined_wo_out):mverde` = 0"), 
             vcov = vcov_o_mv_trip3, coef = na.omit(coef(ols_mv_trip_mod3)))) 
# Test of effect of number of fines under Lula+Roussef in non-priority municipalities
summary(glht(ols_mv_trip_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                           `log(1 + n_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_mv_trip3, coef = na.omit(coef(ols_mv_trip_mod3)))) 
# Test of effect of number of fines under Lula+Rousseff in priority municipalities
summary(glht(ols_mv_trip_mod3, linfct =  c("`log(1 + n_fined_wo_out)` + 
                                        `log(1 + n_fined_wo_out):mverde` + 
                                        `log(1 + n_fined_wo_out):lr_comb:mverde` = 0"), 
             vcov = vcov_o_mv_trip3, coef = na.omit(coef(ols_mv_trip_mod3)))) 

# Test of effect of value of fines under Temer+Bolsonaro in priority municipalities
vcov_o_mv_trip2 <- vcovCL(ols_mv_trip_mod2, cluster = ~muni_id) 
summary(glht(ols_mv_trip_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                           `log(1 + brl_fined_wo_out):mverde` = 0"), 
             vcov = vcov_o_mv_trip2, coef = na.omit(coef(ols_mv_trip_mod2)))) 
# Test of effect of value of fines under Lula+Rousseff in non-priority municipalities
summary(glht(ols_mv_trip_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                           `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_mv_trip2, coef = na.omit(coef(ols_mv_trip_mod2)))) 
# Test of effect of value of fines under Lula+Rousseff in priority municipalities
summary(glht(ols_mv_trip_mod2, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                        `log(1 + brl_fined_wo_out):mverde` + 
                                        `log(1 + brl_fined_wo_out):lr_comb:mverde` = 0"), 
             vcov = vcov_o_mv_trip2, coef = na.omit(coef(ols_mv_trip_mod2)))) 

# Test of effect of value of fines under Temer+Bolsonaro in priority municipalities
vcov_o_mv_trip4 <- vcovCL(ols_mv_trip_mod4, cluster = ~muni_id) 
summary(glht(ols_mv_trip_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                           `log(1 + brl_fined_wo_out):mverde` = 0"), 
             vcov = vcov_o_mv_trip4, coef = na.omit(coef(ols_mv_trip_mod4)))) 
# Test of effect of value of fines under Lula+Rousseff in priority municipalities
summary(glht(ols_mv_trip_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                           `log(1 + brl_fined_wo_out):lr_comb` = 0"), 
             vcov = vcov_o_mv_trip4, coef = na.omit(coef(ols_mv_trip_mod4)))) 
# Test of effect of value of fines under Lula+Rousseff in priority municipalities
summary(glht(ols_mv_trip_mod4, linfct =  c("`log(1 + brl_fined_wo_out)` + 
                                        `log(1 + brl_fined_wo_out):mverde` + 
                                        `log(1 + brl_fined_wo_out):lr_comb:mverde` = 0"), 
             vcov = vcov_o_mv_trip4, coef = na.omit(coef(ols_mv_trip_mod4)))) 

stargazer(o_mv_trip1, o_mv_trip3, o_mv_trip2, o_mv_trip4,
          omit = c("year", "muni_id"), type = "latex",
          column.sep.width = "-10pt", no.space = TRUE,
          font.size = "footnotesize", header = FALSE,
          dep.var.labels = "Forest loss", 
          title = "OLS regression with triple interaction for priority municipalities",
          add.lines = list(c("Year FE", "No", "Yes", "No", "Yes"), 
                           c("Unit FE", rep("Yes", 4))),
          covariate.labels = c("Forest", "Population", "GDP p.c.", "Soy price", "Cattle",
                               "SPEI dry", "Indigenous areas", 
                               "Fines (no.)", "Fines (value)",
                               "pre-2016", "Priority Municipality", 
                               "Fines (no.) x pre-2016", "Fines (no.) x Priority Municipality",
                               "Fines (value) x pre-2016", "Fines (value) x Priority Municipality",
                               "pre-2016 x Priority Municipality", 
                               "Fines (no.) x pre-2016 x Priority Municipality",
                               "Fines (value) x pre-2016 x Priority Municipality"))


#####
# Calculations for missed out forest loss avoidance
fines_2020 <- df_reg |> 
  filter(year == 2020) |> 
  group_by(year) |> 
  summarise(tot_fines = sum(n_fined, na.rm = T)) |> 
  pull()

def_2021 <- df_reg |> 
  filter(year == 2021) |> 
  group_by(year) |> 
  summarise(tot_def = sum(forest_loss, na.rm = T)) |> 
  pull()

c_ols6_post2016 <- coef(ols_mod6)[which(names(coef(ols_mod6)) == "log(1 + n_fined_wo_out)")]
c_ols6_diff2016 <- coef(ols_mod6)[which(names(coef(ols_mod6)) == "log(1 + n_fined_wo_out):lr_comb")]
c_ols6_pre2016 <- c_ols6_post2016 + c_ols6_diff2016

def_chg_post <- exp((log(2*fines_2020) - log(fines_2020)) * c_ols6_post2016)
def_chg_pre <- exp((log(2*fines_2020) - log(fines_2020)) * c_ols6_pre2016)

def_2021 * def_chg_post
def_2021 * def_chg_pre

def_2021 * def_chg_post - def_2021 * def_chg_pre


#####
# Table with summary statistics
df_stat <- readRDS("data/df_merged.rds") 

df_stat <- df_stat |> 
  arrange(muni_id, year) |> 
  filter(year >= 2003, year <= 2021, state %in% legal_amazon)

# Table with summary statistics
tab_sum_stat <- df_stat |> 
  transmute(muni_id, year, 
            forest_loss = log(1 + forest_loss), 
            n_fined_wo_out = log(1 + n_fined_wo_out), 
            brl_fined_wo_out = log(1 + brl_fined_wo_out),
            forest = log(1 + forest), 
            population = log(1 + population), gdp_pc = log (1 + gdp_pc), 
            soy_price = log(1 + (soy_price)), cattle = log(1 + cattle), 
            spei_dry, 
            pa_ind_ha = log(1 + pa_ind_km2), pa_tot_ha = log(1 + pa_tot_km2)) |> 
  ungroup() |> 
  tidyr::pivot_longer(cols = forest_loss:pa_tot_ha) |> 
  mutate(name = factor(name, levels = unique(name))) |> 
  group_by(name) |> 
  summarise(Mean = round(mean(value, na.rm = T), 3), 
            SD = round(sd(value, na.rm = T), 3), 
            Min = round(min(value, na.rm = T), 3), 
            Max = round(max(value, na.rm = T), 3)) |> 
  mutate(Scale = NA, .after = name) |> 
  rename(Variable = name)


xt_tab <- xtable::xtable(tab_sum_stat, digits = 3, caption = "Summary Statistics")

print(xt_tab, include.rownames = F)



