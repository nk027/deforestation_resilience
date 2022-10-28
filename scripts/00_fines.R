
# Data on fines -----
library("dplyr")
library("readr")

# Download files here
# for(state in names(states)) {
#   download.file(
#     url = paste0("https://dadosabertos.ibama.gov.br/dados/SICAFI/", state,
#       "/Arrecadacao/arrecadacaobenstutelados.csv"),
#     destfile = paste0("data/fines_paid/", state, "_arrecadacaobenstutelados.csv"),
#     method = "wget", extra = "--no-check-certificate")
#   download.file(
#     url = paste0("https://dadosabertos.ibama.gov.br/dados/SICAFI/", state,
#       "/Quantidade/multasDistribuidasBensTutelados.csv"),
#     destfile = paste0("data/fines_given/", state, "_multasDistribuidasBensTutelados.csv"),
#     method = "wget", extra = "--no-check-certificate")
# }
# download.file(
#   url = paste0("https://dadosabertos.ibama.gov.br/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao.csv"),
#   destfile = paste0("data/fines_extra/infractions.csv"),
#   method = "wget", extra = "--no-check-certificate")

files <- list.files("data/fines_given") # These have . as separators and , for decimals
d_fine <- lapply(files, \(f) {
  read_csv2(paste0("data/fines_given/", f), locale = locale(decimal = ",", grouping = "."),
            col_names = c("id_ai", "date", "name", "tax_id", "state",
                          "muni", "fine_type", "infraction_type", "legal_frame", "value", "mode",
                          "status", "update"), skip = 1, col_types = cols(date = col_date("%d/%m/%Y"))
  )
})
d_fine <- do.call(rbind, d_fine) |> 
  filter(mode == "Real", fine_type == "Multa", infraction_type == "Flora", !grepl("Excluído", status))


files <- list.files("data/fines_paid") # These have . as decimals for `value` and , for the rest
d_paid <- lapply(files, \(f) {
  read_delim(paste0("data/fines_paid/", f),
             delim = ";", locale = locale(decimal = ",", grouping = "."),
             col_names = c("id_ai", "name", "tax_id", "state", "muni",
                           "status", "fine_type", "infraction_type", "legal_frame", "date", "value", "mode",
                           "payment", "payment_nr", "payment_value", "value_paid", "date_paid",
                           "update"), skip = 1, col_types = cols(date = col_date("%d/%m/%Y"),
                                                                 date_paid = col_date("%d/%m/%Y"), value_paid = col_number())
  )
})
d_paid <- do.call(rbind, d_paid) |>
  filter(mode == "Real", fine_type == "Multa", infraction_type == "Flora", !grepl("Excluído", status))


d_inf <- read_delim("data/fines_extra/infractions.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
d_inf <- d_inf |> 
  transmute(id_ai = paste0(NUM_AUTO_INFRACAO, " - ", SER_AUTO_INFRACAO), 
            date = as.Date(DAT_HORA_AUTO_INFRACAO, "%d/%m/%Y"), 
            name = NOME_INFRATOR, 
            tax_id = CPF_CNPJ_INFRATOR, 
            state = UF, 
            muni = MUNICIPIO, 
            area_class = CLASSIFICACAO_AREA, 
            area_quant = QTD_AREA, 
            type_infraction = INFRACAO_AREA,
            desc_infraction = DES_AUTO_INFRACAO,
            desc_legal_frame = DES_INFRACAO)


#####
# Fine payment information here

# Get rid of double-counted payments in d_paid
d_paid <- d_paid |> 
  mutate(dupl_paid = paste0(id_ai, date_paid, payment, value_paid)) |> 
  filter(!duplicated(dupl_paid)) |> 
  select(-dupl_paid)

# Deal with those where there are multiple names for each id_ai
d_paid |> 
  distinct(id_ai, name) |> 
  filter(duplicated(id_ai)) 
# only four, deal with manually, selecting the newer payments
d_paid <- d_paid |> 
  filter(!(id_ai == "1180 - D" & date == "1998-09-03"), 
         !(id_ai == "204726 - D" & date == "1999-08-24"),
         !(id_ai == "9125929 - E" & date == "2018-04-13"), 
         !(id_ai == "115996 - D" & date == "2002-12-03"))

# remove entries where multiple payment_nr is available, select the one with the highest sum
paid_id_ai_dupl <- d_paid |> 
  distinct(id_ai, payment_nr) |> 
  filter(duplicated(id_ai)) |> 
  pull(id_ai)
paid_id_ai_dupl_select <- d_paid |> 
  filter(id_ai %in% paid_id_ai_dupl) |> 
  group_by(id_ai, payment_nr) |> 
  arrange(id_ai, date_paid) |>
  summarise(value_paid = sum(value_paid, na.rm = T)) |> 
  group_by(id_ai) |> 
  slice_max(value_paid, n = 1) |> 
  slice_head(n = 1) |> 
  mutate(id_ai_payment_nr = paste0(id_ai, payment_nr)) |> 
  pull(id_ai_payment_nr)
d_paid_dupl_select <- d_paid |> 
  filter(id_ai %in% paid_id_ai_dupl) |> 
  mutate(id_ai_payment_nr = paste0(id_ai, payment_nr)) |> 
  filter(id_ai_payment_nr %in% paid_id_ai_dupl_select) |> 
  select(-id_ai_payment_nr)
d_paid_dupl_excl <- d_paid |> 
  filter(id_ai %in% paid_id_ai_dupl) |> 
  mutate(id_ai_payment_nr = paste0(id_ai, payment_nr)) |> 
  filter(!id_ai_payment_nr %in% paid_id_ai_dupl_select) |> 
  select(-id_ai_payment_nr)

d_paid <- d_paid |> 
  filter(!id_ai %in% paid_id_ai_dupl) |> 
  rbind(d_paid_dupl_select)

# one case where the date recorded in d_paid seems to be off, adjust manually by fixing date
d_paid[which(d_paid$id_ai == "30823 - B" & d_paid$date_paid == "997-12-30"), "date_paid"] <- 
  d_paid[which(d_paid$id_ai == "30823 - B" & d_paid$date_paid == "1997-12-30")[1], "date_paid"]

# remove one case where date of payment is off
d_paid <- d_paid |> 
  filter(date_paid != "1200-10-05")

#####
# General fine information here

# one case where the date recorded in d_fine seems to be off, adjust manually by fixing date
d_fine[which(d_fine$id_ai == "9128809 - E" & d_fine$date == "2018-08-16"), "date"] <- 
  d_fine[which(d_fine$id_ai == "9128809 - E" & d_fine$date == "2016-08-16"), "date"]

# Deal with duplicated values in d_fine
fine_id_ai_dupl <- d_fine |> 
  filter(duplicated(id_ai)) |> 
  distinct(id_ai) |> 
  pull()

# fines replicated more than twice, drop them (55 observations with unclear behaviour)
fine_id_ai_dupl_gr2 <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl) |> 
  group_by(id_ai) |> 
  count() |> 
  filter(n > 2) |> 
  pull(id_ai)
d_fine <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_dupl_gr2)
d_paid <- d_paid |> 
  filter(!id_ai %in% fine_id_ai_dupl_gr2)

# id_ai for fines that show up exactly twice
fine_id_ai_dupl <- fine_id_ai_dupl[which(!fine_id_ai_dupl %in% fine_id_ai_dupl_gr2)]

# check which id_ai is doubled but name is not the same, e.g. 9050935 - E
fine_id_ai_name_dupl <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl) |> 
  group_by(id_ai) |> 
  count(name) |> 
  filter(n < 2) |> 
  distinct(id_ai) |> 
  pull()

# those that have a payment (in d_paid), leave the id_ai untouched, other ones get "n" appended to id_ai
fine_id_ai_name_paid <- d_paid |> 
  filter(id_ai %in% fine_id_ai_name_dupl) |> 
  distinct(id_ai, name) |> 
  mutate(id_ai_name = paste0(id_ai, name))
d_fine <- d_fine |> 
  mutate(id_ai_name = paste0(id_ai, name)) |> 
  mutate(id_ai = replace(id_ai, 
                         id_ai %in% fine_id_ai_name_paid$id_ai & !id_ai_name %in% fine_id_ai_name_paid$id_ai_name, 
                         paste0(id_ai[id_ai %in% fine_id_ai_name_paid$id_ai & !id_ai_name %in% fine_id_ai_name_paid$id_ai_name], "n"))) |> 
  select(-id_ai_name)
# for those where both id_ai-name combinations have no payment, append "n" to older ones
fine_id_ai_dupl <- fine_id_ai_dupl[which(!fine_id_ai_dupl %in% fine_id_ai_name_paid$id_ai)]
fine_id_ai_name_dupl <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl) |> 
  group_by(id_ai) |> 
  count(name) |> 
  filter(n < 2) |> 
  distinct(id_ai) |> 
  pull(id_ai)
d_fine_dupl_name <- d_fine |> 
  filter(id_ai %in% fine_id_ai_name_dupl) |> 
  group_by(id_ai) |> 
  mutate(id_ai = replace(id_ai, 
                         date == min(date), 
                         paste0(id_ai[date == min(date)], "n"))) |> 
  group_by(id_ai) |> 
  slice_head(n = 1)

d_fine <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_name_dupl) |> 
  rbind(d_fine_dupl_name)

fine_id_ai_dupl <- fine_id_ai_dupl[which(!fine_id_ai_dupl %in% fine_id_ai_name_dupl)]


#####
# Deal with remaining duplicates

# Of the remaining duplicates, ones where "Cancelado" is one are the most common type
# Then come those with "Quitado. Baixa..." 
d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl) |> 
  count(status) |> 
  arrange(-n) |> 
  mutate(share = prop.table(n), 
         share_sum = cumsum(share))

#####
# Deal with duplicates where one entry has "Cancelado" as status

# one case where "Cancelado" is double there, deal with manually
d_fine <- d_fine |> 
  filter(!(d_fine$id_ai == "9134997 - E" & is.na(d_fine$legal_frame)))
fine_id_ai_dupl <- fine_id_ai_dupl[-which(fine_id_ai_dupl %in% c("9134997 - E"))]


# Deal with rest of "Cancelado" duplicates where it Appears to be the case that 
# they are recorded twice if some outstanding fine amount (after payment) was cancelled
fine_id_ai_canc_dupl <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl, status == "Cancelado") |> 
  pull(id_ai) 

#  add together payments (if there are any) and subtract from original fine amount to get cancelled amount
d_paid_canc_dupl <- d_paid |> 
  filter(id_ai %in% fine_id_ai_canc_dupl) |> 
  group_by(id_ai, tax_id, date, payment_nr) |> 
  summarise(value_paid = sum(value_paid, na.rm = T))

d_paid_canc_dupl_date <- d_paid |> 
  filter(id_ai %in% fine_id_ai_canc_dupl) |> 
  group_by(id_ai, tax_id, date, payment_nr) |> 
  slice_max(date_paid, n = 1) |> 
  slice_head(n = 1) |> 
  rename(date_canc = date_paid)

d_paid_canc_dupl <- d_paid_canc_dupl |> 
  left_join(d_paid_canc_dupl_date |> ungroup() |> select(id_ai, date_canc), 
            by = c("id_ai"))

d_fine_dupl_canc <- d_fine |> 
  filter(id_ai %in% fine_id_ai_canc_dupl) |> 
  arrange(id_ai, date) |> 
  left_join(d_paid_canc_dupl |> ungroup() |> select(id_ai, date, value_paid, date_canc), 
            by = c("id_ai", "date")) |> 
  filter(!(is.na(value_paid) & status != "Cancelado")) |> # filter out duplicates of entries that have no payments -> assigned fully to "Cancelado" 
  mutate(value_paid = replace(value_paid, is.na(value_paid), 0), 
         value = ifelse(status == "Cancelado", value - value_paid, value_paid), # calculate value that was cancelled, rest is paid
         value = replace(value, value < 0, 0), # "Cancelados" where value_paid > value, set to zero and filter out later
         id_ai = replace(id_ai, 
                         status == "Cancelado" & value_paid > 0, 
                         paste0(id_ai[status == "Cancelado" & value_paid > 0], "c")), # assign new id to cancelled fines
         date = replace(date, # replace date for cancellations with the one from the last payment where applicable
                        !is.na(date_canc) & status == "Cancelado", 
                        date_canc[!is.na(date_canc) & status == "Cancelado"])) |>
  select(-value_paid, -date_canc) |> 
  filter(value > 0) # filter out cancellations where value_paid > value

d_fine <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_canc_dupl) |> 
  rbind(d_fine_dupl_canc)

# remove cases where "Cancelado" is not a duplicate but value is 0
d_fine <- d_fine |> 
  filter(!(value == 0 & status == "Cancelado"))


# id_ai only for those not cancelled
fine_id_ai_dupl <- fine_id_ai_dupl[-which(fine_id_ai_dupl %in% fine_id_ai_canc_dupl)]

# Intermediate assessment for fines still duplicated
fine_val_dupl <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl) 
fine_val_no_dupl <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_dupl)
(sum(fine_val_dupl$value) / 2) / sum(fine_val_no_dupl$value) # value of fines still duplicated is ~0.12% of total fined value
nrow(fine_val_dupl) / 2 / nrow(fine_val_no_dupl) # ~0.9% of all fines (count) are still duplicated


#####
# Deal with duplicates with "Quitado. Baixa..." as status

fine_id_ai_baixa_dupl <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl, grepl("Quitado. Baixa", status)) |> 
  distinct(id_ai) |> 
  pull(id_ai)

# for those marked with "Quitado. Baixa..." it appears that they are counted double, 
# if there has been some kind of rebate on the fine. Thus, the easiest thing to do 
# is probably to just drop the entries other than that. The rebate will be visible 
# when matching the data from d_paid anyway.
d_fine_dupl_baixa <- d_fine |> 
  filter(id_ai %in% fine_id_ai_baixa_dupl & grepl("Quitado. Baixa", status)) |> 
  arrange(id_ai) |> 
  group_by(id_ai) |> 
  slice_head(n = 1)

d_fine <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_baixa_dupl) |> 
  rbind(d_fine_dupl_baixa)

# id_ai only for those that have not status "Quitado. Baixa..."
fine_id_ai_dupl <- fine_id_ai_dupl[-which(fine_id_ai_dupl %in% fine_id_ai_baixa_dupl)]

# Intermediate assessment for fines still duplicated
fine_val_dupl <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl) 
fine_val_no_dupl <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_dupl)
(sum(fine_val_dupl$value) / 2) / sum(fine_val_no_dupl$value) # value of fines still duplicated is ~0.12% of total fined value
nrow(fine_val_dupl) / 2 / nrow(fine_val_no_dupl) # ~0.39% of all fines (count) are still duplicated


#####
# Look at next biggest duplicated family (now in terms of value)
d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl) |> 
  group_by(status) |> 
  summarise(value = sum(value, na.rm = T)) |> 
  arrange(-value) |> 
  mutate(share = value / sum(value), 
         share_sum = cumsum(share))

# Deal with duplicates with "homologação..." as status
fine_id_ai_homol_dupl <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl, grepl("homologação", status)) |> 
  distinct(id_ai) |> 
  pull(id_ai)

# four entries appears with both statuses in d_paid, adjust manually
d_paid |> 
  filter(id_ai %in% fine_id_ai_homol_dupl) |> 
  distinct(id_ai, status) |> count(id_ai) |> filter(n == 2)
d_fine <- d_fine |> 
  filter(!(id_ai %in% "201173 - B" & status == "Baixado por prescrição da pret. punit. (Lei 9873/99,art.1º)"), 
         !(id_ai %in% "202714 - B" & status == "Inscrito na dívida ativa"),
         !(id_ai %in% "202721 - B" & status == "Inscrito na dívida ativa"),
         !(id_ai %in% "30375 - D" & status == "Parcelado pela 1ª vez"))
fine_id_ai_homol_dupl <- fine_id_ai_homol_dupl[which(!fine_id_ai_homol_dupl %in% 
                                                       c("201173 - B", "202714 - B", 
                                                         "202721 - B", "30375 - D"))]


# get those duplicates with "homologação..." as status that appear in d_paid, drop others

# drop those with status not coming up in d_paid
fine_id_ai_homol_paid <- d_paid |> 
  filter(id_ai %in% fine_id_ai_homol_dupl) |> 
  distinct(id_ai, status) |> 
  mutate(id_ai_status = paste0(id_ai, status))
d_fine <- d_fine |> 
  mutate(id_ai_status = paste0(id_ai, status)) |> 
  filter(!(id_ai %in% fine_id_ai_homol_paid$id_ai & !id_ai_status %in% fine_id_ai_homol_paid$id_ai_status)) |> 
  select(-id_ai_status)
fine_id_ai_homol_dupl <- fine_id_ai_homol_dupl[which(!fine_id_ai_homol_dupl %in% fine_id_ai_homol_paid$id_ai)]

# drop those not including homologação in the name
d_fine_dupl_homol <- d_fine |> 
  filter((id_ai %in% fine_id_ai_homol_dupl & grepl("homologa", status))) |> 
  arrange(id_ai, status) |> 
  group_by(id_ai) |> 
  slice_head(n = 1)
d_fine <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_homol_dupl) |> 
  rbind(d_fine_dupl_homol)

# id_ai only for those that have not status "homologação..."
fine_id_ai_dupl <- fine_id_ai_dupl[-which(fine_id_ai_dupl %in% fine_id_ai_homol_dupl)]

# Intermediate assessment for fines still duplicated
fine_val_dupl <- d_fine |> 
  filter(id_ai %in% fine_id_ai_dupl) 
fine_val_no_dupl <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_dupl)
(sum(fine_val_dupl$value) / 2) / sum(fine_val_no_dupl$value) # value of fines still duplicated is ~0.06% of total fined value
nrow(fine_val_dupl) / 2 / nrow(fine_val_no_dupl) # ~0.32% of all fines (count) are still duplicated
paid_num_dupl <- d_paid |> 
  filter(id_ai %in% fine_id_ai_dupl) |> nrow()
paid_num_dupl / nrow(d_paid) # in payment data ~0.28% of entries are associated with remaining duplicates


#####
# Given the small number and unclear nature of remaining duplicates, drop them
d_fine <- d_fine |> 
  filter(!id_ai %in% fine_id_ai_dupl)
d_paid <- d_paid |> 
  filter(!id_ai %in% fine_id_ai_dupl)


######
# join paid and fine dataframes

d_fines <- full_join(
  d_fine |> dplyr::select(id_ai, tax_id, state, muni, date, value, name, status),
  d_paid |> dplyr::select(id_ai, tax_id, state, muni, date, date_paid, value_paid),
  by = c("id_ai", "tax_id", "date", "state", "muni"))

# remove those entries where value_paid is more than ten times the amount fined
id_ai_fines_large <- d_fines |> 
  group_by(id_ai, value) |> 
  summarise(value_paid = sum(value_paid, na.rm = T)) |> 
  filter(value_paid > value * 10) |> 
  pull(id_ai)
d_fines <- d_fines |> filter(!id_ai %in% id_ai_fines_large)

# put 0 for non-first elements with multiple payments, so that fine value is not duplicated
d_fines <- d_fines |> 
  arrange(id_ai, date, date_paid, -value_paid) |> 
  mutate(value = replace(value, duplicated(id_ai), NA))

# To wrap up, we need to induce concordance for the municipalities (manually and painful) ---
conc <- read_csv("inst/muni_concordance.csv", col_types = "cccc")
d_fines <- d_fines |> rename(muni_fines = muni, state_fines = state) |>
  left_join(conc, by = c("muni_fines", "state_fines")) |>
  transmute(id_ai, state, muni, date_fined = date, value_fined = value,
            date_paid, value_paid, status, name) |>
  tidyr::replace_na(list(value_paid = 0))

# There's three nonsensical fine dates that we drop and five for fines paid
d_fines <- d_fines |> filter(date_fined > "1970-01-01" & date_fined < "2025-01-01")
# Same for payments
d_fines <- d_fines |> filter((date_paid > "1970-01-01" & date_paid < "2025-01-01") | is.na(date_paid))
# And 172 with zeros, where status is not Cancelado
fines_id_ai_zeroes <- d_fines |> 
  filter((value_fined == 0 & status != "Cancelado")) |> 
  distinct(id_ai) |> pull(id_ai)
d_fines <- d_fines |> 
  filter(!id_ai %in% fines_id_ai_zeroes) |> 
  tidyr::replace_na(list(value_fined = 0))


# add larger categories to make life easier
status_cats <- read.csv("./inst/status_list_cats.csv")
d_fines <- d_fines |> 
  left_join(status_cats |> select(-n) |> rename(status_cat = cat), 
            by = c("status"))


saveRDS(d_fines, "data/fines_raw.rds")

# To work with this on municipality level, we need information per municipality and year
d_fines <- d_fines |> mutate(
  year_fined = as.integer(format(date_fined, "%Y")),
  year_paid = as.integer(format(date_paid, "%Y")))

d_fines_f <- d_fines |> filter(value_fined != 0) |> group_by(muni, state, year_fined) |> # Fines per year
  summarise(n_fined = n(), brl_fined = sum(value_fined, na.rm = TRUE))
d_fines_p <- d_fines |> filter(value_paid != 0) |>  group_by(muni, state, year_paid) |> # Payments per year
  summarise(n_payments = n(), brl_paid = sum(value_paid, na.rm = TRUE))
d_fines_c <- d_fines |> # Cancellations per year, might be interesting to exclude
  filter(status == "Cancelado", value_fined != 0) |> 
  group_by(muni, state, year_fined) |> 
  summarise(n_cancelled = n(), brl_cancelled = sum(value_fined, na.rm = TRUE))
# For 2016 and 2019 some cancelled entries seem to be mistakes, exclude them manually
d_fines_c_outliers <- d_fines |>
  filter(id_ai == "9099708 - E" | (name %in% c("GUILHERME GALVANE BATISTA", 
                                               "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", 
                                               "BUNGE ALIMENTOS SA") & 
                                     muni == "Porto Velho")) |> 
  group_by(muni, state, year_fined) |> 
  summarise(n_outlier_cancelled = n(), brl_outlier_cancelled = sum(value_fined, na.rm = TRUE))

d_fines <- left_join(d_fines_f |> rename(year = year_fined),
                     d_fines_p |> rename(year = year_paid),
                     by = c("muni", "state", "year")) |> filter(year > 1999, year < 2022) |> 
  left_join(d_fines_c |> rename(year = year_fined), 
            by = c("muni", "state", "year")) |> 
  left_join(d_fines_c_outliers |> rename(year = year_fined), 
            by = c("muni", "state", "year"))

saveRDS(d_fines, "data/fines.rds")

