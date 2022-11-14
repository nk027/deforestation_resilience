
# Data on fines -----
library("dplyr")
library("readr")

# Download files here ---
# states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
#   "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
#   "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
#   "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
#   "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
#   "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
#   "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")

# for(state in names(states)) {
#   dir.create("data/fines_paid")
#   download.file(
#     url = paste0("https://dadosabertos.ibama.gov.br/dados/SICAFI/", state, "/Arrecadacao/arrecadacaobenstutelados.csv"),
#     destfile = paste0("data/fines_paid/", state, "_arrecadacaobenstutelados.csv"),
#     method = "wget", extra = "--no-check-certificate")
#   dir.create("data/fines_given")
#   download.file(
#     url = paste0("https://dadosabertos.ibama.gov.br/dados/SICAFI/", state, "/Quantidade/multasDistribuidasBensTutelados.csv"),
#     destfile = paste0("data/fines_given/", state, "_multasDistribuidasBensTutelados.csv"),
#     method = "wget", extra = "--no-check-certificate")
# }
# dir.create("data/fines_extra")
# download.file(
#   url = paste0("https://dadosabertos.ibama.gov.br/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao.csv"),
#   destfile = paste0("data/fines_extra/infractions.csv"),
#   method = "wget", extra = "--no-check-certificate")

# Read the files ---

files <- list.files("data/fines_given") # These have . as separators and , for decimals
d_fine <- lapply(files, \(f) {
  read_csv2(paste0("data/fines_given/", f), locale = locale(decimal = ",", grouping = "."),
    col_names = c("id_ai", "date", "name", "tax_id", "state", "muni", "fine_type",
      "infraction_type", "legal_frame", "value", "mode", "status", "update"),
    skip = 1, col_types = cols(date = col_date("%d/%m/%Y"))
  )
})
d_fine <- do.call(rbind, d_fine) |>
  filter(mode == "Real", fine_type == "Multa", infraction_type == "Flora", !grepl("Excluído", status))

files <- list.files("data/fines_paid") # These have . as decimals for `value` and , for the rest
d_paid <- lapply(files, \(f) {
  read_delim(paste0("data/fines_paid/", f), delim = ";",
    locale = locale(decimal = ",", grouping = "."),
    col_names = c("id_ai", "name", "tax_id", "state", "muni", "status", "fine_type",
      "infraction_type", "legal_frame", "date", "value", "mode", "payment",
      "payment_nr", "payment_value", "value_paid", "date_paid", "update"),
    skip = 1, col_types = cols(date = col_date("%d/%m/%Y"),
      date_paid = col_date("%d/%m/%Y"), value_paid = col_number())
  )
})
d_paid <- do.call(rbind, d_paid) |>
  filter(mode == "Real", fine_type == "Multa", infraction_type == "Flora", !grepl("Excluído", status))

d_inf <- read_delim("data/fines_extra/infractions.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
d_inf <- d_inf |> transmute(
  id_ai = paste0(NUM_AUTO_INFRACAO, " - ", SER_AUTO_INFRACAO),
  date = as.Date(DAT_HORA_AUTO_INFRACAO, "%d/%m/%Y"),
  name = NOME_INFRATOR, tax_id = CPF_CNPJ_INFRATOR, state = UF,
  muni = MUNICIPIO, area_class = CLASSIFICACAO_AREA, area_quant = QTD_AREA,
  type_infraction = INFRACAO_AREA, desc_infraction = DES_AUTO_INFRACAO,
  desc_legal_frame = DES_INFRACAO)


# Information on fine payment ---

# Fix double-counting in d_paid by getting rid of duplicates
d_paid <- d_paid |>
  mutate(dupl_paid = paste0(id_ai, date_paid, payment, value_paid)) |>
  filter(!duplicated(dupl_paid)) |> select(-dupl_paid)

# Deal with entries where there are multiple names for each id_ai
d_paid |> distinct(id_ai, name) |> filter(duplicated(id_ai))
# There are only four --- we select newer payments manually
d_paid <- d_paid |> filter(
  !(id_ai == "1180 - D" & date == "1998-09-03"),
  !(id_ai == "204726 - D" & date == "1999-08-24"),
  !(id_ai == "9125929 - E" & date == "2018-04-13"),
  !(id_ai == "115996 - D" & date == "2002-12-03"))

# Fix entries with multiple payment_nr by keeping the highest one
paid_id_ai_dupl <- d_paid |> distinct(id_ai, payment_nr) |>
  filter(duplicated(id_ai)) |> pull(id_ai)
paid_id_ai_dupl_select <- d_paid |>
  filter(id_ai %in% paid_id_ai_dupl) |>
  group_by(id_ai, payment_nr) |> arrange(id_ai, date_paid) |>
  summarise(value_paid = sum(value_paid, na.rm = TRUE)) |>
  group_by(id_ai) |>
  slice_max(value_paid, n = 1) |> slice_head(n = 1) |>
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

# In one case the date in d_paid appears to be typo'd --- we fix this manually
pos_wrong_date <- which(d_paid$id_ai == "30823 - B" & d_paid$date_paid == "997-12-30")
pos_right_date <- which(d_paid$id_ai == "30823 - B" & d_paid$date_paid == "1997-12-30")[1]
d_paid[pos_wrong_date, "date_paid"] <- d_paid[pos_right_date, "date_paid"]

# In one case the date of payment is completely off --- we remove it (1225 BRL)
d_paid <- d_paid |> filter(date_paid != "1200-10-05")

# Information on fines in general ---

# In one case the date in d_fine is typo'd for a duplicated ID --- we kick it
d_fine <- d_fine |> filter(!(id_ai == "9128809 - E" & date == "2018-08-16"))

# Deal with duplicated values in d_fine
fine_id_ai_dupl <- d_fine |>
  filter(duplicated(id_ai)) |>
  distinct(id_ai) |> pull()
# We drop entries that appear more than twice (there's 55 unclear ones)
fine_id_ai_dupl_gr2 <- d_fine |>
  filter(id_ai %in% fine_id_ai_dupl) |>
  group_by(id_ai) |>
  count() |> filter(n > 2) |>
  pull(id_ai)
d_fine <- d_fine |> filter(!id_ai %in% fine_id_ai_dupl_gr2)
d_paid <- d_paid |> filter(!id_ai %in% fine_id_ai_dupl_gr2)
# These are the id_ai for fines that show up exactly twice
fine_id_ai_dupl <- fine_id_ai_dupl[which(!fine_id_ai_dupl %in% fine_id_ai_dupl_gr2)]

# These are the dulicated id_ai with different recorded name (e.g. 9050935 - E)
fine_id_ai_name_dupl <- d_fine |>
  filter(id_ai %in% fine_id_ai_dupl) |>
  group_by(id_ai) |>
  count(name) |> filter(n < 2) |>
  distinct(id_ai) |> pull()
# We leave id_ai untouched for those with a payment (in d_paid), others get "n" appended
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
# We append "n" for the older entry when both id_ai & name combinations have no payment
fine_id_ai_dupl <- fine_id_ai_dupl[which(!fine_id_ai_dupl %in% fine_id_ai_name_paid$id_ai)]
fine_id_ai_name_dupl <- d_fine |>
  filter(id_ai %in% fine_id_ai_dupl) |>
  group_by(id_ai) |>
  count(name) |> filter(n < 2) |>
  distinct(id_ai) |> pull(id_ai)
d_fine_dupl_name <- d_fine |>
  filter(id_ai %in% fine_id_ai_name_dupl) |>
  group_by(id_ai) |>
  mutate(id_ai = replace(id_ai, date == min(date), paste0(id_ai[date == min(date)], "n"))) |>
  group_by(id_ai) |> slice_head(n = 1)
d_fine <- d_fine |>
  filter(!id_ai %in% fine_id_ai_name_dupl) |>
  rbind(d_fine_dupl_name)
fine_id_ai_dupl <- fine_id_ai_dupl[which(!fine_id_ai_dupl %in% fine_id_ai_name_dupl)]

# Deal with remaining duplicates -----

# Of the remaining duplicates the most common status is "Cancelado".
d_fine |>
  filter(id_ai %in% fine_id_ai_dupl) |>
  count(status) |>
  arrange(-n) |>
  mutate(share = prop.table(n), share_sum = cumsum(share))

# Here, we deal with duplicates where one entry has "Cancelado" as status ---

# In one case "Cancelado" appears in both --- we fix this manually
d_fine <- d_fine |>
  filter(!(d_fine$id_ai == "9134997 - E" & is.na(d_fine$legal_frame)))
fine_id_ai_dupl <- fine_id_ai_dupl[-which(fine_id_ai_dupl %in% c("9134997 - E"))]

# The rest of "Cancelado" duplicates appears to stem from instances where fines
# are recorded twice after some outstanding fine amount (after payment) was cancelled
fine_id_ai_canc_dupl <- d_fine |>
  filter(id_ai %in% fine_id_ai_dupl, status == "Cancelado") |>
  pull(id_ai)
# Add together payments (if present) and subtract from the original fine to get the cancelled amount
d_paid_canc_dupl <- d_paid |>
  filter(id_ai %in% fine_id_ai_canc_dupl) |>
  group_by(id_ai, tax_id, date, payment_nr) |>
  summarise(value_paid = sum(value_paid, na.rm = TRUE))
d_paid_canc_dupl_date <- d_paid |>
  filter(id_ai %in% fine_id_ai_canc_dupl) |>
  group_by(id_ai, tax_id, date, payment_nr) |>
  slice_max(date_paid, n = 1) |>
  slice_head(n = 1) |>
  rename(date_canc = date_paid)
d_paid_canc_dupl <- d_paid_canc_dupl |>
  left_join(d_paid_canc_dupl_date |> ungroup() |> select(id_ai, date_canc), by = c("id_ai"))
d_fine_dupl_canc <- d_fine |>
  filter(id_ai %in% fine_id_ai_canc_dupl) |>
  arrange(id_ai, date) |>
  left_join(d_paid_canc_dupl |> ungroup() |> select(id_ai, date, value_paid, date_canc), by = c("id_ai", "date")) |>
  # Filter out duplicated entries that have no payments -> they are fully assigned to "Cancelado"
  filter(!(is.na(value_paid) & status != "Cancelado")) |>
  mutate(value_paid = replace(value_paid, is.na(value_paid), 0),
    value = ifelse(status == "Cancelado", value - value_paid, value_paid), # Calculate the cancelled value
    value = replace(value, value < 0, -1), # Adjust "Cancelados" with value_paid > value, we filter them later
    id_ai = replace(id_ai, status == "Cancelado" & value_paid > 0,
      paste0(id_ai[status == "Cancelado" & value_paid > 0], "c")), # Assign a new id to cancelled fines
      date = replace(date, # Replace date for cancellations with the date of the last payment if applicable
        !is.na(date_canc) & status == "Cancelado", date_canc[!is.na(date_canc) & status == "Cancelado"])) |>
  select(-value_paid, -date_canc) |>
  filter(value >= 0) # Filter out cancellations where value_paid > value
d_fine <- d_fine |>
  filter(!id_ai %in% fine_id_ai_canc_dupl) |>
  rbind(d_fine_dupl_canc)
# Remove cases where "Cancelado" is not duplicated, but the value is 0
d_fine <- d_fine |> filter(!(value == 0 & status == "Cancelado"))

# Get the id_ai for those not cancelled
fine_id_ai_dupl <- fine_id_ai_dupl[-which(fine_id_ai_dupl %in% fine_id_ai_canc_dupl)]
# Assess the fines that are still duplicated
fine_val_dupl <- d_fine |> filter(id_ai %in% fine_id_ai_dupl)
fine_val_no_dupl <- d_fine |> filter(!id_ai %in% fine_id_ai_dupl)
# The value of remaining duplicates is ~0.13% of the total fined value
(sum(fine_val_dupl$value) / 2) / sum(fine_val_no_dupl$value)
# ~0.83% of all fines (count) are still duplicated
(nrow(fine_val_dupl) / 2 / nrow(fine_val_no_dupl) )

# Here, we deal with duplicates with "Quitado. Baixa..." as status ---
fine_id_ai_baixa_dupl <- d_fine |>
  filter(id_ai %in% fine_id_ai_dupl, grepl("Quitado. Baixa", status)) |>
  distinct(id_ai) |> pull(id_ai)

# Duplicated entries marked "Quitado. Baixa..." appear to stem from double counting due to a rebate on the fine.
# Thus, the easiest thing to do is to just drop the entries --- rebates will be obvious after matching to d_paid.
d_fine_dupl_baixa <- d_fine |>
  filter(id_ai %in% fine_id_ai_baixa_dupl & grepl("Quitado. Baixa", status)) |>
  arrange(id_ai) |> group_by(id_ai) |>
  slice_head(n = 1)
d_fine <- d_fine |> filter(!id_ai %in% fine_id_ai_baixa_dupl) |>
  rbind(d_fine_dupl_baixa)

# Get the id_ai for those with a different status than "Quitado. Baixa ..."
fine_id_ai_dupl <- fine_id_ai_dupl[-which(fine_id_ai_dupl %in% fine_id_ai_baixa_dupl)]
# Assess the fines that are still duplicated
fine_val_dupl <- d_fine |> filter(id_ai %in% fine_id_ai_dupl)
fine_val_no_dupl <- d_fine |> filter(!id_ai %in% fine_id_ai_dupl)
# The value of remaining duplicates is ~0.12% of the total fined value
(sum(fine_val_dupl$value) / 2) / sum(fine_val_no_dupl$value)
# ~0.38% of all fines (count) are still duplicated
(nrow(fine_val_dupl) / 2 / nrow(fine_val_no_dupl))

# Investigate the next largest duplication type (now in terms of value) ---
d_fine |> filter(id_ai %in% fine_id_ai_dupl) |>
  group_by(status) |>
  summarise(value = sum(value, na.rm = TRUE)) |>
  arrange(-value) |>
  mutate(share = value / sum(value), share_sum = cumsum(share))

# Deal with duplicates with "Para homologação..." as status
fine_id_ai_homol_dupl <- d_fine |>
  filter(id_ai %in% fine_id_ai_dupl, grepl("homologação", status)) |>
  distinct(id_ai) |>
  pull(id_ai)
# Four entries appear with both statuses in d_paid --- we adjust manually
d_paid |> filter(id_ai %in% fine_id_ai_homol_dupl) |>
  distinct(id_ai, status) |> count(id_ai) |> filter(n == 2)
d_fine <- d_fine |> filter(
  !(id_ai %in% "201173 - B" & status == "Baixado por prescrição da pret. punit. (Lei 9873/99,art.1º)"),
  !(id_ai %in% "202714 - B" & status == "Inscrito na dívida ativa"),
  !(id_ai %in% "202721 - B" & status == "Inscrito na dívida ativa"),
  !(id_ai %in% "30375 - D" & status == "Parcelado pela 1ª vez"))
fine_id_ai_homol_dupl <- fine_id_ai_homol_dupl[which(!fine_id_ai_homol_dupl %in%
  c("201173 - B", "202714 - B", "202721 - B", "30375 - D"))]

# We get duplicates with "Para homologação..." as status that appear in d_paid and drop others
fine_id_ai_homol_paid <- d_paid |>
  filter(id_ai %in% fine_id_ai_homol_dupl) |>
  distinct(id_ai, status) |>
  mutate(id_ai_status = paste0(id_ai, status))
d_fine <- d_fine |>
  mutate(id_ai_status = paste0(id_ai, status)) |>
  filter(!(id_ai %in% fine_id_ai_homol_paid$id_ai & !id_ai_status %in% fine_id_ai_homol_paid$id_ai_status)) |>
  select(-id_ai_status)
fine_id_ai_homol_dupl <- fine_id_ai_homol_dupl[which(!fine_id_ai_homol_dupl %in% fine_id_ai_homol_paid$id_ai)]
# We drop those not including "homologação" in the status
d_fine_dupl_homol <- d_fine |>
  filter((id_ai %in% fine_id_ai_homol_dupl & grepl("homologa", status))) |>
  arrange(id_ai, status) |>
  group_by(id_ai) |>
  slice_head(n = 1)
d_fine <- d_fine |>
  filter(!id_ai %in% fine_id_ai_homol_dupl) |>
  rbind(d_fine_dupl_homol)

# Get the id_ai for those that do not have the status "Para homologação..."
fine_id_ai_dupl <- fine_id_ai_dupl[-which(fine_id_ai_dupl %in% fine_id_ai_homol_dupl)]
# Assess the fines that are still duplicated
fine_val_dupl <- d_fine |> filter(id_ai %in% fine_id_ai_dupl)
fine_val_no_dupl <- d_fine |> filter(!id_ai %in% fine_id_ai_dupl)
# The value of remaining duplicates is ~0.056% of the total fined value
(sum(fine_val_dupl$value) / 2) / sum(fine_val_no_dupl$value)
# ~0.32% of all fines (count) are still duplicated
(nrow(fine_val_dupl) / 2 / nrow(fine_val_no_dupl))
paid_num_dupl <- d_paid |> filter(id_ai %in% fine_id_ai_dupl) |> nrow()
# Of the payments, ~0.28% of entries are associated with remaining duplicates
paid_num_dupl / nrow(d_paid)

# Drop the remaining duplicates, given the small number and unclear nature ---
d_fine <- d_fine |> filter(!id_ai %in% fine_id_ai_dupl)
d_paid <- d_paid |> filter(!id_ai %in% fine_id_ai_dupl)


# Join the paid and fined data -----
d_fines <- full_join(
  d_fine |> dplyr::select(id_ai, tax_id, state, muni, date, value, name, status),
  d_paid |> dplyr::select(id_ai, tax_id, state, muni, date, date_paid, value_paid),
  by = c("id_ai", "tax_id", "date", "state", "muni"))

# Remove entries where value_paid is more than ten times the amount fined
id_ai_fines_large <- d_fines |>
  group_by(id_ai, value) |>
  summarise(value_paid = sum(value_paid, na.rm = TRUE)) |>
  filter(value_paid > value * 10) |>
  pull(id_ai)
d_fines <- d_fines |> filter(!id_ai %in% id_ai_fines_large)

# Put 0 for non-first elements with multiple payments, so that fine value is not duplicated
d_fines <- d_fines |>
  arrange(id_ai, date, date_paid, -value_paid) |>
  mutate(value = replace(value, duplicated(id_ai), NA))

# To wrap up, we need to induce concordance for the municipalities (manually and painfully) ---
conc <- read_csv("inst/muni_concordance.csv", col_types = "cccc")
d_fines <- d_fines |> rename(muni_fines = muni, state_fines = state) |>
  left_join(conc, by = c("muni_fines", "state_fines")) |>
  transmute(id_ai, state, muni, date_fined = date, value_fined = value, date_paid, value_paid, status, name) |>
  tidyr::replace_na(list(value_paid = 0))

# There's three nonsensical fine dates that we drop and five for fines paid
d_fines <- d_fines |> filter(date_fined > "1970-01-01" & date_fined < "2025-01-01")
# Same for payments
d_fines <- d_fines |> filter((date_paid > "1970-01-01" & date_paid < "2025-01-01") | is.na(date_paid))
# And 172 with zeros, where the status is not "Cancelado"
fines_id_ai_zeroes <- d_fines |>
  filter((value_fined == 0 & status != "Cancelado")) |>
  distinct(id_ai) |> pull(id_ai)
d_fines <- d_fines |> filter(!id_ai %in% fines_id_ai_zeroes) |>
  tidyr::replace_na(list(value_fined = 0))

# Aggregate to larger categories to make life easier
status_cats <- read.csv("./inst/status_list_cats.csv")
d_fines <- d_fines |> left_join(status_cats |> select(-n) |> rename(status_cat = cat), by = c("status"))

# Save the raw data
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
  filter(id_ai == "9099708 - E" | (name %in%
    c("GUILHERME GALVANE BATISTA", "AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA", "BUNGE ALIMENTOS SA") &
    muni == "Porto Velho")) |>
  group_by(muni, state, year_fined) |>
  summarise(n_outlier_cancelled = n(), brl_outlier_cancelled = sum(value_fined, na.rm = TRUE))

d_fines <- left_join(d_fines_f |> rename(year = year_fined), d_fines_p |> rename(year = year_paid),
  by = c("muni", "state", "year")) |> filter(year > 1999, year < 2022) |>
  left_join(d_fines_c |> rename(year = year_fined), by = c("muni", "state", "year")) |>
  left_join(d_fines_c_outliers |> rename(year = year_fined), by = c("muni", "state", "year"))

# Save the final data
saveRDS(d_fines, "data/fines.rds")
