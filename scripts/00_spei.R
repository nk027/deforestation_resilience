
# Spatial data -----
library("sf")
library("dplyr")
library("raster")
library("ncdf4")

states <- c("AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas",
            "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo",
            "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul",
            "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná",
            "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte",
            "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina",
            "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins")
ufs <- structure(names(states), names = states)
legal_amazon <- c("AC", "AP", "AM",  "MA", "MT", "PA", "RO", "RR", "TO")

# Plain shapefile ---
sh <- read_sf("data/shp") |>
  transmute(muni_id = as.integer(CD_MUN), muni = NM_MUN, state = SIGLA, area_km2 = AREA_KM2)

# SPEI ---

file <- paste0("data/spei/spei03.nc")
# Get info
nc <- ncdf4::nc_open(file)
time <- ncdf4::ncvar_get(nc, "time")
time_units <- ncdf4::ncatt_get(nc, "time", "units")
ncdf4::nc_close(nc)
dates <- as.Date(gsub("^.*([0-9]{4}-[0-9]{1}-[0-9]{1}).*$", "\\1", time_units[[2]])) + time
bands <- which(dates >= "2000-01-01")

# Read and merge NCDF bands
if(require("parallel")) { # Do it in parallel
  n_cores <- detectCores(); cl <- makeCluster(n_cores)
  start <- Sys.time()
  extr_spei <- parLapply(cl, bands, function(x, file, sh) {
    library("raster"); library("sf")
    r <- raster::crop(raster::raster(file, band = x), extent(sh))
    raster::extract(r, sh, df = TRUE, weights = FALSE)
  }, file, sh)
  cat("Calculation finished after", format(Sys.time() - start), "\n")
  stopCluster(cl); detach("package:parallel")
} else { # Pure iterative approach
  extr_spei <- vector("list", length(bands))
  for(i in seq_along(bands)) {
    r <- raster::crop(raster::raster(file, band = bands[i]), extent(sh))
    extr_spei[[i]] <- raster::extract(r, sh, df = TRUE, weights = FALSE)
    cat("Extracted values ", i, " of ", length(bands), ".", sep = "")
  }
}
names(extr_spei) <- dates[bands]

saveRDS(extr_spei, paste0("data/spei/spei03_raw.rds"))
extr_spei <- readRDS("data/spei/spei03_raw.rds")

# Get the list in the right format
spei <- lapply(extr_spei, function(x) {
  x$id <- x[[1]]
  x$value <- x[[2]] * x[[3]] # Weights
  aggregate(value ~ id, data = x, sum)
})
for(i in seq_along(spei)) names(spei[[i]])[2] <- names(spei)[i]
spei <- Reduce(function(x, y) left_join(x, y, by = "id"), spei)

d_spei <- data.frame(id = seq(5572), muni_id = sh$muni_id) |>
  left_join(spei, by = "id") |>
  tidyr::pivot_longer(`2000-01-16`:`2020-12-16`, names_to = "date") |>
  mutate(date = as.Date(date, "%Y-%m-%d"), 
         year = as.integer(format(date, "%Y")), month = as.integer(format(date, "%m")))
saveRDS(d_spei, "data/spei.rds")
