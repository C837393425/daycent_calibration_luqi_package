#!/usr/bin/env Rscript

#' @title Plot NASS Country Mean 
#' @description Script to plot NASS country decade mean for each crop
#' @author Luqi Jiao Emanuele
#' @date May 2026

# Set up environment
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")
library(RMySQL)
library(dplyr)
library(ggplot2) 

cred_file <- path.expand("~/.dblogin")
if (!file.exists(cred_file)) {
  stop("Credential file not found: ", cred_file)
}

cred <- readLines(cred_file, warn = FALSE)
cred <- trimws(cred)
cred <- cred[nzchar(cred)]
if (length(cred) < 2) {
  stop("Credential file must contain at least two non-empty lines (user, password): ", cred_file)
}

user     <- cred[1]
password <- cred[2]


host       = "trillium.nrel.colostate.edu" 
db_nass    = "nass_data"

nass_fips_tbl = "NASS_County_Yld_CropYear"
nass_st_tbl = "NASS_State_Yld_CropYear" 


# NASS data
conn_write = DBI::dbConnect(RMariaDB::MariaDB(), host = host, user = user, password = password, dbname = db_nass) 

nass_fips_data <- DBI::dbGetQuery(
  conn_write,
  paste0("SELECT * FROM ", nass_fips_tbl,  "  ;"))
nass_st_data <- DBI::dbGetQuery(
  conn_write,
  paste0("SELECT * FROM ", nass_st_tbl,  "  ;")) 
 
DBI::dbDisconnect(conn_write)

# calculate the country mean
nass_fips_df <- nass_fips_data %>%
  filter(crop != "corn_silage" & crop != "sorghum_silage") %>% 
  mutate(decade = floor(year / 10) * 10) %>% 
  group_by(crop, decade, yld_unit) %>%
  summarise(mean_yield = mean(tot_yld, na.rm = TRUE))
 
nass_st_df <- nass_st_data %>%
  filter(crop %in% c("tobacco", "sunflower", "drybeans", "onions", "peas", "tomato")) %>% 
  mutate(decade = floor(year / 10) * 10) %>% 
  group_by(crop, decade, yld_unit) %>%
  summarise(mean_yield = mean(tot_yld, na.rm = TRUE))

# merge
combine_df <- rbind(nass_fips_df, nass_st_df)

# plot the country mean
pdf("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/nass_crop_mean_national_all.pdf",
    width = 11, height = 8)
for (crop_val in unique(combine_df$crop)) {

  combine_df_crop <- combine_df %>%
    filter(crop == crop_val) 

  y_unit <- combine_df_crop$yld_unit[1]

  p1 <- ggplot(combine_df_crop, aes(x = decade, y = mean_yield)) +
    geom_line() +
    geom_point() +
    labs(title = paste("NASS National Mean Yield by Decade -", crop_val),
         x = "Decade",
         y = paste0("Mean Yield (", y_unit, ")"))

  print(p1)
}

dev.off()
 
write.csv(combine_df, file = "/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/prep_data/nass_crop_mean_national_all.csv", row.names = FALSE)
