# find the % of crop in LRR_M for crop calibraiton

# Set up environment
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")
library(RMySQL)
library(dplyr)
library(readr)
library(ggplot2)


cred_file <- "~/.dblogin"
cred <- readLines(cred_file)

user     <- cred[1]
password <- cred[2]


host       = "trillium.nrel.colostate.edu"
db_nri     = "nri2017"
db_nass    = "nass_data"

conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nri)

# load in crop table
site_tbl <- dbGetQuery(
  conn_write,
  paste0("select recordid2017
          from INV_lookup_point_site_info where LRR_MLRA2022 like 'M' and tier_2023 = 3;") )

crop_tbl <- dbGetQuery(
  conn_write,
  paste0("select chist_landuse, recordid2017 from CHIST_INV2024_preLandRep_FINAL_vert_06182025;") )


combine <- crop_tbl %>%
  filter(recordid2017 %in% site_tbl$recordid2017) %>%
  group_by(recordid2017, chist_landuse) %>%
  distinct() %>%
  ungroup() %>%
  group_by(chist_landuse) %>%
  mutate(n_pts = n()) %>%
  distinct(chist_landuse, n_pts) %>%
  ungroup() %>%
  reframe(chist_landuse, n_pts, pct_pts = (n_pts/sum(n_pts)) * 100)

# plot
ggplot(combine, aes(x = reorder(chist_landuse, pct_pts), y = pct_pts)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Percent of Points by Crop Type",
    x = "Crop Type",
    y = "Percentage of Points by Year"
  ) +
  theme_minimal()


print(combine[order(-combine$pct_pts), ], n = 36)


#   chist_landuse                          n_pts   pct_pts
#   <chr>                                  <int>     <dbl>
#  1 Row/Soybeans                           69283 21.04     
#  2 Row/Corn for grain                     69161 21.00     
#  3 Close/Wheat                            28387  8.62    
#  4 Hay/Grass plant year                   19278  5.85    
#  5 Hay/Grass harvest year                 18637  5.66    
#  6 Pasture/Grass graze year               17197  5.22    
#  7 Close/Oats                             14693  4.46    
#  8 Pasture/Grass plant year               10708  3.25    
#  9 Other crop/Other-setaside etc           9864  3.00    
# 10 Hay/Legume plant year                   8796  2.67    
# 11 Row/Sorghum for grain                   8629  2.62    
# 12 Hay/Legume harvest year                 8177  2.48    
# 13 Other farmland/CRP land                 6354  1.93    
# 14 Other crop/Summer fallow                5897  1.79    
# 15 Pasture/Grass-forbs-legumes graze year  5749  1.75    
# 16 Row/Corn for silage                     5009  1.52    
# 17 Pasture/Grass-forbs-legumes plant year  3592  1.09    
# 18 Hay/Legume-grass harvest year           3174  0.964   
# 19 Pasture/Legume graze year               2836  0.861   
# 20 Pasture/Legume plant year               2470  0.750   
# 21 Rangeland                               2375  0.721   
# 22 Close/All other close grown             2361  0.717   
# 23 Hay/Legume-grass plant year             1729  0.525   
# 24 Close/Barley                            1485  0.451   
# 25 Row/Other veg/Peas                      1118  0.339   
# 26 Row/Sunflower                            732  0.222   
# 27 Row/Sugar beets                          492  0.149   
# 28 Row/Other veg/Dry Beans                  392  0.119   
# 29 Row/Sorghum for silage                   288  0.0875  
# 30 Row/Potatoes                             162  0.0492  
# 31 Row/Tobacco                              144  0.0437  
# 32 Row/Other veg/Tomatoes                    95  0.0288  
# 33 Row/Peanuts                               21  0.00638 
# 34 Close/Rice                                18  0.00547 
# 35 Row/Cotton                                16  0.00486 
# 36 Row/Other veg/Onions                       1  0.000304





