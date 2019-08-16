# --- NOTE ---
# Use this script to generate data used for Shiny
#   1. shapefiles_FOI_data_merged_region
#   2. marker_data
#   3. lambda_every_catchment_areas
#   4. Pop_Total
#   5. Pop_UnVaccine
# For the first 3 files --> seperate into 3 different files to have better organization and reduce file sizes. Also refine minor information in the original data
# For the last 2 files --> Extract useful information from the large files
# ---------- #

# Mymensingh 4 division BGD

# Get directory of the script (this part only work if source the code, wont work if run directly in the console)
# This can be set manually !!!
script.dir <- dirname(sys.frame(1)$ofile)
script.dir <- paste0(script.dir, '/')
setwd(script.dir)

# Create folder to store the result (will show warnings if the folder already exists --> but just warning, no problem)
dir.create(file.path('Generate'), showWarnings = TRUE)
Savepath <- 'Generate/'
DataPath <- 'Data/'

# ===== Shp =====
# For shp --> Remove useless information and rename some cities --> better visualization and reduce file size to make it easier to upload to Shiny server
shpmap <- readRDS(paste0(DataPath, 'shapefiles_FOI_data_merged_region.rds'))

# "VN.TT" "VN.KH" "VN.DC" "VN.BP" "VN.DT" "VN.AG" "VN.KG" "VN.CN" "VN.ST" "VN.BL" "VN.CM" "VN.TV"
shpmap@data$subnation[[17]] <- c('Thua Thien Hue', 'Dak Lak', 'Binh Phuoc', 'Dong Thap', 'An Giang',
                                 'Kien Giang', 'Can Tho', 'Soc Trang', 'Bac Lieu', 'Ca Mau', 'Tra Vinh')

# "VN.BG" "VN.HD" "VN.HP" "VN.TB" "VN.TH"
shpmap@data[["subnation"]][[24]] <- c('Bac Giang', 'Hai Duong', 'Hai Phong', 'Thai Binh', 'Thanh Hoa')

temp <- as.character(shpmap@data$region)
temp[45] <- 'BK_Metro'
temp <- as.factor(temp)
shpmap@data$region <- temp
rm(temp)

# ===== Marker =====
# build data for markers
# Use this marker to have better visualization in the map

Marker.Data <- shpmap@data[, c("region", "Centroid_long", "Centroid_lat", "subnation")]
Marker.Data$number <- NA
## The next line is For new data 19/12/2018
Marker.Data$subnation[[15]] <- c('Dhaka', 'Rajshahi', 'Mymensingh', 'Sylhet')
for (i in 1 : nrow(Marker.Data)){
    Marker.Data$number[i] <- length(Marker.Data$subnation[[i]])
}
Marker.Data$subnation = sapply(Marker.Data$subnation, function(x) paste(x,collapse = ", "))
Marker.Data$showonmap <- Marker.Data$subnation

## For old data
# Marker.Data$showonmap[which(Marker.Data$number > 1)] <- c('6 provinces', '4 divisions', 'Northern districts', '13 southern and middle provinces',
#                                                           'Bellary and neighbor districts', '3 counties in Tibet', 'Non-Western Terai districts',
#                                                           '5 northern provinces', 'Northern districts of West Bengal', 'Gorakhpur division',
#                                                           'Western Terai districts', 'Eastern region', 'Central region', 'Kaoping region',
#                                                           'Southern region', 'Taipei region', 'Northern region', 'Kathmandu valley districts')

## For new data 19/12/2018
Marker.Data$showonmap[which(Marker.Data$number > 1)] <- c('28 provinces', '6 provinces', '4 divisions', 'Northern districts', '13 southern and middle provinces',
                                                          'Bellary and neighbor districts', '3 counties in Tibet', 'Non-Western Terai districts',
                                                          '5 northern provinces', 'Northern districts of West Bengal', 'Gorakhpur division',
                                                          'Western Terai districts', 'Eastern region', 'Central region', 'Kaoping region',
                                                          'Southern region', 'Taipei region', 'Northern region', 'Kathmandu valley districts')
Marker.Data$id <- NA
Marker.Data$ISO <- c('CHN', 'IND', 'IDN', 'JPN', 'MYS', 'PHL', 'IND', 'KHM', 'CHN', 'NPL',
                     'IND', 'KOR', 'IND', 'LKA', 'BGD', 'IND', 'VNM', 'IND', 'TWN', 'CHN',
                     'CHN', 'NPL', 'CHN', 'VNM', 'CHN', 'IND', 'LAO', 'CHN', 'IND', 'NPL',
                     'CHN', 'NPL', 'TWN', 'TWN', 'IND', 'CHN', 'TWN', 'IDN', 'TWN', 'TWN',
                     'TWN', 'IND', 'IND', 'IND', 'THA', 'IND', 'NPL', 'THA', 'IND')

Map_ISO <- unique(Marker.Data$ISO)
for (usio in Map_ISO){
    Marker.Data$id[which(Marker.Data$ISO == usio)] <- c(1 : length(which(Marker.Data$ISO == usio)))
}

shpmap$FOI_dist <- NULL
shpmap$subnation <- NULL

# ===== lambda =====
lambda <- readRDS(paste0(DataPath ,'lambda_every_catchment_areas.rds'))
lambda$THA[[2]]$data_for_map$catchment_areas[[1]] <- 'BK_Metro'

saveRDS(shpmap, 'shapefiles_FOI_data_merged_region.rds')
saveRDS(Marker.Data, 'marker_data.rds')
saveRDS(lambda, 'lambda_every_catchment_areas.rds')


# ===== Create Pop_Total and Pop_UnVaccine.Rds =====

region <- c("AUS", "BGD", "BRN", "BTN", "Low.CHN", "High.CHN", "Low.IDN", "High.IDN", "Low.IND", "Medium.IND", "High.IND",
            "JPN", "KHM", "KOR", "LAO", "LKA", "MMR", "MYS", "Low.NPL", "High.NPL", "PAK", "PHL", "PNG", "PRK", "RUS", "SGP",
            "THA", "TLS", "TWN", "VNM")   

poptotal <- readRDS(paste0(DataPath, 'Naive_pop_24ende_1950_2015.rds'))
popunvac <- readRDS(paste0(DataPath, 'After_vac_pop_24ende_or_1950_2015.rds'))

idx_total <- which(poptotal$region %in% region)
Pop_Total <- poptotal[idx_total, ]
idx_totalTWN <- which(poptotal$region %in% 'total_TWN')
idx_TWN <- which(Pop_Total$region %in% 'TWN')
Pop_Total[idx_TWN,-1] <- poptotal[idx_totalTWN,-1]
Pop_Total$region <- as.character(Pop_Total$region)

idx_unvac <- which(popunvac$region %in% region)
Pop_UnVaccine <- popunvac[idx_unvac, ]
idx_totalTWN <- which(popunvac$region %in% 'total_TWN')
idx_TWN <- which(Pop_UnVaccine$region %in% 'TWN')
Pop_UnVaccine[idx_TWN,1:(ncol(Pop_UnVaccine)-1)] <- popunvac[idx_totalTWN,1:(ncol(Pop_UnVaccine)-1)]

saveRDS(Pop_Total, 'Pop_Total.rds')
saveRDS(Pop_UnVaccine, 'Pop_UnVaccine.rds')
