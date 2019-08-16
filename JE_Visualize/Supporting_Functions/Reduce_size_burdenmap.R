# --- NOTE ---
# Use this script to generate Burden_Cases_Map.Rds (which will be used in Tab 4 Shiny)
# Also use this script to generate Incidence Rate or Deaths (instead of Cases) to be used in Tab 4 Shiny
# ---------- #

# Get directory of the script (this part only work if source the code, wont work if run directly in the console)
# This can be set manually !!!
script.dir <- dirname(sys.frame(1)$ofile)
script.dir <- paste0(script.dir, '/')
setwd(script.dir)

# Create folder to store the result (will show warnings if the folder already exists --> but just warning, no problem)
dir.create(file.path('Generate'), showWarnings = TRUE)
Savepath <- 'Generate/'


DataPath.Map <- 'Data/'
FileName.Map <- 'burden_map.rds'
Map <- readRDS(paste0(DataPath.Map, FileName.Map))

Map@data$dist <- NULL
Map@data$admin_level <- NULL
Map@data$value <- NULL

Map@data$value.vaccine <- 0
Map@data$value.unvaccine <- 0

# Exclude these regions: HKG - MAC - SaLa.MYS - Pen.MYS - Sara.MYS
Map <- Map[c(-7, -18, -20, -21, -22),] 

# saveRDS(Map, 'burden_map_cutoff.Rds')
# Map@data$Subnation <- NULL
# Map@data$FOI <- c(1.7, 6.2, 6.2, 6.2, 11.1, 17.8, 26.5, 26.5, 1.7, 14.4, 14.1, 0.1, 
#                   8.7, 4.1, 7.3, 4.0, 7.3, 7.7, 9.0, 8.4, 1.7, 16.5, 26.5, 7.7, 1.7, 1.7, 7.7, 26.5, 6.1, 17.8)/100
# Map@data$id = rownames(Map@data)
# Map.points = fortify(Map, region="id")
# Map.df = join(Map.points, Map@data, by="id")
# ggplot(Map.df) + aes(long, lat, group = group, fill = FOI) + geom_polygon()
# Map.df.w <- Map.df[, c(1, 2, 12)]
# rasterdf <- rasterFromXYZ(Map.df.w)
# writeOGR(obj=Map, dsn = '~/DuyNguyen/', layer="Map", driver="ESRI Shapefile")

library(shiny)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(DT)
library(leaflet)
library(sp)
library(rgdal)
library(rgeos)
library(data.table)
library(plyr)

# ----- LIBRARY FUNCTION -----
Create_Region_Burden_All <- function(cv, cuv, dv, duv, pv, puv, region, agegroup, listtime){
    
    if (region[1] != 'World')
        idx.region <- which(names(cv) %in% region)
    else
        idx.region <- c(1 : length(cv))
    
    list.dt.integrate <- list(cv, cuv, dv, duv)
    list.dt.integrate.result <- lapply(list.dt.integrate, function(x){Reduce('+', x[idx.region])})
    rm(list.dt.integrate)
    list.burden.result <- lapply(list.dt.integrate.result, 
                                 function(x){
                                     # All age group
                                     if (agegroup == 1){
                                         x <- x[, seq(1, ncol(x), 2)] + x[, seq(2, ncol(x), 2)]
                                     }else{
                                         # Children
                                         if (agegroup == 2){
                                             x <- x[, seq(1, ncol(x), 2)]
                                         }else{ # Adult
                                             x <- x[, seq(2, ncol(x), 2)]
                                         }
                                     }
                                     x <- data.frame(x)
                                     colnames(x) <- listtime
                                     x <- melt(x)
                                     colnames(x) <- c('Year', 'Burden_Value')
                                     return(x)
                                 })
    rm(list.dt.integrate.result)
    
    # ----- Find Difference Cases and Deaths in Vacc and Unvacc -----
    list.burden.result[[5]] <- list.burden.result[[2]] - list.burden.result[[1]] # Diff in Cases of Unvacc - Vacc
    list.burden.result[[5]]$Year <- list.burden.result[[1]]$Year
    
    list.burden.result[[6]] <- list.burden.result[[4]] - list.burden.result[[3]] # Diff in Deaths of Unvacc - Vacc
    list.burden.result[[6]]$Year <- list.burden.result[[1]]$Year
    
    # ----- Find IR -----
    if (region[1] != 'World'){
        df.vaccine.region <- pv[which(pv$region %in% region), -ncol(pv)]
        df.unvaccine.region <- puv[which(puv$region %in% region), -ncol(puv)]
    }else{
        df.vaccine.region <- pv[ , -ncol(pv)]
        df.unvaccine.region <- puv[ , -ncol(puv)]
    }
    
    vec.vaccine.region.total <- 0
    vec.unvaccine.region.total <- 0
    
    if (agegroup == 1){
        vec.vaccine.region.total <- as.numeric(colSums(df.vaccine.region))
        vec.unvaccine.region.total <- as.numeric(colSums(df.unvaccine.region))
    }
    
    i <- c(1 : length(region))
    if (agegroup == 2){
        vec.vaccine.region.total <- as.numeric(colSums(df.vaccine.region[rep((i - 1)*100, each = 15) + 1 : 15, ]))
        vec.unvaccine.region.total <- as.numeric(colSums(df.unvaccine.region[rep((i - 1)*100, each = 15) + 1 : 15, ]))
    }
    
    if (agegroup == 3){
        vec.vaccine.region.total <- as.numeric(colSums(df.vaccine.region[rep((i - 1)*100, each = 75) + 16 : 100, ]))
        vec.unvaccine.region.total <- as.numeric(colSums(df.unvaccine.region[rep((i - 1)*100, each = 75) + 16 : 100, ]))
    }
    
    vec.pop.region <- vec.vaccine.region.total + vec.unvaccine.region.total
    
    rm(df.vaccine.region, df.unvaccine.region, vec.unvaccine.region.total, vec.vaccine.region.total)
    
    vec.pop.region <- rep(vec.pop.region, each = nrow(list.burden.result[[1]]) / length(listtime))
    
    t1 <- list.burden.result[[1]]
    t2 <- list.burden.result[[2]]
    t1$Burden_Value <- t1$Burden_Value / vec.pop.region * 100000
    t2$Burden_Value <- t2$Burden_Value / vec.pop.region * 100000
    
    rm(vec.pop.region)
    
    list.burden.result[[7]] <- t1
    list.burden.result[[8]] <- t2
    
    return(list.burden.result)
}

Create_Region_Specific_Burden_All <- function(cv, cuv, dv, duv, pv, puv, region, agegroup, listtime, idx.burden){
    # find mean of burden (cases, deaths, IR) in 2 scenario vaccine and unvaccine for whole time
    # idx.burden = 1 --> cases
    # idx.burden = 2 --> deaths
    # idx.burden = 3 --> IR
    
    list.burden <- Create_Region_Burden_All(cv, cuv, dv, duv, pv, puv, region, agegroup, listtime)
    if (idx.burden == 1){
        bvl.v <- aggregate(list.burden[[1]]$Burden_Value, list(Year = list.burden[[1]]$Year), FUN = mean)
        bvl.uv <- aggregate(list.burden[[2]]$Burden_Value, list(Year = list.burden[[1]]$Year), FUN = mean)
    }else{
        if (idx.burden == 2){
            bvl.v <- aggregate(list.burden[[3]]$Burden_Value, list(Year = list.burden[[1]]$Year), FUN = mean)
            bvl.uv <- aggregate(list.burden[[4]]$Burden_Value, list(Year = list.burden[[1]]$Year), FUN = mean)
        }else{
            if (idx.burden == 3){
                bvl.v <- aggregate(list.burden[[7]]$Burden_Value, list(Year = list.burden[[1]]$Year), FUN = mean)
                bvl.uv <- aggregate(list.burden[[8]]$Burden_Value, list(Year = list.burden[[1]]$Year), FUN = mean)
            }else{
                return(NULL)
            }
        }
    }
    bvl <- cbind(bvl.v, bvl.uv[[2]])
    colnames(bvl) <- c('Year', 'Burden_Value.Vaccine', 'Burden_Value.Unvaccine')
    return(bvl)
}

# ----- Preprocess Tab 2 -----
DataPath.Map <- '../Data/' # Turn back to the data folder of Shiny
# Load Population data
Pop.Total <- readRDS(paste0(DataPath.Map, 'Pop_Total.rds'))
Pop.Unvaccine <- readRDS(paste0(DataPath.Map, 'Pop_UnVaccine.rds'))
Pop.Total <- Pop.Total[ , colnames(Pop.Unvaccine)]
Pop.Vaccine <- Pop.Total[ , -ncol(Pop.Total)] - Pop.Unvaccine[ , -ncol(Pop.Unvaccine)]
Pop.Vaccine$region <- Pop.Total$region

Subregions <- unique(Pop.Unvaccine$region)

Pop.Time <- colnames(Pop.Unvaccine)[-ncol(Pop.Total)] #remove region column name
Pop.Time <- sapply(Pop.Time, function(x){substr(x, 2, 5)})
Pop.Time <- as.numeric(Pop.Time) # Time year

rm(Pop.Total)

# ----- Preprocess Tab 3 -----
Cases.Vaccine <- readRDS(paste0(DataPath.Map, 'vac_cases_agegroup.rds'))
Cases.Unvaccine <- readRDS(paste0(DataPath.Map, 'no_vac_cases_agegroup.rds'))
Deaths.Vaccine <- readRDS(paste0(DataPath.Map, 'vac_deaths_agegroup.rds'))
Deaths.Unvaccine <- readRDS(paste0(DataPath.Map, 'no_vac_deaths_agegroup.rds'))



# ----- PROCESS -----
# if burden is cases --> idx.burden = 1 (2 for deaths, 3 for IR)
idx.burden <- 1
for (i in 1 : length(Map)){
    cat('Processing', as.character(Map@data$Country[i]), '\n')
    list.burden <- Create_Region_Specific_Burden_All(Cases.Vaccine, Cases.Unvaccine, Deaths.Vaccine, Deaths.Unvaccine,
                                                     Pop.Vaccine, Pop.Unvaccine, Map@data$Country[i], 
                                                     agegroup = 1, Pop.Time, idx.burden = idx.burden)
    
    Map@data$value.vaccine[i] <- list(list.burden$Burden_Value.Vaccine)
    Map@data$value.unvaccine[i] <- list(list.burden$Burden_Value.Unvaccine)
}

saveRDS(Map, file = paste0(Savepath, 'Burden_Cases_Map.rds'))

# ----- PLOT -----
# # qt <- quantile(unlist(Map@data$value.vaccine), probs = c(0, 10, 25, 40, 55, 70, 85, 100)/100)
# # qt <- as.numeric(qt)
# # bins <- qt # bin for legend
# bins <- c(0, 2, 4, 6, 8, 10, 12, 20, 30, 40) * 1000
# legend_label <- paste(head(round(bins, 2),-1), tail(round(bins, 2), -1), sep = "-")
# 
# idx <- seq(1, length(unlist(Map$value.vaccine)), 66) # first year
# pal <- colorBin("YlOrRd", domain = unlist(Map$value.vaccine), bins = bins) # color function
# 
# labels <- paste('Region:', Map$Country, "<br/>Burden:", round(unlist(Map$value.vaccine)[idx], 3)) # label for FOI value
# 
# m <- leaflet(Map) %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>% setView(107, 25, 3.25) %>%
#     addPolygons(
#         fillColor = ~pal(unlist(Map$value.vaccine)[idx]), weight = 1, opacity = 1, color = "black",
#         fillOpacity = 1, stroke = T, layerId = ~Country,
#         label = lapply(labels, HTML),
#         labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
#                                     textsize = "16px", direction = "auto", opacity = 0.88),
#         highlightOptions = highlightOptions(color = "blue", weight = 2)
#     ) %>%
#     addLegend(colors = pal((head(bins,-1) + tail(bins, -1))/2), opacity = 1, labels = legend_label,
#               position = "bottomright", title = "Burden") %>%
#     addMiniMap() # add mini map to show zoom area
# m
