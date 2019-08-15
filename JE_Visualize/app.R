rm(list=ls())

# ----- NOTE -----
# Try plot different y axis in plotly

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
library(future)
plan(multiprocess)

# ----- Tweaks to format layout -----
tweaks <- list(
    tags$head(
        tags$style(
            HTML(
                "
                .multicol {
                height: auto;
                -webkit-column-count: 2; /* Chrome, Safari, Opera */
                -moz-column-count: 2;    /* Firefox */
                column-count: 2;
                -moz-column-fill: auto;
                -column-fill: auto;
                }
                div.radio {margin-top: 0px;}
                "
            )
            )
            )
            )

tweaks_normal <- list(
    tags$head(
        tags$style(
            HTML(
                "
                div.radio {margin-top: 10px;}
                "
            )
            )
            )
        )

# ----- LIBRARY FUNCTION -----
Create_Age_Stratify_DF <- function(vector_pop, age_span = 5){
    # create stratified dataframe from vector_pop, which is a vector population of 0 - 99 years old at 1 year
    # age_span >= 1
    # Input:
    #   - vector_pop: 1D vector population for each age group from 0, 1, 2, ... to 99
    #   - age_span: length of each age group that you want to create (default = 5 --> Age group 0-4, 5-9, 10-14, ...)
    # Ouput:
    #   - df.stratified: dataframe containing 2 columns: age group, and population of that age group
    sa <- seq(0, 99, by = age_span) # lower age
    se <- seq(0 + age_span - 1, 99, by = age_span) # upper age
    agegroup <- paste0(sa, '-', se) # string with format lower age - upper age 
    df.stratified <- data.frame(agegroup = agegroup, pop = 0)
    addition <- rep(100 * seq(1, length(vector_pop) / 100) - 100 , each = age_span)
    for (i in (1 : length(agegroup))){
        df.stratified$pop[i] <- sum(vector_pop[rep(((i - 1)*age_span + 1) : (i*age_span), length(vector_pop) / 100) + addition])
    }
    if (age_span == 1){
        df.stratified$agegroup <- c(0 : (nrow(df.stratified) - 1)) # rename if agegroup = 1 year. Ex: Age group 32-32 (weird format) --> Age group 32
    }
    df.stratified$agegroup <- factor(df.stratified$agegroup, levels = df.stratified$agegroup)
    return(df.stratified)
}

Create_Region_Pop_DF <- function(df.all, listtime, region, year, age_span = 5){
    # create agegroup with age_span dataframe for specific region at specific year from comprehensive df.all
    # listtime is Pop.Time at below
    # region is ISO 3 Code
    # ex: region = 'VNM', year = 1977
    # Input:
    #   - df.all: comprehensive data at each age group, each year, each region
    #   - listtime: list of the provided time (Pop.Time variable as below scripts)
    #   - region: ISO code for the selected region that you want to extract information for (set 'World' if you want to find the total information)
    #   - year: the year that you want to extract information in
    #   - age_span: length of 1 age group
    # Ouput:
    #   - df.region: dataframe containing information of selected region will have 2 columns: age_group and the population in that age_group
    
    if (region[1] != 'World'){
        df.region <- df.all[which(df.all$region %in% region), -ncol(df.all)]
    }else{
        df.region <- df.all[ , -ncol(df.all)]
    }
    
    vec.region <- df.region[[which(listtime == year)]]
    df.region <- Create_Age_Stratify_DF(vec.region, age_span = age_span)
    return(df.region)
}

Create_Region_Coverage_DF <- function(df.vaccine, df.unvaccine, listtime, region, year, age_span = 1){
    # Create dataframe of coverage with age_span at specific region at specific year
    # df.vaccine and df.unvaccine is a full dataframe about vaccinated and unvaccinated population for all years and all regions
    # Idea is to calculate the vaccinated and unvaccinated population at each age group, at the selected region, selected year --> Find the coverage of each age_group
    # Input:
    #   - df.vaccine, df.unvaccinated: information of vaccinated/unvaccinated people (all year, all region, at each year age group)
    #   - listtime, region, year, age_span: same as above functions
    # Output:
    #   - df.coverage.region: coverage at selected region, selected year, for each age group having length of age_span
    
    if (region[1] != 'World'){
        df.vaccine.region <- df.vaccine[which(df.vaccine$region %in% region), -ncol(df.vaccine)]
        df.unvaccine.region <- df.unvaccine[which(df.unvaccine$region %in% region), -ncol(df.unvaccine)]    
    }else{
        df.vaccine.region <- df.vaccine[ , -ncol(df.vaccine)]
        df.unvaccine.region <- df.unvaccine [ , -ncol(df.unvaccine)] 
    }
    
    vec.vaccine.region <- df.vaccine.region[[which(listtime == year)]]
    df.vaccine.region <- Create_Age_Stratify_DF(vec.vaccine.region, age_span = age_span)
    
    vec.unvaccine.region <- df.unvaccine.region[[which(listtime == year)]]
    df.unvaccine.region <- Create_Age_Stratify_DF(vec.unvaccine.region, age_span = age_span)
    
    df.coverage.region <- data.frame(agegroup = df.vaccine.region[[1]], 
                                     coverage = 100 * df.vaccine.region[[2]] / (df.vaccine.region[[2]] + df.unvaccine.region[[2]]))
    
    return(df.coverage.region)
}

Create_Region_Coverage_Time_DF <- function(df.vaccine, df.unvaccine, region, agegroup, listtime){
    # Create dataframe of coverage for all time at specific regions
    # df.vaccine and df.unvaccine is a full dataframe about vaccinated and unvaccinated population for all years and all regions
    # Find the coverage of the selected region(s), selected agegroup, in entire listtime period
    # Input:
    #   - df.vaccine, df.unvaccine, region, listtime: same as above function
    #   - agegroup: 3 options
    #       + 1: entire population
    #       + 2: Children (0 - 14 year old)
    #       + 3: Adult (15 - 99 year old)
    # Output:
    #   - df.coverage.region: dataframe of selected agegroup, region, listtime, will have 4 columns: Year, Coverage portion at each year, vaccinated people at each year, total population at each year
    
    if (region[1] != 'World'){
        df.vaccine.region <- df.vaccine[which(df.vaccine$region %in% region), ]
        df.unvaccine.region <- df.unvaccine[which(df.unvaccine$region %in% region), ]
        numregion <- length(unique(df.vaccine.region[[ncol(df.vaccine.region)]]))
        df.vaccine.region <- df.vaccine.region[, -ncol(df.vaccine.region)]
        df.unvaccine.region <- df.unvaccine.region[, -ncol(df.unvaccine.region)]
    }else{
        df.vaccine.region <- df.vaccine[ , -ncol(df.vaccine)]
        df.unvaccine.region <- df.unvaccine [ , -ncol(df.unvaccine)] 
        numregion <- length(unique(df.vaccine[[ncol(df.vaccine)]]))
    }
    
    rm(df.vaccine, df.unvaccine)
    
    vec.vaccine.region.total <- 0
    vec.unvaccine.region.total <- 0
    
    if (agegroup == 1){
        vec.vaccine.region.total <- as.numeric(colSums(df.vaccine.region))
        vec.unvaccine.region.total <- as.numeric(colSums(df.unvaccine.region))        
    }
    
    if (agegroup == 2){
        for (i in 1 : numregion){
            vec.vaccine.region.total <- vec.vaccine.region.total + as.numeric(colSums(df.vaccine.region[1 : 15 + (i - 1)*100, ]))
            vec.unvaccine.region.total <- vec.unvaccine.region.total + as.numeric(colSums(df.unvaccine.region[1 : 15 + (i-1)*100, ]))
        }
    }
    
    if (agegroup == 3){
        for (i in 1 : numregion){
            vec.vaccine.region.total <- vec.vaccine.region.total + as.numeric(colSums(df.vaccine.region[16 : 100 + (i - 1)*100, ]))
            vec.unvaccine.region.total <- vec.unvaccine.region.total + as.numeric(colSums(df.unvaccine.region[16 : 100 + (i-1)*100, ]))
        }
    }
    
    pop.region <- vec.vaccine.region.total + vec.unvaccine.region.total
    coverage.region <- round(vec.vaccine.region.total / pop.region * 100, 2)
    
    df.coverage.region <- data.frame(Year = listtime, Coverage = coverage.region, 
                                     Vaccinated = round(vec.vaccine.region.total), Population = round(pop.region))
    return(df.coverage.region)
}

Sort_day_study <- function(vector_date){
    # Sort vector_date with specific format to specific order
    # vector_date has format of "mm/yyyy_mm/yyyy"
    # return the ordered index
    d1 <- as.numeric(sapply(vector_date, function(x){substring(x, 1, 2)}))
    d2 <- as.numeric(sapply(vector_date, function(x){substring(x, 4, 7)}))
    d3 <- as.numeric(sapply(vector_date, function(x){substring(x, 9, 10)}))
    d4 <- as.numeric(sapply(vector_date, function(x){substring(x, 12, 15)}))
    result <- order(d2, d1, d4, d3) # sort by first year, then first month, second year, second month
    return(result)
}

# Create_Region_IR <- function(dt.burden, df.vaccine, df.unvaccine, region, agegroup, listtime){
#     # Create dataframe of incidence rate = new cases / total pop * 100000 (per 100000 people) based on agegroup
#     # dt.burden is cases generate 'list'
#     # df.vaccine and df.unvaccine is dataframe of vaccinated and unvaccinated pop in tab 2
#     # region is ISO code of selected region
#     # agegroup is 1, 2, 3 for all, children, adult respectively
#     # listtime is Pop.Time = c(1950:2015)
#     
#     region_burden <- Create_Region_Burden(dt.burden, region, agegroup, listtime) # create dataframe of new cases
#     
#     if (region[1] != 'World'){
#         df.vaccine.region <- df.vaccine[which(df.vaccine$region %in% region), ]
#         df.unvaccine.region <- df.unvaccine[which(df.unvaccine$region %in% region), ]
#         numregion <- length(unique(df.vaccine.region[[ncol(df.vaccine.region)]]))
#         df.vaccine.region <- df.vaccine.region[, -ncol(df.vaccine.region)]
#         df.unvaccine.region <- df.unvaccine.region[, -ncol(df.unvaccine.region)]
#     }else{
#         df.vaccine.region <- df.vaccine[ , -ncol(df.vaccine)]
#         df.unvaccine.region <- df.unvaccine [ , -ncol(df.unvaccine)] 
#         numregion <- length(unique(df.vaccine[[ncol(df.vaccine)]]))
#     }
#     
#     rm(df.vaccine, df.unvaccine)
#     
#     vec.vaccine.region.total <- 0
#     vec.unvaccine.region.total <- 0
#     
#     if (agegroup == 1){
#         vec.vaccine.region.total <- as.numeric(colSums(df.vaccine.region))
#         vec.unvaccine.region.total <- as.numeric(colSums(df.unvaccine.region))        
#     }
#     
#     if (agegroup == 2){
#         for (i in 1 : numregion){
#             vec.vaccine.region.total <- vec.vaccine.region.total + as.numeric(colSums(df.vaccine.region[1 : 15 + (i - 1)*100, ]))
#             vec.unvaccine.region.total <- vec.unvaccine.region.total + as.numeric(colSums(df.unvaccine.region[1 : 15 + (i-1)*100, ]))
#         }
#     }
#     
#     if (agegroup == 3){
#         for (i in 1 : numregion){
#             vec.vaccine.region.total <- vec.vaccine.region.total + as.numeric(colSums(df.vaccine.region[16 : 100 + (i - 1)*100, ]))
#             vec.unvaccine.region.total <- vec.unvaccine.region.total + as.numeric(colSums(df.unvaccine.region[16 : 100 + (i-1)*100, ]))
#         }
#     }
#     
#     vec.pop.region <- vec.vaccine.region.total + vec.unvaccine.region.total
#     
#     rm(df.vaccine.region, df.unvaccine.region, vec.unvaccine.region.total, vec.vaccine.region.total)
#     
#     vec.pop.region <- rep(vec.pop.region, each = nrow(region_burden) / length(listtime))
#     
#     region_burden$Burden_Value <- region_burden$Burden_Value / vec.pop.region * 100000
#     
#     rm(vec.pop.region)
#     
#     return(region_burden)
# }

Create_Region_Burden_All <- function(cv, cuv, dv, duv, pv, puv, region, agegroup, listtime){
    # Create burden (cases, deaths) dataframe of the selected region, selected agegroup, in entire listtime, and in 2 scenarios: vaccination, without vaccination
    # Input:
    #   - cv (cases.vaccine): cases dataframe in scenario: vaccination 
    #   - cuv (cases.unvaccine): cases dataframe in scenario: without vaccination
    #   - dv, duv: death dataframe in 2 scenario: vaccination, without vaccination
    #   - pv, puv: vaccinated people dataframe and not-vaccinated people dataframe
    #   - region, agegroup, listtime: same as above functions
    # Output:
    #   - list.burden.result: list of 6 sub-lists: 2 for cases (in 2 scenarios), 2 for deaths (in 2 scenarios), 1 for averted cases, 1 for averted deaths
    
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
    
    return(list.burden.result)
}

dynamicUI <- function(n) {
    if (n == 0) {
        column(
            10
        )
        # return()
    } else {
        if (n == 1){
            column(
                10,
                plotlyOutput(outputId = 'tab3_plot1', height = '800px') %>% withSpinner(type = 8)
            )
        }else{
            if (n == 2){
                column(
                    10,
                    fluidRow(
                        plotlyOutput(outputId = 'tab3_plot1') %>% withSpinner(type = 8) 
                    ),
                    br(), br(),
                    fluidRow(
                        plotlyOutput(outputId = 'tab3_plot2') %>% withSpinner(type = 8)
                    )
                )
            }else{
                if (n == 3){
                    column(
                        10,
                        fluidRow(
                            column(
                                6,
                                plotlyOutput(outputId = 'tab3_plot1') %>% withSpinner(type = 8) 
                            ),
                            column(
                                6,
                                plotlyOutput(outputId = 'tab3_plot2') %>% withSpinner(type = 8) 
                            )
                        ),
                        br(),
                        fluidRow(
                            column(
                                12,
                                plotlyOutput(outputId = 'tab3_plot3') %>% withSpinner(type = 8)
                            )
                        )
                    )
                }else{
                    column(
                        10,
                        fluidRow(
                            column(
                                6,
                                plotlyOutput(outputId = 'tab3_plot1') %>% withSpinner(type = 8)
                            ),
                            column(
                                6,
                                plotlyOutput(outputId = 'tab3_plot2') %>% withSpinner(type = 8)
                            )
                        ),
                        br(),
                        fluidRow(
                            column(
                                6,
                                plotlyOutput(outputId = 'tab3_plot3') %>% withSpinner(type = 8)
                            ),
                            column(
                                6,
                                plotlyOutput(outputId = 'tab3_plot4') %>% withSpinner(type = 8)
                            )
                        )
                    )
                }
            }
        }
    }
}

# ----- Set Working Directory -----
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# ----- Preprocess Tab 2 -----

# List all iso 3 code (For tab 2 and 3)
Endemic_ISO <- c('AUS', 'BGD', 'BRN', 'BTN', 'CHN', 'IDN', 'IND', 'JPN', 'KHM', 'KOR', 
                 'LAO', 'LKA', 'MMR', 'MYS', 'NPL', 'PAK', 'PHL', 'PNG', 'PRK', 'RUS', 
                 'SGP', 'THA', 'TLS', 'TWN', 'VNM')

# List all endemic countries match Endemic ISO
Endemic_countries <- c('Australia', 'Bangladesh', 'Brunei Darussalam', 'Bhutan', 'China', 'Indonesia',
                       'India', 'Japan', 'Cambodia', 'South Korea', 'Lao PDR', 'Sri Lanka', 'Myanmar',
                       'Malaysia', 'Nepal', 'Pakistan', 'Philippines', 'Papua New Guinea', 'North Korea',
                       'Russian Federation', 'Singapore', 'Thailand', 'Timor-Leste', 'Taiwan', 'Vietnam')

# Sort countries by alpha
order.idx <- order(Endemic_countries)
Endemic_countries <- Endemic_countries[order.idx]
Endemic_ISO <- Endemic_ISO[order.idx]
rm(order.idx)

# Load Population data (total pop and susceptible pop after vaccination)
# Use them to calculate vaccinated = total - susceptible(unvaccine)
Pop.Total <- readRDS('Data/Pop_Total.rds')
Pop.Unvaccine <- readRDS('Data/Pop_UnVaccine.rds')
Pop.Total <- Pop.Total[ , colnames(Pop.Unvaccine)]
Pop.Vaccine <- Pop.Total[ , -ncol(Pop.Total)] - Pop.Unvaccine[ , -ncol(Pop.Unvaccine)]
Pop.Vaccine$region <- Pop.Total$region

Subregions <- unique(Pop.Unvaccine$region)

Pop.Time <- colnames(Pop.Unvaccine)[-ncol(Pop.Total)] #remove region column name
Pop.Time <- sapply(Pop.Time, function(x){substr(x, 2, 5)})
Pop.Time <- as.numeric(Pop.Time) # Time year

rm(Pop.Total)

# ----- Preprocess Tab 1 -----
Shpmap <- readRDS('Data/shapefiles_FOI_data_merged_region.rds') # shapefile of regions --> features on map
Marker.Data <- readRDS('Data/marker_data.rds') # information about the geography of each region --> Use for map visualizing
Lambda.Data <- readRDS('Data/lambda_every_catchment_areas.rds')# catalytic modelled FOI at each regions (organized by country)

bins <- c(0, 0.075, 0.15, 0.225, 0.3, 0.375, 0.45, 0.525) # bin for legend
legend_label <- paste(head(bins,-1), tail(bins, -1), sep = "-") 
pal <- colorBin("YlOrRd", domain = Shpmap$FOI_value, bins = bins) # color function

Map_ISO <- unique(Marker.Data$ISO)
Map_countries <- c()
for (usio in Map_ISO){
    # Marker.Data$id[which(Marker.Data$ISO == usio)] <- c(1 : length(which(Marker.Data$ISO == usio)))
    Map_countries <- c(Map_countries, Endemic_countries[which(Endemic_ISO == usio)])
}

labels <- paste('Region:', Marker.Data$showonmap, "<br/>FOI:", round(Shpmap$FOI_value, 3)) # label for FOI value

order.idx <- order(Map_countries)
Map_countries <- Map_countries[order.idx]
Map_ISO <- Map_ISO[order.idx]
rm(order.idx)

flag <- 0

# ----- Preprocess Tab 3 -----
Cases.Vaccine <- readRDS('Data/vac_cases_agegroup.rds')
Cases.Unvaccine <- readRDS('Data/no_vac_cases_agegroup.rds')
Deaths.Vaccine <- readRDS('Data/vac_deaths_agegroup.rds')
Deaths.Unvaccine <- readRDS('Data/no_vac_deaths_agegroup.rds')


# ----- Preprocess Tab 4 -----
# Burdenmap <- readRDS('Data/Burden_IR_Map.rds') # features on map
Burdenmap <- readRDS('Data/Burden_Cases_Map.rds') # features on map

# --- Devide different legends ---
# qt.vaccine <- quantile(unlist(Burdenmap@data$value.vaccine), probs = c(0, 10, 25, 40, 55, 70, 85, 100)/100)
# qt.vaccine <- as.numeric(qt.vaccine)
# bins.vaccine <- qt.vaccine # bin for legend
# legend_label.vaccine <- paste(head(round(bins.vaccine, 2),-1), tail(round(bins.vaccine, 2), -1), sep = "-") 
# rm(qt.vaccine)
# 
# qt.unvaccine <- quantile(unlist(Burdenmap@data$value.unvaccine), probs = c(0, 10, 25, 40, 55, 70, 85, 100)/100)
# qt.unvaccine <- as.numeric(qt.unvaccine)
# bins.unvaccine <- qt.unvaccine # bin for legend
# legend_label.unvaccine <- paste(head(round(bins.unvaccine, 2),-1), tail(round(bins.unvaccine, 2), -1), sep = "-") 
# rm(qt.unvaccine)
# 
# pal.vaccine <- colorBin("YlOrRd", domain = unlist(Burdenmap$value.vaccine), bins = bins.vaccine) # color function
# pal.unvaccine <- colorBin("YlOrRd", domain = unlist(Burdenmap$value.unvaccine), bins = bins.unvaccine) # color function

# --- same legend IR -----
# burden.vector <- c(unlist(Burdenmap@data$value.vaccine), unlist(Burdenmap@data$value.unvaccine))
# bins.burden <- as.numeric(quantile(burden.vector , probs = c(0, 10, 25, 40, 55, 70, 85, 100)/100))
# legend_label.burden <- paste(head(round(bins.burden, 2),-1), tail(round(bins.burden, 2), -1), sep = "-")
# pal.burden <- colorBin("YlOrRd", domain = burden.vector, bins = bins.burden)
# rm(burden.vector)

# --- same legend Cases -----
burden.vector <- c(unlist(Burdenmap@data$value.vaccine), unlist(Burdenmap@data$value.unvaccine))
bins.burden <- c(0, 2, 4, 6, 8, 10, 12, 20, 30, 70) * 1000
legend_label.burden <- paste(head(round(bins.burden/1000, 2),-1), tail(round(bins.burden/1000, 2), -1), sep = "-")
legend_label.burden[1] <- 'Below 2'
legend_label.burden[length(legend_label.burden)] <- 'Over 30'
pal.burden <- colorBin("YlOrRd", domain = burden.vector, bins = bins.burden)
rm(burden.vector)


tab4.numbaddpolygons <- 0
# tab3.list.plots <- NULL
# ===== UI =====
ui <- navbarPage(
    "My Application", # name application
    # ----- Tab 1: FOI Map -----
    tabPanel(
        "FOI Map", # name of Tab 1
        fluidPage(
            tweaks,
            # titlePanel('FOI INTERACTIVE MAP'),
            fluidRow( # graph + map
                column(
                    6,
                    # ----- leaflet FOI Map -----
                    leafletOutput(
                        outputId = 'tab1_FOIMap',
                        height = 650
                    ) %>% withSpinner(type = 8)
                ),
                column(
                    6,
                    # ----- Select Distribution graph -----
                    plotlyOutput(outputId = 'tab1_hist') %>% withSpinner(type = 8)
                )
            ),
            hr(),
            fluidRow( # input
                column(
                    2,
                    # ----- Select country -----
                    wellPanel(
                        selectInput(
                            inputId = 'tab1_country',
                            h4('Select Country:'),
                            choices = Map_countries,
                            selected = Map_countries[1]
                        )    
                    )
                ),
                column(
                    4,
                    # ----- Select regions after select country -----
                    wellPanel(
                        uiOutput(outputId = 'tab1_uiregion')
                    )
                ),
                column(
                    6,
                    # ----- Display information about regions in country -----
                    uiOutput(outputId = 'tab1_uiregioninfo')    
                )
            )
        )
    ),
    
    # ----- Tab 2: Age distribution -----
    tabPanel(
        "Estimates of Vaccine Coverage", # name of Tab 2
        fluidPage(
            # titlePanel("JE Immunization coverage"),
            
            sidebarLayout(
                # ----- Sidebar -----
                sidebarPanel(
                    # ----- Select country -----
                    selectInput(
                        inputId = 'tab2_country',
                        h4('Select Country:'),
                        choices = c(Endemic_countries, 'World'),
                        selected = Endemic_countries[1]
                    ),
                    
                    # ----- Select region (if it has) -----
                    uiOutput(outputId = 'tab2_uiregion'),
                    
                    # ----- Slide year -----
                    sliderInput(
                        inputId = 'tab2_year',
                        h4('Year of interest'),
                        min = min(Pop.Time),
                        max = max(Pop.Time),
                        value = floor(mean(Pop.Time)),
                        step = 1,
                        sep = '',
                        animate = animationOptions(interval = 650, loop = TRUE)
                    )
                    
                    # ----- Show data Table -----
                    # DT::dataTableOutput(outputId = 'tab2_coveragetable')
                ),
                # ----- MainPanel -----
                mainPanel(
                    # ----- Plot Bar chart -----
                    plotlyOutput(outputId = 'tab2_barchart'),
                    # ----- Plot line chart -----
                    plotlyOutput(outputId = 'tab2_linechart')
                )
            )
        )
    ),
    
    # ----- Tab 3: Burden -----
    tabPanel(
        "Estimates of JE Burden",
        fluidPage(
            # titlePanel('Estimates of JE Burden'),
            # tweaks_normal,
            useShinyjs(),
            fluidRow(
                # ----- Select Country + Region (if it has) + Age group -----
                column(
                    2,
                    wellPanel(
                        selectInput(inputId = 'tab3_country', h4('Select Country:'),
                                    choices = c(Endemic_countries, 'World'), selected = Endemic_countries[1]),
                        uiOutput(outputId = 'tab3_uiregion'),
                        radioButtons(inputId = 'tab3_agegroup', h4('Select Age Group:'), 
                                     choices = c('All' = 1, '0 - 14 year old' = 2, 'Over 14 year old' = 3), selected = 1),
                        checkboxGroupInput(inputId = 'tab3_typeplots', h4('Select Graphs'),
                                           choices = setNames(c(1:4), c('Cases', 'Deaths', 'Averted', 'Vaccinated Individuals')), selected = 1),
                        actionButton(inputId = 'tab3_buttonplot', 'Update Graphs')
                    )
                ),
                # ----- Area for 4 plots -----
                div(id = "placeholder"),
                dynamicUI(0)
            )
        )
    ),
    # ----- Tab 4: Burden Map ----
    tabPanel(
        'Burden Map',
        fluidPage(
            fluidRow(
                column( # For input value (slidebar year, select unvaccine + vaccine, table)
                    3,
                    wellPanel(
                        # ----- Slide year -----
                        sliderInput(
                            inputId = 'tab4_year',
                            h4('Year of interest'),
                            min = min(Pop.Time),
                            max = max(Pop.Time),
                            value = floor(mean(Pop.Time)),
                            step = 1,
                            sep = '',
                            animate = animationOptions(interval = 650, loop = TRUE)
                        ),
                        radioButtons(inputId = 'tab4_scenario', h4('Select Scenario:'), 
                                     choices = c('Past Vaccination' = 1, 'No Vaccination' = 2), selected = 1),
                        DT::dataTableOutput(outputId = 'tab4_burdentable'),
                        actionButton(inputId = 'tab4_buttonupdatetable', 'Update Table')
                    )
                ),
                column( # Leaflet Map for burden value (rememer to set unable to zoom or to move)
                    9,
                    leafletOutput(
                        outputId = 'tab4_BurdenMap',
                        height = 800
                    )
                )
            )
        )
    ),
    # ----- Tab 5: Methodology -----
    tabPanel(
        'Methodology',
        fluidPage(
            # Create structure of methodology tab here
        )
    ),
    # ----- Tab 6: About -----
    tabPanel(
        'About',
        fluidPage(
            # Create structure of about tab here
        )
    )
)

# ===== Server =====
server <- function(input, output, session){
    
    # ---  Tab 1 Process -----
    # ----- Reactive get Map_countries -----
    tab1.getMap_countries <- reactive({
        x <- Map_countries
        return(x)
    })
    
    # ----- Reactive get Map_ISO -----
    tab1.getMap_ISO <- reactive({
        x <- Map_ISO
        return(x)
    })
    
    # ----- Reactive get Marker.Data -----
    tab1.getMarker.Data <- reactive({
        x <- Marker.Data
        return(x)
    })
    
    # ----- Reactive get Lambda.Data -----
    tab1.getLambda.Data <- reactive({
        x <- Lambda.Data
        return(x)
    })
    
    # ----- Reactive get Shpmap -----
    tab1.getShpmap <- reactive({
        x <- Shpmap
        return(x)
    })
    
    # ----- create reactiveVal to trigger click on Map -----
    tab1.flag <- reactiveVal(flag)
    
    # ----- get iso of selected countries -----
    tab1.select_iso <- reactive({
        a <- tab1.getMap_ISO()
        b <- tab1.getMap_countries()
        c <- a[which(b == input$tab1_country)]
        return(c)
    })
    
    # ----- Find indexes of regions (in dataframe) of selected country -----
    tab1.select_subregion <- reactive({
        a <- tab1.getMarker.Data()
        b <- tab1.select_iso()
        grep(b, a$ISO)
    })
    
    # ----- Find index of final selected regions (in dataframe) -----
    tab1.select_final <- reactive({
        idx.region <- tab1.select_subregion()
        idx.idx.region <- as.numeric(input$tab1_radioregion)
        length.idx.region <- length(idx.region)
        if (length.idx.region == 1 || length.idx.region < idx.idx.region){
            idx.region[1]
        }else{
            idx.region[idx.idx.region]
        }
    })
    
    # ----- Add new ui for selecting regions tab1_uiregion -----
    output$tab1_uiregion <- renderUI({
        tab1.idx.region <- tab1.select_subregion()
        isolate({
            tab1.marker.data  <- tab1.getMarker.Data()
            if (tab1.flag() == 0){
                controls <-
                    list(
                        h4('Select Region:'),
                        tags$div(align = 'left',
                                 class = 'multicol',
                                 radioButtons(inputId  = 'tab1_radioregion',
                                              label = NULL,
                                              choices  = setNames(c(1:length(tab1.idx.region)), tab1.marker.data$showonmap[tab1.idx.region]),
                                              selected = 1)
                        )
                    )
            }else{
                tab1.flag(0)
                tab1.shpmap <- tab1.getShpmap()
                tab1.h_light <- input$tab1_FOIMap_shape_click
                tab1.idxselected <- which(tab1.shpmap$region == tab1.h_light$id)
                rm(tab1.shpmap)
                tab1.selectedregion <- tab1.marker.data$id[tab1.idxselected]
                controls <-
                    list(
                        h4('Select Region:'),
                        tags$div(align = 'left',
                                 class = 'multicol',
                                 radioButtons(inputId  = 'tab1_radioregion',
                                              label = NULL,
                                              choices  = setNames(c(1:length(tab1.idx.region)), tab1.marker.data$showonmap[tab1.idx.region]),
                                              selected = tab1.selectedregion)
                        )
                    )
                
            }
        })        
    })
    
    # ----- Add new ui for dispalying regions info tab1_uiregioninfo -----
    output$tab1_uiregioninfo <- renderUI({
        tab1.idx.region <- tab1.select_subregion()
        tab1.marker.data  <- isolate(tab1.getMarker.Data())
        tab1.idx.many <- tab1.idx.region[which(tab1.marker.data$number[tab1.idx.region] > 1)]
        if (length(tab1.idx.many) >= 1){
            tab1.info <- paste0('<strong>Region ', tab1.marker.data$showonmap[tab1.idx.many], ':</strong> ', 
                                tab1.marker.data$subnation[tab1.idx.many])
            tab1.info <- paste0("<div align = \"justify\">",
                                tab1.info,
                                "</div>")
            list(
                wellPanel(
                    h4('Information'),
                    HTML(tab1.info)
                )
            )   
        }
    })
    
    # ----- RenderLeaflet tab1_FOIMap -----
    output$tab1_FOIMap <- renderLeaflet({
        tab1.shpmap <- tab1.getShpmap()
        m <- leaflet(tab1.shpmap, options = leafletOptions(zoomSnap = 0,
                                                           zoomDelta = 0.15)) %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>% setView(107, 25, 3.25) %>%
            addPolygons(
                fillColor = ~pal(FOI_value), weight = 1, opacity = 1, color = "black", 
                fillOpacity = 1, stroke = T, layerId = ~region,
                label = lapply(labels, HTML),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "16px", direction = "auto", opacity = 0.88),
                highlightOptions = highlightOptions(color = "blue", weight = 2)
            ) %>%
            addLegend(colors = pal((head(bins,-1) + tail(bins, -1))/2), opacity = 1, labels = legend_label, 
                      position = "bottomright", title = "FOI")
        m
    })
    
    # ----- Highlight when click on polygon Tab1 observe envent-----
    observeEvent(input$tab1_FOIMap_shape_click, {
        # Stored selected region and subset regional data:
        tab1.h_light <- input$tab1_FOIMap_shape_click
        if(is.null(tab1.h_light))
            return() 
        
        isolate({tab1.flag(1)})  # trigger flag to indicate just have a event click on map
        tab1.shpmap <- tab1.getShpmap()
        tab1.idxselected <- which(tab1.shpmap$region == tab1.h_light$id)
        tab1.selectedPolygon <- tab1.shpmap[tab1.idxselected, ]
        rm(tab1.shpmap)
        
        # Update UI tab1_country and tab1_radioregion
        tab1.marker.data <- tab1.getMarker.Data()
        tab1.map_countries <- tab1.getMap_countries()
        tab1.map_ISO <- tab1.getMap_ISO()
        tab1.selectedcountry <- tab1.marker.data$ISO[tab1.idxselected]
        tab1.selectedcountry <- tab1.map_countries[which(tab1.map_ISO == tab1.selectedcountry)]
        tab1.selectedregion <- tab1.marker.data$id[tab1.idxselected]
        updateSelectInput(session, "tab1_country", selected = tab1.selectedcountry)
        updateRadioButtons(session, 'tab1_radioregion', selected = as.character(tab1.selectedregion))
        
        tab1.pointlng <- tab1.selectedPolygon$Centroid_long
        tab1.pointlat <- tab1.selectedPolygon$Centroid_lat
        tab1.newPoly <- tab1.selectedPolygon@polygons
        tab1.newPoly <- SpatialPolygons(tab1.newPoly)
        tab1.newPoly@proj4string <- tab1.selectedPolygon@proj4string
        rm(tab1.selectedPolygon)
        
        tab1.mapproxy <- leafletProxy("tab1_FOIMap")
        if (!is.null(tab1.mapproxy) && !is.na(tab1.mapproxy)){
            # tab1.mapproxy %>% clearGroup("Selected")
            tab1.mapproxy %>% setView(lng = tab1.pointlng, lat = tab1.pointlat, 4.75)
            # tab1.mapproxy %>% addPolylines(data = tab1.newPoly,
                                           # weight = 3, opacity = 1, color = "green", fillOpacity = 0, stroke = T, group = "Selected")
        }
    })
    
    # ----- Highlight when select region by ui -----
    observeEvent({input$tab1_radioregion
        input$tab1_country}, {
            tab1.idx.final <- tab1.select_final()
            if(!is.na(tab1.idx.final)){
                tab1.shpmap <- tab1.getShpmap()
                tab1.selectedPolygon <- tab1.shpmap[tab1.idx.final, ]
                rm(tab1.shpmap)
                tab1.pointlng <- tab1.selectedPolygon$Centroid_long
                tab1.pointlat <- tab1.selectedPolygon$Centroid_lat
                tab1.newPoly <- tab1.selectedPolygon@polygons
                tab1.newPoly <- SpatialPolygons(tab1.newPoly)
                tab1.newPoly@proj4string <- tab1.selectedPolygon@proj4string
                rm(tab1.selectedPolygon)
                
                tab1.mapproxy <- leafletProxy("tab1_FOIMap")
                if (!is.null(tab1.mapproxy) && !is.na(tab1.mapproxy)){
                    # tab1.mapproxy %>% clearGroup("Selected")
                    tab1.mapproxy %>% setView(lng = tab1.pointlng, lat = tab1.pointlat, 4.75)
                    # tab1.mapproxy %>% addPolylines(data = tab1.newPoly,
                    #                                weight = 3, opacity = 1, color = "green", fillOpacity = 0, stroke = T, group = "Selected")
                }
            }
        })
    
    # ----- Plot Violin graph for each region tab1_hist -----
    output$tab1_hist <- renderPlotly({
        tab1.idx.final <- tab1.select_final()
        tab1.marker.data <- tab1.getMarker.Data()
        tab1.lambda.data <- tab1.getLambda.Data()
        tab1.map.country <- tab1.getMap_countries()
        tab1.map.iso <- tab1.getMap_ISO()
        
        tab1.selectedcountry <- tab1.marker.data$ISO[tab1.idx.final] # get ISO Code
        tab1.selectedcountry.name <- tab1.map.country[which(tab1.map.iso == tab1.selectedcountry)] # name of country
        tab1.selectedregion <- as.character(tab1.marker.data$region[tab1.idx.final]) # get factor region name
        tab1.selectedregion.brief <- tab1.marker.data$showonmap[tab1.idx.final]
        tab1.lambda.data.region <- tab1.lambda.data[[which(names(tab1.lambda.data) == tab1.selectedcountry)]]
        temp.idx <- 1
        for (i in 1 : length(tab1.lambda.data.region)){
            if (tab1.lambda.data.region[[i]]$data_for_map$catchment_areas == tab1.selectedregion){
                temp.idx <- i
                break
            }
        }
        tab1.lambda.data.region <- tab1.lambda.data.region[[temp.idx]]$lambda_extracted
        rm(tab1.marker.data, tab1.lambda.data)
        
        if (length(colnames(tab1.lambda.data.region)) > 1){
            tab1.orderdate <- Sort_day_study(colnames(tab1.lambda.data.region))
            tab1.lambda.data.region <- tab1.lambda.data.region[ , tab1.orderdate]
        }
        tab1.lambda.data.region <- melt(tab1.lambda.data.region)
        colnames(tab1.lambda.data.region) <- c('Date', 'FOI_Value')
        
        m <- list(
            l = 30,
            r = 20,
            b = 30,
            t = 50
        )
        
        p <- plot_ly(type = 'violin', height = 650)
        tab1.date.study <- unique(tab1.lambda.data.region$Date)
        for (i in 1:length(tab1.date.study)){
            temp.sum <- quantile(tab1.lambda.data.region$FOI_Value[tab1.lambda.data.region$Date == tab1.date.study[i]], 
                                 c(0, 0.025, 0.5, 0.975, 1))
            temp.sum <- round(as.numeric(temp.sum), 5)
            textshown <- paste('Max:', temp.sum[5], 
                               '\n</br> 97.5% Quantile:', temp.sum[4], 
                               '\n</br> Median:', temp.sum[3],
                               '\n</br> 2.5% Quantile:', temp.sum[2], 
                               '\n</br> Min:', temp.sum[1])
            p <- add_trace(
                p,
                x = tab1.lambda.data.region$Date[tab1.lambda.data.region$Date == tab1.date.study[i]],
                y = tab1.lambda.data.region$FOI_Value[tab1.lambda.data.region$Date == tab1.date.study[i]],
                line = list (color = '#1f78b4'),
                points = F,
                name = '',
                hoverinfo = 'none',
                spanmode = 'hard'
            )
            
            p <- add_markers(
                p,
                mode = 'markers',
                x = tab1.lambda.data.region$Date[tab1.lambda.data.region$Date == tab1.date.study[i]][1],
                y = seq(temp.sum[1], temp.sum[5], length.out = 50),
                text = textshown,
                marker = list(opacity = 0, color = '#b2df8a'),
                hoverinfo = 'text'
            )
        }
        p <- layout(
            p,
            title = paste0('Country: ', tab1.selectedcountry.name, '\nRegion: ', tab1.selectedregion.brief),
            margin = m,
            xaxis = list(
                title = "Date"
            ),
            yaxis = list(
                title = "FOI Value",
                zeroline = F
            ),
            showlegend = F,
            legend = list(orientation = 'h', xanchor = 'center', yanchor = 'middle', x = 0.5, y = -1)
        )
        p
    })
    
    # --- Tab 2 Process -----
    # ----- Reactive get Endemic_countries -----
    tab2.getEndemic_countries <- reactive({
        x <- Endemic_countries
        return(x)
    })
    
    # ----- Reactive get Endemic_ISO -----
    tab2.getEndemic_ISO <- reactive({
        x <- Endemic_ISO
        return(x)
    })
    
    # ----- Reactive get Subregion -----
    tab2.getSubregions <- reactive({
        x <- Subregions
        return(x)
    })
    
    # ----- Reactive get Pop.Vaccine -----
    tab2.getPop.Vaccine <- reactive({
        x <- Pop.Vaccine
        return(x)
    })
    
    # ----- Reactive get Pop.Unvaccine -----
    tab2.getPop.Unaccine <- reactive({
        x <- Pop.Unvaccine
        return(x)
    })
    
    # ----- Reactive get Pop.Time -----
    tab2.getPop.Time <- reactive({
        x <- Pop.Time
        return(x)
    })
    
    # ----- Find ISO of selected country -----
    tab2.select_iso <- reactive({
        a <- tab2.getEndemic_ISO()
        b <- tab2.getEndemic_countries()
        name.country <- input$tab2_country
        if (name.country == 'World'){
            c <- 'XXX' # define ISO for 'World' is 'XXX'    
        }else{
            c <- a[which(b == input$tab2_country)]    
        }
        return(c)
    })
    
    # ----- Find idx of regions (in dataframe) of selected country -----
    tab2.select_subregion <- reactive({
        iso.country <- tab2.select_iso()
        subregions <- tab2.getSubregions()
        c <- grep(iso.country, subregions) # if 'World' is selected --> c is empty vector
        if (length(c) == 0)
            c <- -1 # # if 'World' is selected --> c is -1
        return(c)
    })
    
    # ----- Add new ui for select region -----
    output$tab2_uiregion <- renderUI({
        tab2.idx.region <- tab2.select_subregion()
        subregions <- tab2.getSubregions()
        if(length(tab2.idx.region) > 1 && tab2.idx.region > 0){
            checkboxGroupInput(inputId = 'tab2_region', h4('Select Regions'),
                               choices = setNames(c(1:length(tab2.idx.region)), subregions[tab2.idx.region]), selected = 1)
        }
    })
    
    # ----- Observe Event for plot tab2 graphs and data table -----
    observeEvent({input$tab2_country
        input$tab2_region
        input$tab2_year},{
            tab2.idx.region <- tab2.select_subregion()
            subregions <- tab2.getSubregions()
            if(length(tab2.idx.region) == 1){ # only country
                if (tab2.idx.region != -1)
                    tab2.region <- subregions[tab2.idx.region] # find region name
                else
                    tab2.region <- 'World' # define name of region of 'World' is 'World'
            }else{# country has several subregions --> use checkboxGroupInput
                if(length(input$tab2_region) == 0){ # do not select any subregions
                    # do nothing
                    output$tab2_barchart <- renderPlotly({})
                    output$tab2_linechart <- renderPlotly({})
                    # output$tab2_coveragetable <- DT::renderDataTable({})
                    return()
                }else{ # select 1 or more subregions
                    tab2.region <- subregions[as.numeric(input$tab2_region) + tab2.idx.region[1] - 1] # find several region name
                }
            }
            pop.time <- tab2.getPop.Time()
            pop.vaccine <- tab2.getPop.Vaccine()
            pop.unvaccine <- tab2.getPop.Unaccine()
            
            # ----- Plot tab 2 barchart -----
            output$tab2_barchart <- renderPlotly({
                # find Pop.Vaccine in selected region and selected year
                df.region.Pop.Vaccine <- Create_Region_Pop_DF(pop.vaccine, pop.time, tab2.region, input$tab2_year, 5)
                # find Pop.Unvaccine in selected region and selected year
                df.region.Pop.Unvaccine <- Create_Region_Pop_DF(pop.unvaccine, pop.time, tab2.region, input$tab2_year, 5)
                # Combine 2 dataframe
                df.region.Pop <- df.region.Pop.Vaccine
                df.region.Pop$Unvaccine <- df.region.Pop.Unvaccine[[2]]
                colnames(df.region.Pop) <-  c('Agegroup', 'Vaccined', 'Unvaccined')
                rm(df.region.Pop.Unvaccine, df.region.Pop.Vaccine)
                # Plotly
                plot_ly(data = df.region.Pop, x = ~Agegroup, y = ~Vaccined, type = 'bar', name = 'Vaccinated') %>%
                    add_trace(y = ~Unvaccined, name = 'Unvaccinated') %>%
                    layout(yaxis = list(title = 'Population'), 
                           xaxis = list(title = 'Age group'),
                           legend = list(x = 0.85, y = 0.9),
                           barmode = 'stack')
            })
            
            # ----- Plot tab 2 linechart -----
            output$tab2_linechart <- renderPlotly({
                df.region.Coverage <- Create_Region_Coverage_DF(pop.vaccine, pop.unvaccine, pop.time, tab2.region, input$tab2_year, 1)
                colnames(df.region.Coverage) <-  c('Age', 'Coverage')
                plot_ly(data = df.region.Coverage, x = ~Age, y = ~Coverage, type = 'scatter', mode = 'lines+markers')%>%
                    layout(yaxis = list(title = 'Vaccinated(%)', range = c(0, 103)), 
                           xaxis = list(title = 'Age', range = c(0, 100)))
            })
            
            # ----- Plot tab 2 coveragetable -----
            # output$tab2_coveragetable <- DT::renderDataTable({
            #     tab2.region.Coverage.Time <- Create_Region_Coverage_Time_DF(pop.vaccine, pop.unvaccine, tab2.region, 1, pop.time)
            #     colnames(tab2.region.Coverage.Time) <- c('Year', 'Coverage (%)', 'Vaccinated People', 'Population')
            #     datatable(
            #         tab2.region.Coverage.Time,
            #         options = list(
            #             orderClasses = TRUE, 
            #             lengthMenu = list(c(10, 30, 50, -1), c('10', '30', '50', 'All')), pageLength = 10, 
            #             scrollY = 500, scrollCollapse = TRUE, bFilter = FALSE
            #         )
            #     )
            # })
        })
    
    # --- Tab 3 Process -----
    # ----- Reactive get Cases.Unvaccine -----
    tab3.getCases.Unvaccine <- reactive({
        x <- Cases.Unvaccine
        return(x)
    })
    
    # ----- Reactive get Cases.Vaccine -----
    tab3.getCases.Vaccine <- reactive({
        x <- Cases.Vaccine
        return(x)
    })
    
    # ----- Reactive get Deaths.Unvaccine -----
    tab3.getDeaths.Unvaccine <- reactive({
        x <- Deaths.Unvaccine
        return(x)
    })
    
    # ----- Reactive get Deaths.Vaccine -----
    tab3.getDeaths.Vaccine <- reactive({
        x <- Deaths.Vaccine
        return(x)
    })
    
    # ----- Find ISO of selected country Tab 3 -----
    tab3.select_iso <- reactive({
        a <- tab2.getEndemic_ISO()
        b <- tab2.getEndemic_countries()
        name.country <- input$tab3_country
        if (name.country == 'World'){
            c <- 'XXX' # define ISO for 'World' is 'XXX'    
        }else{
            c <- a[which(b == input$tab3_country)]    
        }
        return(c)
    })
    
    # ----- Find idx of subregions (in dataframe) of selected country Tab 3-----
    tab3.select_subregion <- reactive({
        iso.country <- tab3.select_iso()
        subregions <- tab2.getSubregions()
        c <- grep(iso.country, subregions) # if 'World' is selected --> c is empty vector
        if (length(c) == 0)
            c <- -1 # # if 'World' is selected --> c is -1
        return(c)
    })
    
    # ----- Add new ui for select region Tab 3-----
    output$tab3_uiregion <- renderUI({
        tab3.idx.region <- tab3.select_subregion()
        subregions <- tab2.getSubregions()
        if(length(tab3.idx.region) > 1 && tab3.idx.region > 0){
            checkboxGroupInput(inputId = 'tab3_region', h4('Select Regions'),
                               choices = setNames(c(1:length(tab3.idx.region)), subregions[tab3.idx.region]), selected = 1)
        }
    })
    
    
    # ----- Get Type of plots -----
    tab3.getTypeplots <- reactive({
        return(as.numeric(input$tab3_typeplots))
    })
    
    # ----- Get Number of plots -----
    tab3.getNumberplots <- reactive({
        return(length(tab3.getTypeplots()))
    })
    
    
    
    # ----- Observe Event to plot 4 tab3 graphs -----
    
    tab3.list.plots <- reactiveValues(data = NULL)
    tab3.calc.finished <- reactiveVal(0)
    
    observeEvent(input$tab3_buttonplot,{
        tab3.calc.finished(0)
        removeUI(selector = "#tab3_plot1", immediate = TRUE)
        removeUI(selector = "#tab3_plot2", immediate = TRUE)
        removeUI(selector = "#tab3_plot3", immediate = TRUE)
        removeUI(selector = "#tab3_plot4", immediate = TRUE)
        removeUI(selector = "#plot_content", immediate = TRUE)
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Calculating", value = 0)
        
        tab3.idx.region <- tab3.select_subregion()
        subregions <- tab2.getSubregions()
        if(length(tab3.idx.region) == 1){ # only country
            if (tab3.idx.region != -1)
                tab3.region <- subregions[tab3.idx.region] # find region name
            else
                tab3.region <- 'World' # define name of region of 'World' is 'World'
        }else{
            if(length(input$tab3_region) == 0){ # do not select any subregions
                showNotification("Have not chosen any regions", type = 'error')
                return()
            }else{ # select 1 or more subregions
                tab3.region <- subregions[as.numeric(input$tab3_region) + tab3.idx.region[1] - 1] # find several region name
            }
        }
        
        tab3.selectplots <- tab3.getTypeplots()
        tab3.numberplots <- tab3.getNumberplots()
        
        if (tab3.numberplots == 0){
            tab3.calc.finished(0)
            showNotification("Have not chosen any graphs", type = 'error')
            return()
        }else{
            removeUI(selector = "#tab3_plot1", immediate = TRUE)
            removeUI(selector = "#tab3_plot2", immediate = TRUE)
            removeUI(selector = "#tab3_plot3", immediate = TRUE)
            removeUI(selector = "#tab3_plot4", immediate = TRUE)
            removeUI(selector = "#plot_content", immediate = TRUE)
            
            insertUI(
                selector = "#placeholder",
                where = "afterEnd",
                ui = div(id = 'plot_content', dynamicUI(tab3.numberplots)),
                immediate = TRUE
            )
        }
        
        cases.unvaccine <- tab3.getCases.Unvaccine()
        cases.vaccine <- tab3.getCases.Vaccine()
        pop.time <- tab2.getPop.Time()
        deaths.unvaccine <- tab3.getDeaths.Unvaccine()
        deaths.vaccine <- tab3.getDeaths.Vaccine()
        pop.vaccine <- tab2.getPop.Vaccine()
        pop.unvaccine <- tab2.getPop.Unaccine()
        
        list.burden <- Create_Region_Burden_All(cases.vaccine, cases.unvaccine, deaths.vaccine, deaths.unvaccine,
                                                pop.vaccine, pop.unvaccine, tab3.region, input$tab3_agegroup, pop.time)
        
        rm(cases.vaccine, cases.unvaccine, deaths.vaccine, deaths.unvaccine)
        
        # ----- Preprocess for plotcases -----
        
        plotly.plotcases <- plot_ly(type = 'box') %>%
            add_boxplot(
                x = list.burden[[2]]$Year, y = list.burden[[2]]$Burden_Value,
                marker = list(opacity = 0, color = '#9e0142'),
                line = list (color = '#9e0142'),
                name = 'No Vaccination'
            ) %>%
            add_boxplot(
                x = list.burden[[1]]$Year, y = list.burden[[1]]$Burden_Value,
                marker = list(opacity = 0, color = '#3288bd'),
                line = list (color = '#3288bd'),
                name = 'Past Vaccination'
            ) %>% layout (
                title = 'Estimated Cases',
                xaxis = list(
                    title = "Year"
                ),
                yaxis = list(
                    title = "Cases",
                    zeroline = F
                ),
                legend = list(orientation = 'h'),
                boxmode = 'group',
                boxgroupgap = 0
            )
        
        progress$set(0.5, detail = "Calculating 25%")
        list.burden[c(1, 2)] <- NULL
        
        # ----- Preprocess for plotdeaths -----
        
        plotly.plotdeaths <- plot_ly(type = 'box') %>%
            add_boxplot(
                x = list.burden[[2]]$Year, y = list.burden[[2]]$Burden_Value,
                marker = list(opacity = 0, color = '#9e0142'),
                line = list (color = '#9e0142'),
                name = 'No Vaccination'
            ) %>%
            add_boxplot(
                x = list.burden[[1]]$Year, y = list.burden[[1]]$Burden_Value,
                marker = list(opacity = 0, color = '#3288bd'),
                line = list (color = '#3288bd'),
                name = 'Past Vaccination'
            ) %>% layout(
                title = 'Estimated Deaths',
                xaxis = list(
                    title = "Year"
                ),
                yaxis = list(
                    title = "Deaths",
                    zeroline = F
                ),
                legend = list(orientation = 'h'),
                boxmode = 'group',
                boxgroupgap = 0
            )
        
        progress$set(0.5, detail = "Calculating 50%")
        list.burden[c(1, 2)] <- NULL
        
        
        # ----- Preprocess for plotdf -----
        
        plotly.plotdf <- plot_ly(type = 'box') %>%
            add_boxplot(
                x = list.burden[[2]]$Year, y = list.burden[[2]]$Burden_Value,
                marker = list(opacity = 0, color = '#7570b3'),
                line = list (color = '#7570b3'),
                name = 'Deaths'
            ) %>%
            add_boxplot(
                x = list.burden[[1]]$Year, y = list.burden[[1]]$Burden_Value,
                marker = list(opacity = 0, color = '#1b9e77'),
                line = list (color = '#1b9e77'),
                name = 'Cases'
            ) %>% layout(
                title = 'Cases and Deaths averted',
                xaxis = list(
                    title = "Year"
                ),
                yaxis = list(
                    title = "People",
                    zeroline = F
                ),
                legend = list(orientation = 'h'),
                boxmode = 'group',
                boxgroupgap = 0
            )
        
        progress$set(1, detail = "Calculating 75%")
        list.burden[c(1, 2)] <- NULL
        
        # ----- Preprocess for plotvr -----
        
        tab3.vr <- Create_Region_Coverage_Time_DF(pop.vaccine, pop.unvaccine, tab3.region, input$tab3_agegroup, pop.time)
        
        plotly.plotvr <- plot_ly(
            data = tab3.vr, x = ~Year, y = ~Vaccinated, 
            type = 'scatter', mode = 'lines+markers') %>% 
            layout(
                title = 'Vaccinated Individuals presented in each year',
                xaxis = list(
                    title = "Year",
                    range = c(1950, 2015)
                ),
                yaxis = list(
                    title = "People"
                )
            )
        
        progress$set(1, detail = "Calculating 100%")
        rm(tab3.vr, pop.unvaccine, pop.vaccine)
        
        # ----- Create list plotly -----
        list.plots <- list(plotly.plotcases, plotly.plotdeaths, plotly.plotdf, plotly.plotvr)
        rm(plotly.plotcases, plotly.plotdeaths, plotly.plotdf, plotly.plotvr)
        
        tab3.list.plots$data <- list.plots[tab3.selectplots]
        rm(list.plots)
        
        showNotification("Calculating: Done", type = 'message')
        
        tab3.calc.finished(1)
    })
    
    observeEvent(tab3.calc.finished(),{
        if (tab3.calc.finished() == 1){
            output$tab3_plot1 <- renderPlotly({
                tab3.list.plots$data[[1]]
            })
            
            output$tab3_plot2 <- renderPlotly({
                if (length(tab3.list.plots$data) >= 2)
                    tab3.list.plots$data[[2]]
            })
            
            output$tab3_plot3 <- renderPlotly({
                if (length(tab3.list.plots$data) >= 3)
                    tab3.list.plots$data[[3]]
            })
            
            output$tab3_plot4 <- renderPlotly({
                if (length(tab3.list.plots$data) >= 4)
                    tab3.list.plots$data[[4]]
            })
        }
    })
    
    # ---  Tab 4 Process -----
    # ----- Reactive get Burdenmap -----
    tab4.getBurdenmap <- reactive({
        return(Burdenmap)
    })
    
    # ----- RenderLeaflet BurdenMap -----
    output$tab4_BurdenMap <- renderLeaflet({
        tab4.burdenmap <- tab4.getBurdenmap()
        
        pop.time <- tab2.getPop.Time()
        isolate(idx.year <- which(pop.time %in% input$tab4_year))
        isolate(scenario <- input$tab4_scenario)
        
        m <- leaflet(tab4.burdenmap, options = leafletOptions(zoomSnap = 0,
                                                              zoomDelta = 0.15)) %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>% setView(107, 25, 3.25) %>% 
            clearShapes() # %>% clearControls()
        tab4.numbaddpolygons <<- 0
        
        m <- m %>% addLegend(colors = pal.burden((head(bins.burden,-1) + tail(bins.burden, -1))/2), opacity = 1, labels = legend_label.burden, 
                             position = "bottomright", title = "Cases (thousand)")
        
        if (scenario == 1){ # vaccine scenario
            idx <- seq(idx.year, length(unlist(tab4.burdenmap$value.vaccine)), 66) # selected year --- 66 is total length of pop.time
            labels <- paste('Region:', tab4.burdenmap$Country, "<br/>Burden:",
                            round(unlist(tab4.burdenmap$value.vaccine)[idx], 3)) # label for burden value
            m <- m %>% addPolygons(
                fillColor = ~pal.burden(unlist(tab4.burdenmap$value.vaccine)[idx]), weight = 1, opacity = 1, color = "black",
                fillOpacity = 1, stroke = T, layerId = ~Country,
                label = lapply(labels, HTML),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "16px", direction = "auto", opacity = 0.88),
                highlightOptions = highlightOptions(color = "blue", weight = 2)
            ) #%>%
            # addLegend(colors = pal.vaccine((head(bins.vaccine,-1) + tail(bins.vaccine, -1))/2), opacity = 1, labels = legend_label.vaccine,
            # position = "bottomright", title = "Burden")
            
        }else{ # unvaccine scenario
            idx <- seq(idx.year, length(unlist(tab4.burdenmap$value.unvaccine)), 66) # selected year --- 66 is total length of pop.time
            labels <- paste('Region:', tab4.burdenmap$Country, "<br/>Burden:",
                            round(unlist(tab4.burdenmap$value.unvaccine)[idx], 3)) # label for burden value
            m <- m %>% addPolygons(
                fillColor = ~pal.burden(unlist(tab4.burdenmap$value.unvaccine)[idx]), weight = 1, opacity = 1, color = "black",
                fillOpacity = 1, stroke = T, layerId = ~Country,
                label = lapply(labels, HTML),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "16px", direction = "auto", opacity = 0.88),
                highlightOptions = highlightOptions(color = "blue", weight = 2)
            ) #%>%
            # addLegend(colors = pal.unvaccine((head(bins.unvaccine,-1) + tail(bins.unvaccine, -1))/2), opacity = 1, labels = legend_label.unvaccine,
            #           position = "bottomright", title = "Burden")
        }
        rm(idx.year, idx, pop.time, labels)
        m
    })
    
    # ----- Changing No / Past Vaccination -----
    observeEvent(input$tab4_scenario,{
        tab4.burdenmap <- tab4.getBurdenmap()
        pop.time <- tab2.getPop.Time()
        isolate(idx.year <- which(pop.time %in% input$tab4_year))
        scenario <- input$tab4_scenario
        tab4.mapproxy <- leafletProxy("tab4_BurdenMap", data = tab4.burdenmap)
        if (scenario == 1){ # vaccine scenario
            idx <- seq(idx.year, length(unlist(tab4.burdenmap$value.vaccine)), 66) # selected year --- 66 is total length of pop.time
            labels <- paste('Region:', tab4.burdenmap$Country, "<br/>Burden:", 
                            round(unlist(tab4.burdenmap$value.vaccine)[idx], 3)) # label for burden value
            tab4.mapproxy <- tab4.mapproxy %>% addPolygons(
                fillColor = ~pal.burden(unlist(tab4.burdenmap$value.vaccine)[idx]), weight = 1, opacity = 1, color = "black", 
                fillOpacity = 1, stroke = T, layerId = ~Country,
                label = lapply(labels, HTML),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "16px", direction = "auto", opacity = 0.88),
                highlightOptions = highlightOptions(color = "blue", weight = 2) 
            ) #%>%
            # addLegend(colors = pal.vaccine((head(bins.vaccine,-1) + tail(bins.vaccine, -1))/2), opacity = 1, labels = legend_label.vaccine, 
            # position = "bottomright", title = "Burden")
            
        }else{ # unvaccine scenario
            idx <- seq(idx.year, length(unlist(tab4.burdenmap$value.unvaccine)), 66) # selected year --- 66 is total length of pop.time
            labels <- paste('Region:', tab4.burdenmap$Country, "<br/>Burden:", 
                            round(unlist(tab4.burdenmap$value.unvaccine)[idx], 3)) # label for burden value
            tab4.mapproxy <- tab4.mapproxy %>% addPolygons(
                fillColor = ~pal.burden(unlist(tab4.burdenmap$value.unvaccine)[idx]), weight = 1, opacity = 1, color = "black", 
                fillOpacity = 1, stroke = T, layerId = ~Country,
                label = lapply(labels, HTML),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "16px", direction = "auto", opacity = 0.88),
                highlightOptions = highlightOptions(color = "blue", weight = 2)
            ) #%>%
            # addLegend(colors = pal.unvaccine((head(bins.unvaccine,-1) + tail(bins.unvaccine, -1))/2), opacity = 1, labels = legend_label.unvaccine, 
            #           position = "bottomright", title = "Burden")
        }
        rm(idx.year, idx, pop.time, labels)
        tab4.mapproxy
    })
    
    # ----- Animation of Sidebar tab4_year -----
    observeEvent(input$tab4_year,{
        tab4.burdenmap <- tab4.getBurdenmap()
        pop.time <- tab2.getPop.Time()
        idx.year <- which(pop.time %in% input$tab4_year)
        isolate(scenario <- input$tab4_scenario)
        tab4.mapproxy <- leafletProxy("tab4_BurdenMap", data = tab4.burdenmap)
        if (!is.null(tab4.mapproxy) && !is.na(tab4.mapproxy)){
            tab4.numbaddpolygons <<- tab4.numbaddpolygons + 1
            if (tab4.numbaddpolygons == 132){
                tab4.numbaddpolygons <<- 0
                tab4.mapproxy <- tab4.mapproxy %>% clearShapes()
            }
            if (scenario == 1){ # vaccine scenario
                idx <- seq(idx.year, length(unlist(tab4.burdenmap$value.vaccine)), 66) # selected year --- 66 is total length of pop.time
                labels <- paste('Region:', tab4.burdenmap$Country, "<br/>Burden:", 
                                round(unlist(tab4.burdenmap$value.vaccine)[idx], 3)) # label for burden value
                tab4.mapproxy <- tab4.mapproxy %>% 
                    addPolygons(
                        fillColor = ~pal.burden(unlist(tab4.burdenmap$value.vaccine)[idx]), weight = 1, opacity = 1, color = "black", 
                        fillOpacity = 1, stroke = T, layerId = ~Country,
                        label = lapply(labels, HTML),
                        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                    textsize = "16px", direction = "auto", opacity = 0.88),
                        highlightOptions = highlightOptions(color = "blue", weight = 2)
                    )
            }else{ # unvaccine scenario
                idx <- seq(idx.year, length(unlist(tab4.burdenmap$value.unvaccine)), 66) # selected year --- 66 is total length of pop.time
                labels <- paste('Region:', tab4.burdenmap$Country, "<br/>Burden:", 
                                round(unlist(tab4.burdenmap$value.unvaccine)[idx], 3)) # label for burden value
                tab4.mapproxy <- tab4.mapproxy %>%
                    addPolygons(
                        fillColor = ~pal.burden(unlist(tab4.burdenmap$value.unvaccine)[idx]), weight = 1, opacity = 1, color = "black", 
                        fillOpacity = 1, stroke = T, layerId = ~Country,
                        label = lapply(labels, HTML),
                        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                    textsize = "16px", direction = "auto", opacity = 0.88),
                        highlightOptions = highlightOptions(color = "blue", weight = 2)
                    )
            }
            rm(idx.year, idx, pop.time, labels)
            tab4.mapproxy
        }
    })
    
    # ----- Render burdentable -----
    observeEvent(input$tab4_buttonupdatetable,{
        tab4.burdenmap <- tab4.getBurdenmap()
        pop.time <- tab2.getPop.Time()
        idx.year <- which(pop.time %in% input$tab4_year)
        idx <- seq(idx.year, length(unlist(tab4.burdenmap$value.vaccine)), 66) # selected year --- 66 is total length of pop.time
        value.vaccine <- round(unlist(tab4.burdenmap$value.vaccine)[idx], 3)
        value.unvaccine <- round(unlist(tab4.burdenmap$value.unvaccine)[idx], 3)
        country <- as.character(tab4.burdenmap$Country)
        databurden <- data.frame(Region = country, Vaccine = value.vaccine, Unvaccine = value.unvaccine)
        rm(value.vaccine, value.unvaccine, country)
        colnames(databurden) <- c('Region', 'Past Vaccination', 'No Vaccination')
        output$tab4_burdentable <- DT::renderDataTable({
            datatable(
                databurden,
                options = list(
                    orderClasses = TRUE, 
                    lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')), pageLength = 10, 
                    scrollY = 500, scrollCollapse = TRUE, bFilter = FALSE
                )
            )
        })
    })
    
}

# ===== Run =====
shinyApp(ui, server)

