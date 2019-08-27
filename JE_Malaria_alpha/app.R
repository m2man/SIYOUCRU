# Note
# Mapping predicted treatment failure with and without including Sample size

rm(list=ls())

## ----- INCLUDE LIBRARIES -----
library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.minicharts)
library(sp)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(readxl)
library(htmltools)
library(htmlwidgets)
library(shinyjs)
library(V8)

## ----- Read data -----
# ========== ADJUST HERE ==========
# Link to the data (.xlsx format) which is located in the Data Folder
# Should make sure new data file has the same format with the Data_v2.xlsx
data <- read_xlsx('Data/Data_v2.xlsx')
# data <- data[-1, ] # need to view data first to decide whether should run this line or not

# threshold of sample size to decide which marker is big and small (sample size > thresh --> big marker)
# big marker will have text of information inside it, small marker will have text outside
thresh <- 10

# Parameters for formular adjusting size of markers
# formular for deciding the size of marker: par1 * sqrt[sample_size / min(sample_size)] + par2
# par1 is the parameter in the slider bar appearing on the user interface (UI), par2 is manually set here (default is 0)
par2 <- 0

# Color Palette for plotting treatment failure ("RdYlGn" is one of various pallete themes)
# Can find more pallete here: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes/
# "rev" is used to reverse the color pallete (default of RdYlGn is Red < Yellow < Green). 
# But is our case, Green is lowest, and Red is highest --> have to use "rev" function
palette <- rev(brewer.pal(11, "RdYlGn"))

# ========== SHOULD NOT TOUCH THESE CODE BELOW ==========
colnames(data) <- c('Location', 'Latitude', 'Longitude', 'Total_Sample', 'Predicted_Failure') # Rename column to read easily
data$Location_Code <- substr(data$Location, 1, 5) # add new Code columnto show on Map

# Convert to numeric -- Original is character
data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)
data$Total_Sample <- as.numeric(data$Total_Sample)
data$Predicted_Failure <- as.numeric(data$Predicted_Failure) * 100

# define which marker is big (more than "thres" sample)
data.big <- data[which(data$Total_Sample > thresh), ]
data.small <- data[which(data$Total_Sample <= thresh), ]

# Color Palette for plotting treatment failure (can google for more color theme, "RdYlGn" is one of various pallete themes)
# palette <- rev(brewer.pal(11, "RdYlGn"))
pal <- colorNumeric(palette, domain = data$Predicted_Failure) # color function

# Read Map to bold boundary
lv1 <- readRDS('Map/VN_0.rds')
lv1.tf <- spTransform(lv1, lv1@proj4string)
rm(lv1)

# Content Popup --> show detail if user click on markers on Map
content_popup <- paste0("<p style = \"font-size: 13px\">",
                        "<span style = \"color:red\">Location:</span> <b>", data$Location_Code, "</b><br/>",
                        "<span style = \"color:red\">Total Sample:</span><b> ", data$Total_Sample, "</b><br/>",
                        "<span style = \"color:red\">Treatment Failure:</span><b> ", data$Predicted_Failure, "%</b><br/>",
                        "</p>"
)

# ----- JS Code for exporting to PDF (DO NOT TOUCH) -----
jsCode <- "
    shinyjs.preparetoexport = function(params){
        var defaultParams = {
            id : null
        };
        params = shinyjs.getParams(params, defaultParams);
        $(params.id).appendTo('body');
        $('body > :not(' + params.id + ')').toggle(200);
    }
        
    shinyjs.export = function(){
        window.setTimeout(function(){
        window.print();
        }, 300);
    }
        
    shinyjs.afterexport = function(params){
        var defaultParams = {
            id : null
        };
        params = shinyjs.getParams(params, defaultParams);
        $('body > :not(' + params.id + ')').toggle(200);
        $(params.id).appendTo('.col-sm-10');
    }
        
    shinyjs.refresh = function(params){
        var defaultParams = {
            id : null
        };
        params = shinyjs.getParams(params, defaultParams);
        $(params.id).appendTo('body');
        $('body > :not(' + params.id + ')').toggle(200);
        window.print();
        window.close();
        window.setTimeout(function(){
            $('body > :not(' + params.id + ')').toggle(200);
            $(params.id).appendTo('.col-sm-10');
        }, 2000);
    }
"

## ----- Start Shiny -----
## ----- Define ui --> how to see the program -----
ui <- fluidPage(
    titlePanel('Malaria Interactive Map'), # Set name of the Panel
    
    # ----- Add Shinyjs to run js in Shiny -----
    useShinyjs(),
    extendShinyjs(text = jsCode),
    tags$head(
        # Include our custom CSS
        includeCSS("style.css")
    ),
    
    # ----- Design UI of the page -----
    fluidRow(
        # ----- Side Area for setting values -----
        column(
            2,
            tags$div(
                class = 'sidearea',
                wellPanel(
                    # ----- Radio Buttons for selecting w or wo including Sample Size -----
                    radioButtons(inputId = 'choose_sample_or_not', h4('Select Type:'), 
                                 choices = c('Without Sample Size' = 1, 'With Sample Size' = 2), selected = 1),
                    # ----- ConditionalPanel for displaying Slider for adjusting marker size if chosen with sample size -----
                    conditionalPanel(
                        condition = "input.choose_sample_or_not == 2",
                        sliderInput(
                            inputId = 'slider_adjust_size',
                            h4('Adjust Size of Markers'),
                            min = 1, max = 5, value = 2, step = 0.25, sep = ''
                        )
                    ),
                    fluidRow(
                        column(
                            6,
                            # ----- Button for exporting to PDF -----
                            actionButton(inputId = 'btn_export', 'Export to PDF')
                        )
                    )
                )
            )
        ),# ----- Map Area for displaying Map -----
        column(
            10,
            tags$div(
                class = "maparea",
                # ----- Leaflet output for displaying map -----
                leafletOutput(outputId = 'map', height = 845),
                # ----- Panel for displaying own legend -----
                absolutePanel(id = 'mylegend', class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              right = '2%', width = 75,
                              top = '8%', height = 150,
                              plotOutput(outputId = 'legend', height = '100%')),
                # ----- Conditional Panel for displaying Scalebar if chosen with sample size -----
                conditionalPanel(
                    condition = "input.choose_sample_or_not == 2",
                    absolutePanel(
                        id = "scalebar", class = "panel panel-default", fixed = TRUE, draggable = FALSE, 
                        left = "auto", right = '2%', bottom = '5%',
                        width = 275, height = 120,
                        h4('Sample Scale Bar'),
                        fluidRow(
                            column(
                                1,
                                h5('0')
                            ),
                            column(
                                2,
                                offset = 0,
                                uiOutput(outputId = 'sb_1')
                            ),
                            column(
                                2,
                                offset = 0,
                                uiOutput(outputId = 'sb_2')
                            ),
                            column(
                                2,
                                offset = 0,
                                uiOutput(outputId = 'sb_3')
                            ),
                            column(
                                2,
                                offset = 0,
                                uiOutput(outputId = 'sb_4')
                            ),
                            column(
                                2,
                                offset = 0,
                                uiOutput(outputId = 'sb_5')
                            )
                        ),
                        img(src='scalebar.jpg', width = '100%')
                    )
                )
            )
        )
    )
)

## ----- Define servers function --> how to interact with the map -----
server <- function(input, output, session){
    
    # --- Set value for 1st parameter (main contributor for scaling) -----
    par1 <- reactiveVal(10)
    
    # --- Find 6 milestone for scalebar -----
    maxdistance <- reactive({
        d <- seq(0,200,length.out = 6)
        d = ((d/2 - par2)/par1())^2 * min(data$Total_Sample)
        return(d)
    })
    
    # --- Render UI for 6 milestones in scalebar -----
    output$sb_1 <- renderUI({
        d <- maxdistance()
        h5(round(d[2]))
    })
    
    output$sb_2 <- renderUI({
        d <- maxdistance()
        h5(round(d[3]))
    })
    
    output$sb_3 <- renderUI({
        d <- maxdistance()
        h5(round(d[4]))
    })
    
    output$sb_4 <- renderUI({
        d <- maxdistance()
        h5(round(d[5]))
    })
    
    output$sb_5 <- renderUI({
        d <- maxdistance()
        h5(round(d[6]))
    })
    
    # --- Plot UI for displaying legend (have to create own legend) -----
    output$legend <- renderPlot({
        par(mar=c(1, 0, 2, 0))
        legend_image <- as.raster(matrix(pal(0:100), ncol=1))
        plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Failure')
        text(x=1.5, y = seq(0,1,l=5), labels = paste0(seq(100,0,l=5), '%'))
        rasterImage(legend_image, 0.5, 0, 1, 1)
    })
    
    # --- Displaying leaflet -----
    output$map <- renderLeaflet({
        m <- leaflet(data, 
                     options = leafletOptions(minZoom = 6,
                                              zoomSnap = 0,
                                              zoomDelta = 0.15)) %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
            setView(107, 14, 7) %>%
            addPolygons(data = lv1.tf, color = 'black', weight = 3, fill = F)
        m
    })
    
    # --- Adjust marker if change Slider Marker Size -----
    observeEvent(input$slider_adjust_size,{
        par1(as.numeric(input$slider_adjust_size))
        mapproxy <- leafletProxy("map", data = data)
        mapproxy <- mapproxy %>% clearMarkers() %>%
            addCircleMarkers (
                lat = data.big$Latitude, lng = data.big$Longitude,
                radius =  par1() * sqrt(data.big$Total_Sample / min(data$Total_Sample)) + par2,
                weight = 1, color = "#000000",
                fillColor = pal(data.big$Predicted_Failure), 
                fillOpacity = 0.75, 
                label = lapply(paste0("<b>",as.character(data.big$Predicted_Failure), '%</b>'), HTML),
                labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE, textsize = '12px')
            ) %>%
            addCircleMarkers(
                lat = data.small$Latitude, lng = data.small$Longitude,
                radius = par1() * sqrt(data.small$Total_Sample / min(data$Total_Sample)) + par2,
                weight = 1, color = "#000000",
                fillColor = pal(data.small$Predicted_Failure), 
                fillOpacity = 0.75, 
                label = lapply(paste0("<b>",as.character(data.small$Predicted_Failure), '%</b>'), HTML),
                labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                            offset=c(0,4),
                                            style = list(
                                                "font-size" = "12px"
                                            ))
            )
        mapproxy
    })
    
    # --- Changing map if change w or wo sample size -----
    observeEvent(input$choose_sample_or_not,{
        mapproxy <- leafletProxy("map", data = data)
        if(input$choose_sample_or_not == 1){ # without sample size
            mapproxy <- mapproxy %>% clearMarkers() %>%
                addCircleMarkers (
                    lat = ~Latitude, lng = ~Longitude, 
                    weight = 1, color = "#000000",
                    radius = 10,
                    label = ~htmlEscape(paste0('Failure: ', as.character(Predicted_Failure), '%')),
                    labelOptions = labelOptions(textsize = '13px'),
                    fillColor = ~pal(Predicted_Failure), 
                    fillOpacity = 0.75,
                    popup = content_popup
                )
        }else{
            mapproxy <- mapproxy %>% clearMarkers() %>%
                addCircleMarkers (
                    lat = data.big$Latitude, lng = data.big$Longitude,
                    radius =  par1() * sqrt(data.big$Total_Sample / min(data$Total_Sample)) + par2,
                    weight = 1, color = "#000000",
                    fillColor = pal(data.big$Predicted_Failure), 
                    fillOpacity = 0.75, 
                    label = lapply(paste0("<b>",as.character(data.big$Predicted_Failure), '%</b>'), HTML),
                    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE, textsize = '12px')
                ) %>%
                addCircleMarkers(
                    lat = data.small$Latitude, lng = data.small$Longitude,
                    radius = par1() * sqrt(data.small$Total_Sample / min(data$Total_Sample)) + par2,
                    weight = 1, color = "#000000",
                    fillColor = pal(data.small$Predicted_Failure), 
                    fillOpacity = 0.75, 
                    label = lapply(paste0("<b>",as.character(data.small$Predicted_Failure), '%</b>'), HTML),
                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                offset=c(0,4),
                                                style = list(
                                                    "font-size" = "12px"
                                                ))
                )
        }
        mapproxy
    })
    
    # --- Button function for exporting PDF -----
    onclick("btn_export",{
        js$preparetoexport('.maparea')
        js$export()
        Sys.sleep(5)
        js$afterexport('.maparea')
    })
    
}

## ----- Run Shiny -----
shinyApp(ui, server)
