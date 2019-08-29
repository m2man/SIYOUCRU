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
library(cowplot)
library(grid)
library(dplyr)

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

# Color for piechart (Failure - Success)
elements_piechart <- c('Failure', 'Success')
colors_piechart <- c("#d96a6a", "#4fc13c")
Piechart_Code_Color <- data.frame(Element = elements_piechart, Color = colors_piechart)


# Create Location Name and Code dataframe
# code and name need to match the order to each other
code <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
name <- c('Dak Lak', 'Dak Nong', 'Khanh Hoa', 'Ninh Thuan', 'Gia Lai', 'Quang Tri', 'Binh Phuoc', 'Quang Binh', 'Quang Nam', 'Phu Yen', 'Kon Tum')
# color <- brewer.pal(n = length(code), name = "Paired")
color <- c('#0f9d58', '#683ab7', '#0288d1', '#ffd600', '#ff5252', '#0097a7', '#c2185b', '#f57c00', '#795548', '#757575', '#e65100')
Location_Code_Color <- data.frame(Code = code, Name = name, Color = color)

# ========== SHOULD NOT TOUCH THESE CODE BELOW ==========
colnames(data) <- c('Location', 'Latitude', 'Longitude', 'Total_Sample', 'Predicted_Failure') # Rename column to read easily
data$Location_Code <- substr(data$Location, 1, 5) # add new Code columnto show on Map

# Take the numeric code of location code: VN010 --> "01", VN033 --> "03"
# Position is the 3rd and 4th character of Location_Code (VN011, VN033, ...)
data$Numeric_Code <- as.numeric(substr(data$Location_Code, 3, 4)) 

# Convert to numeric -- Original is character
data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)
data$Total_Sample <- as.numeric(data$Total_Sample)
data$Predicted_Failure <- as.numeric(data$Predicted_Failure) * 100

# Match the name of location to the code
data <- inner_join(data, Location_Code_Color, by = c("Numeric_Code" = "Code")) 

# Create Success column to plot piechart Failed - Success (percentage)
data$Predicted_Success <- 100 - data$Predicted_Failure

# define which marker is big (more than "thres" sample)
data.big <- data[which(data$Total_Sample > thresh), ]
data.small <- data[which(data$Total_Sample <= thresh), ]

# Color Palette for plotting treatment failure (can google for more color theme, "RdYlGn" is one of various pallete themes)
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

# ----- Legends Code and JS Code for exporting to PDF (DO NOT TOUCH) -----
# Check which locations appearing in the data, then plot the legend separately
idx_code_data <- which(code %in% data$Numeric_Code)
code_data <- code[idx_code_data]
name_data <- name[idx_code_data]
color_data <- color[idx_code_data]

Location_Code_Color_data <- data.frame(Code = code_data, Name = name_data, Color = color_data)
Location_Code_Color_data$Name <- factor(Location_Code_Color_data$Name, levels = name_data)
Location_Code_Color_data$Color <- factor(Location_Code_Color_data$Color, levels = color_data)
geographyplot <- ggplot(Location_Code_Color_data, aes(Name, fill = Name)) + geom_bar() + 
    scale_fill_manual("Locations", values = levels(Location_Code_Color_data$Color)) + 
    theme(
        legend.background = element_rect(colour = "transparent"),
        legend.key.size = unit(0.75, "cm")
    )
location_legend <- get_legend(geographyplot)

# Create legend for piechart
Piechart_Code_Color$Element <- factor(Piechart_Code_Color$Element, levels = elements_piechart)
Piechart_Code_Color$Color <- factor(Piechart_Code_Color$Color, levels = colors_piechart)
pieplot <- ggplot(Piechart_Code_Color, aes(Element, fill = Element)) + geom_bar() + 
    scale_fill_manual(values = levels(Piechart_Code_Color$Color)) + 
    theme(
        legend.background = element_rect(colour = "transparent"),
        legend.key.size = unit(0.75, "cm")
    )
piechart_legend <- get_legend(pieplot)

# Javascript
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
                    radioButtons(inputId = 'choose_pie_or_not', h4('Select Type:'), 
                                 choices = c('Pie Chart' = 1, 'Geography Color' = 2), selected = 1),
                    # ----- Displaying Slider for adjusting marker size if chosen with sample size -----
                    sliderInput(
                        inputId = 'slider_adjust_size',
                        h4('Adjust Size of Markers'),
                        min = 1, max = 5, value = 2, step = 0.25, sep = ''
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
                
                # ===== Displaying Scalebar =====
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
                ),
                
                # ----- Conditional Panel for displaying Geography if chosen with Geography Color -----
                conditionalPanel(
                    condition = "input.choose_pie_or_not == 2",
                    # # ----- Panel for displaying own legend -----
                    absolutePanel(id = 'mylegend', class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                  right = '2%', width = 117,
                                  top = '8%', height = 175,
                                  plotOutput(outputId = 'legend', height = '100%'))
                ),
                
                # ----- Conditional Panel for displaying Pie chart legend if chosen with Pie Chart -----
                conditionalPanel(
                    condition = "input.choose_pie_or_not == 1",
                    # # ----- Panel for displaying own legend -----
                    absolutePanel(id = 'mylegend_pie', class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                  right = '2%', width = 125,
                                  top = '8%', height = 75,
                                  plotOutput(outputId = 'legend_pie', height = '100%'))
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
    
    # --- Plot UI for displaying legend Geography(have to create own legend) -----
    output$legend <- renderPlot({
        par(mar=c(1,1,1,1))
        grid.draw(location_legend)
    })
    
    # --- Plot UI for displaying legend Piechart (have to create own legend) -----
    output$legend_pie <- renderPlot({
        par(mar=c(1,1,1,1))
        grid.draw(piechart_legend)
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
    
    # --- Changing map if change w or wo sample size -----
    observeEvent({input$choose_pie_or_not
                  input$slider_adjust_size},{
        par1(as.numeric(input$slider_adjust_size))
        mapproxy <- leafletProxy("map", data = data)
        if(input$choose_pie_or_not == 1){ # pie chart
            mapproxy <- mapproxy %>% clearMarkers() %>% clearControls() %>% clearMinicharts() %>%
                addMinicharts(
                    data$Longitude, data$Latitude,
                    type = "pie",
                    chartdata = data[, c("Predicted_Failure", "Predicted_Success")], 
                    colorPalette = colors_piechart, 
                    width = par1() * sqrt(data$Total_Sample / min(data$Total_Sample)) + par2, 
                    transitionTime = 0.2,
                    legend = FALSE # DONT DISPLAY DEFAULT LEGEND SINCE IT WILL LOST THE COLOR WHEN EXPORTING PDF
                )
        }else{ # Geography Color
            mapproxy <- mapproxy %>% clearMarkers() %>% clearControls() %>% clearMinicharts() %>%
                addCircleMarkers (
                    lat = data.big$Latitude, lng = data.big$Longitude,
                    radius =  par1() * sqrt(data.big$Total_Sample / min(data$Total_Sample)) + par2,
                    weight = 1, color = "#000000",
                    fillColor = data.big$Color, 
                    fillOpacity = 0.75, 
                    label = lapply(paste0("<b>",as.character(data.big$Predicted_Failure), '%</b>'), HTML),
                    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE, textsize = '12px')
                ) %>%
                addCircleMarkers(
                    lat = data.small$Latitude, lng = data.small$Longitude,
                    radius = par1() * sqrt(data.small$Total_Sample / min(data$Total_Sample)) + par2,
                    weight = 1, color = "#000000",
                    fillColor = data.small$Color, 
                    fillOpacity = 0.75, 
                    label = lapply(paste0("<b>",as.character(data.small$Predicted_Failure), '%</b>'), HTML),
                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                offset=c(0,4),
                                                style = list(
                                                    "font-size" = "12px"
                                                ))
                ) #%>%
            #addLegend("bottomleft", colors = unique(data$Color), labels = unique(data$Name), opacity = 1) # but this legend do not work when export to pdf
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
