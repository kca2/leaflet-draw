###############
# load libraries
###############
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf) # reproject from UTM to WGS84
library(shinyjs)
library(rgdal) #to read in polygons

###############
# reproject data as needed & get names for each unique layer 
###############
#---MSA
spp <- readOGR("./data", layer = "all_spp_kc", GDAL1_integer64_policy = TRUE)
spp$Species <- gsub("[[:punct:]]", " ", spp$Species) # remove special characters
spp$countCol <- ifelse(spp$COUNT_ == 500, "green",
                       ifelse(spp$COUNT_ <= 4, "grey", "red")) # colour polygons based on # of participants

spp_proj <- spTransform(spp, "+proj=longlat +datum=WGS84")

spp_list <- unique(spp_proj$Species)

# commercial fisheries
comFishSpp <- c("Arctic Char", "Atlantic Mackerel", "Capelin", "Cod", "Crab", "Groundfish", 
                 "Halibut", "Herring", "Lobster", "Mackerel", "Redfish", "Shrimp", "Smelt", 
                 "Snow Crab", "Squid", "Tuna", "Turbot")
comFish <- spp_proj[spp_proj$Species %in% comFishSpp, ]

# spawning
spawnSpp <- c("Capelin Spawning", "Halibut Spawning", "Herring Spawning", "Mackerel Spawning")
spawn <- spp_proj[spp_proj$Species %in% spawnSpp, ]

# regular fish
fishSpp <- c("Porbeagle Shark", "Sunfish", "Swordfish")
fish <- spp_proj[spp_proj$Species %in% fishSpp, ]

#birds
birdSpp <- c("Sea Duck", "Waterfowl Seabird")
bird <- spp_proj[spp_proj$Species %in% birdSpp, ]

#---AIS

#---RRD
rrd <- readOGR("./data", layer = "RRD", GDAL1_integer64_policy = TRUE)

rrd_proj <- spTransform(rrd, "+proj=longlat +datum=WGS84")

rrd_list <- unique(rrd_proj$Energy)

###############
# Define UI 
# Empty SpatialPolygonsDataFrame warning when no options are selected 
###############
ui <- bootstrapPage(
    useShinyjs(),
    
    fluidRow(column(width = 12, offset = 0,
                    div(style = 'padding-left:10px',
                        h3("EAC Dashboard - Gros Morne Region")))),
    
    fluidRow(column(width = 4,  
                    div(id = "sideCol", style = 'padding-left:10px',
                        
                        helpText("MSA"),
                        # actionButton("msaButton", label = "show/hide"),
                        # shinyjs::hidden(
                        #   div(id = "msaDiv",
                        #       checkboxGroupInput("sppCheck", NULL, choices = spp_list))
                        # ),
                        actionButton("comFishButton", label = "Show/Hide Commercial Fisheries"),
                        shinyjs::hidden(
                          div(id = "comFishDiv",
                              checkboxGroupInput("comFishCheck", NULL, choices = comFishSpp))
                        ),
                        
                        br(),
                        br(),
                        
                        actionButton("spawnButton", label = "Show/Hide Spawning"),
                        shinyjs::hidden(
                          div(id = "spawnDiv",
                              checkboxGroupInput("spawnCheck", NULL, choices = spawnSpp))
                        ), 
                        
                        br(),
                        br(), 
                        
                        actionButton("fishButton", label = "Show/Hide Non-commercial Fisheries"),
                        shinyjs::hidden(
                          div(id = "fishDiv",
                              checkboxGroupInput("fishCheck", NULL, choices = fishSpp))
                        ),
                        
                        br(),
                        br(),
                        
                        actionButton("birdButton", label = "Show/Hide Birds"),
                        shinyjs::hidden(
                          div(id = "birdDiv",
                              checkboxGroupInput("birdCheck", NULL, choices = birdSpp))
                        ),
                        
                        
                        # div(id = "msaDiv",
                        #     helpText("MSA as identified by 5/+ participants"),
                        #     checkboxGroupInput("sppCheck", NULL, choices = spp_list)),
                        
                        # helpText("MSA as identified by 5/+ participants"),
                        # checkboxGroupInput("sppCheck", NULL, #"Please select all that apply: ", 
                        #                    choices = spp_list),
                        
                        helpText("AIS polygons"),
                        checkboxGroupInput("aisCheck", NULL), 
                        
                        helpText("RRD polygons"),
                        checkboxGroupInput("rrdCheck", NULL, #"Please select all that apply: ", 
                                           choices = rrd_list)
                        
                        )),
             
             column(width = 8,
                    div(id = "mapCol", 
                        leafletOutput("map", height = 500, width = 600)))),
    
    br(),
    
    fluidRow(column(width = 12, offset = 0, #style = "background-color: #A7AFB2",
                    div(style = 'padding-left:10px',
                        helpText("To download user drawn polygon: "),
                        "Please select from the following: "))), 
    br(), 
    
    fluidRow(column(width = 3, offset = 0,
                    div(style = 'padding-left:10px',
                        id = "usrRow",
                        numericInput("usr", "Participant Number: ", "1", min = 1, max = 30, step = 1))),
             # column(width = 3, 
             #        id = "sppRow",
             #        selectInput("spp", "Species: ", choices = spp_list)),
             column(width = 3,
                    id = "usage",
                    selectInput("use", "Usage: ", choices = c("AIS", "MSA", "NRRD", "SS", "TCC"))),
             column(width = 2, 
                    id = "zoneID",
                    selectInput("zones", "Zone: ", choices = c(1,2,3))),
             column(width = 3, 
                    div(style = 'padding-top:25px', 
                    id = "downloaddiv",
                    downloadButton("dlshp", "Download polygon")))),
    
    # fluidRow(column(width = 5, offset = 0,
    #                 div(style = 'padding-left:10px', 
    #                     id = "downloaddiv",
    #                     downloadButton("dlshp", "Download polygon")))),
    br()
)

###############
# Define server logic
###############
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(provider = providers$CartoDB.Positron) %>% 
            setView(lng = -57.80, lat = 49.60, zoom = 8.5) %>%
            addDrawToolbar(rectangleOptions = FALSE,
                           editOptions = editToolbarOptions(edit = FALSE,
                                                            remove = TRUE),
                           polylineOptions = FALSE,
                           circleOptions = FALSE,
                           circleMarkerOptions = FALSE,
                           markerOptions = FALSE)
    })
    
    # #---Spp lyr
    # spp_cfilter <- reactive({
    #   spp_proj[spp_proj$Species %in% input$sppCheck, ]
    # })
    # 
    # observe({
    #   spp_csub <- spp_proj[spp_proj$Species %in% input$sppCheck, ]
    #   leafletProxy("map", data = spp_cfilter()) %>%
    #     #clearShapes() %>%
    #     addPolygons(weight = 1, color = "blue", popup = ~paste("No. of Surveyees: ", spp_csub$COUNT_, "<br/>",
    #                                                            "Species: ", spp_csub$Species))
    # })
    
    # observeEvent(input$msaButton, {
    #   shinyjs::toggle(id = "msaDiv")
    # })
    
    observeEvent(input$comFishButton, {
      shinyjs::toggle(id = "comFishDiv")
    })
    
    observeEvent(input$spawnButton, {
      shinyjs::toggle(id = "spawnDiv")
    })
    
    observeEvent(input$fishButton, {
      shinyjs::toggle(id = "fishDiv")
    })
    
    observeEvent(input$birdButton, {
      shinyjs::toggle(id = "birdDiv")
    })
    
    #---allow users to turn layers on/off 
    observe({
      
      # spp_csub <- spp_proj[spp_proj$Species %in% input$sppCheck, ]
      comFish_csub <- comFish[comFish$Species %in% input$comFishCheck, ]
      spawn_csub <- spawn[spawn$Species %in% input$spawnCheck, ]
      fish_csub <- fish[fish$Species %in% input$fishCheck, ]
      bird_csub <- bird[bird$Species %in% input$birdCheck, ]
      
      
      
      rrd_csub <- rrd_proj[rrd_proj$Energy %in% input$rrdCheck, ]
      
      leafletProxy("map") %>%
        
        clearShapes() %>%
        
        # addPolygons(data = spp_proj[spp_proj$Species %in% input$sppCheck, ], 
        #             weight = 1, color = "grey", smoothFactor = 0.5, 
        #             fillColor = spp_csub$countCol,
        #             popup = ~paste("Species: ", spp_csub$Species, "<br/>",
        #                            "No. of Participants: ", spp_csub$COUNT_)) %>% 
        
        addPolygons(data = comFish[comFish$Species %in% input$comFishCheck, ],
                    weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = comFish_csub$countCol,
                    popup = ~paste("Species: ", comFish_csub$Species, "<br/>",
                                   "No. of Participants: ", comFish_csub$COUNT_)) %>% 
        
        addPolygons(data = spawn[spawn$Species %in% input$spawnCheck, ],
                    weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = spawn_csub$countCol,
                    popup = ~paste("Species: ", spawn_csub$Species, "<br/>",
                                   "No. of Participants: ", spawn_csub$COUNT_)) %>%
        
        addPolygons(data = fish[fish$Species %in% input$fishCheck, ],
                    weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = fish_csub$countCol,
                    popup = ~paste("Species: ", fish_csub$Species, "<br/>",
                                   "No. of Participants: ", fish_csub$COUNT_)) %>% 
        
        addPolygons(data = bird[bird$Species %in% input$birdCheck, ],
                    weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = bird_csub$countCol,
                    popup = ~paste("Species: ", bird_csub$Species, "<br/>",
                                   "No. of Participants: ", bird_csub$COUNT_)) %>% 
        
        
        
        addPolygons(data = rrd_proj[rrd_proj$Energy %in% input$rrdCheck, ], 
                    weight = 1, color = "green",
                    popup = ~paste("Energy: ", rrd_csub$Energy))
    })

    
    output$dlshp <- downloadHandler(
        # filename = function() {
        #   spp_sub <- spp_proj[spp_proj$Species == input$spp, ]
        #   lyrName <- unique(spp_sub$Species)
        #   usr <- paste0(input$usr)
        #   zoneID <- paste0(input$zones)
        #   paste("P", usr, "_", lyrName, "_", zoneID, ".zip", sep = "")
        # },
        filename = function() {
          lyrName <- paste0(input$use)
          usr <- paste0(input$usr)
          zoneID <- paste0(input$zones)
          paste("P", usr, "_", lyrName, "_Z", zoneID, ".zip", sep = "")
        },
        content = function(file) {
            temp_shp <- tempdir()
            geo = input$map_draw_new_feature$geometry$coordinates[[1]]
            lng = map_dbl(geo, `[[`, 1)
            lat = map_dbl(geo, `[[`, 2)
            shp = st_as_sf(tibble(lon = lng, lat = lat), 
                           coords = c("lon", "lat"),
                           crs = 4326) %>%
                summarise(geometry = st_combine(geometry)) %>%
                st_cast("POLYGON")

            shp_files <- list.files(temp_shp, "shapefile*", 
                                    full.names = TRUE)
            if(length(shp_files) != 0) {
                file.remove(shp_files)
            }
            st_write(shp, paste(temp_shp, "shapefile.shp", sep = "\\"))
            # copy the zip file to the file argument
            shp_files <- list.files(temp_shp, "shapefile*", 
                                    full.names = TRUE)
            zip(zipfile = file, files = shp_files, flags = "-j")
            file.remove(shp_files)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
