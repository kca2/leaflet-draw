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
                       ifelse(spp$COUNT_ <= 4, "grey", "blue")) # colour polygons based on # of participants
spp$parts <- ifelse(spp$COUNT_ == 500, 
                    "ICZM Atlas of Sig. Coastal & Marine Areas", 
                    spp$COUNT_)

spp_proj <- spTransform(spp, "+proj=longlat +datum=WGS84")

spp_list <- unique(spp_proj$Species)

# commercial fisheries
comFishSpp <- c("Capelin", "Cod", "Crab", 
                 "Halibut", "Herring", "Lobster", "Mackerel", "Redfish", "Shrimp", "Smelt", 
                 "Snow Crab", "Squid", "Tuna", "Turbot")
comFish <- spp_proj[spp_proj$Species %in% comFishSpp, ]

# spawning
spawnSpp <- c("Capelin Spawning", "Herring Spawning", "Mackerel Spawning")
spawn <- spp_proj[spp_proj$Species %in% spawnSpp, ]

# regular fish
fishSpp <- c("Porbeagle Shark", "Sunfish", "Swordfish")
fish <- spp_proj[spp_proj$Species %in% fishSpp, ]

#birds
birdSpp <- c("Waterfowl Seabird", "Waterfowl Seabirds")
bird <- spp_proj[spp_proj$Species %in% birdSpp, ]
bird$Species <- ifelse(grepl("Waterfowl", bird$Species), "Waterfowl/Seabirds", bird$Species)
birdList <- unique(bird$Species)

#nesting
bnestSpp <- c("Eider Duck Nesting", "Migratory Bird Nesting", "Nesting Birds", "Piping Plover Nesting",
             "Waterfowl Spp   Seabirds Nesting")
bnest <- spp_proj[spp_proj$Species %in% bnestSpp, ]
bnest$Species <- ifelse(grepl("Waterfowl", bnest$Species), "Waterfowl Species & Seabirds Nesting",
                        bnest$Species)
bnestList <- unique(bnest$Species)


# shellfish
sfishSpp <- c("Clams", "Mussels", "Scallop", "Soft Shell Clam")
sfish <- spp_proj[spp_proj$Species %in% sfishSpp, ]

# ais
aisSpp <- c("Coffin Box", "Golden Star Tunicate", "Green Crab", "Rainbow Trout")
ais <- spp_proj[spp_proj$Species %in% aisSpp, ]

#sar
sarSpp <- c("American Eel", "American Marten", "Atlantic Wolffish", "Northern Wolffish", "Banded Killfish", 
            "Barrows Goldeneye", "Crowded Wormseed Mustard", "Goldeneye", "Griscombs Armica", "Harlequin Duck",
            "Ivory Gull", "Leatherback Turtle", "Mountain Fern", "Peregrine Falcon", "Piping Plover", "Red Knot",
            "Rusty Blackbird", "Short Eared Owl", "Woodland Caribou", "Wooly Arnica")
sar <- spp_proj[spp_proj$Species %in% sarSpp, ]

# marine mammals
mmSpp <- c("Bay Seals", "Seals", "Whale Dolphin")
mm <- spp_proj[spp_proj$Species %in% mmSpp, ]
mm$Species <- ifelse(mm$Species == "Whale Dolphin", "Whale/Dolphin", mm$Species)
mmList <- unique(mm$Species)

# significant marine habitats 
habsSpp <- c("Eelgrass", "Saltmarsh Goose Staging Area", "Saltmarsh")
habs <- spp_proj[spp_proj$Species %in% habsSpp, ] # not those habs

# spawning collapse & fisheries closures
scfcSpp <- c("Lobster Closure", "No Herring Spawning", "Snow Crab Closure")
scfc <- spp_proj[spp_proj$Species %in% scfcSpp, ]

# geological importance
geoSpp <- c("Artifact Fossil")
geo <- spp_proj[spp_proj$Species %in% geoSpp, ]
geo$Species <- "Artifact/Fossil"
geoList <- unique(geo$Species)

#---MSA 1A
# impt areas for commerical & rec fisheries
imptList <- c("High", "Medium", "Low")
zoneFxn <- function(new_col, old_col){
  new_col <- ifelse(grepl("High", old_col), "High",
                    ifelse(grepl("Med", old_col), "Medium", "Low"))
}
colCountFxn <- function(new_col, old_col){
  new_col <- ifelse(old_col <= 4, "grey", "blue")
}

msa_1a <- readOGR("./data", layer = "all_MSA_1A", GDAL1_integer64_policy = TRUE)
msa_1a$countCol <- colCountFxn(msa_1a$countCol, msa_1a$COUNT_)
msa_1a$Zone <- zoneFxn(msa_1a$Zone, msa_1a$Colour)

msa_1a_proj <- spTransform(msa_1a, "+proj=longlat +datum=WGS84")

#---MSA 3A
# impt areas for research
msa_3a <- readOGR("./data", layer = "all_MSA_3A", GDAL1_integer64_policy = TRUE)
msa_3a$countCol <- colCountFxn(msa_3a$countCol, msa_3a$COUNT_)
msa_3a$Zone <- zoneFxn(msa_3a$Zone, msa_3a$Colour)

msa_3a_proj <- spTransform(msa_3a, "+proj=longlat +datum=WGS84")

#---MSA 4A 
# THIS IS THE SAME AS TCC 10A???

#---TCC 1A
# areas of personal importance 
tcc_1a <- readOGR("./data", layer = "all_TCC_1A", GDAL1_integer64_policy = TRUE)
tcc_1a$countCol <- colCountFxn(tcc_1a$countCol, tcc_1a$COUNT_)
tcc_1a$Zone <- zoneFxn(tcc_1a$Zone, tcc_1a$Colour)

tcc_1a_proj <- spTransform(tcc_1a, "+proj=longlat +datum=WGS84")

#---TCC 2A
# viewsheds of personal importance
tcc_2a <- readOGR("./data", layer = "all_TCC_2A", GDAL1_integer64_policy = TRUE)
tcc_2a$countCol <- colCountFxn(tcc_2a$countCol, tcc_2a$COUNT_)
tcc_2a$Zone <- zoneFxn(tcc_2a$Zone, tcc_2a$Colour)

tcc_2a_proj <- spTransform(tcc_2a, "+proj=longlat +datum=WGS84")


#---RRD
# rrd <- readOGR("./data", layer = "RRD", GDAL1_integer64_policy = TRUE)
# 
# rrd_proj <- spTransform(rrd, "+proj=longlat +datum=WGS84")
# 
# rrd_list <- unique(rrd_proj$Energy)

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
                        
                        helpText("Circle & Identify"),
                        # actionButton("msaButton", label = ""),
                        # shinyjs::hidden(
                        #   div(id = "msaDiv",
                        #       checkboxGroupInput("sppCheck", NULL, choices = spp_list))
                        # ),
                        actionButton("comFishButton", label = "Commercial Fisheries"),
                        shinyjs::hidden(
                          div(id = "comFishDiv",
                              checkboxGroupInput("comFishCheck", NULL, choices = comFishSpp))
                        ),
                        
                        actionButton("spawnButton", label = "Spawning"),
                        shinyjs::hidden(
                          div(id = "spawnDiv",
                              checkboxGroupInput("spawnCheck", NULL, choices = spawnSpp))
                        ),
                        
                        actionButton("fishButton", label = "Non-commercial Fisheries"),
                        shinyjs::hidden(
                          div(id = "fishDiv",
                              checkboxGroupInput("fishCheck", NULL, choices = fishSpp))
                        ), br(),
                        
                        actionButton("birdButton", label = "Birds"),
                        shinyjs::hidden(
                          div(id = "birdDiv",
                              checkboxGroupInput("birdCheck", NULL, choices = birdList))
                        ), 
                        
                        actionButton("bnestButton", label = "Nesting Areas"),
                        shinyjs::hidden(
                          div(id = "bnestDiv",
                              checkboxGroupInput("bnestCheck", NULL, choices = bnestList))
                        ), br(), 
                        
                        actionButton("sfishButton", label = "Shellfish"),
                        shinyjs::hidden(
                          div(id = "sfishDiv",
                              checkboxGroupInput("sfishCheck", NULL, choices = sfishSpp))
                        ), br(),
                        
                        actionButton("aisButton", label = "AIS"),
                        shinyjs::hidden(
                          div(id = "aisDiv",
                              checkboxGroupInput("aisCheck", NULL, choices = aisSpp))
                        ), 
                        
                        actionButton("sarButton", label = "SAR"),
                        shinyjs::hidden(
                          div(id = "sarDiv",
                              checkboxGroupInput("sarCheck", NULL, choices = sarSpp))
                        ), br(), 
                        
                        actionButton("mmButton", label = "Marine Mammals"),
                        shinyjs::hidden(
                          div(id = "mmDiv",
                              checkboxGroupInput("mmCheck", NULL, choices = mmList))
                        ), br(),
                        
                        actionButton("habsButton", label = "Sig. Marine Habitats"),
                        shinyjs::hidden(
                          div(id = "habsDiv",
                              checkboxGroupInput("habsCheck", NULL, choices = habsSpp))
                        ), br(), 
                        
                        actionButton("scfcButton", label = "Collapse/Closures"),
                        shinyjs::hidden(
                          div(id = "scfcDiv",
                              checkboxGroupInput("scfcCheck", NULL, choices = scfcSpp))
                        ), br(), 
                        
                        actionButton("geoButton", label = "Geologically Important"),
                        shinyjs::hidden(
                          div(id = "geoDiv",
                              checkboxGroupInput("geoCheck", NULL, choices = geoList))
                        ),
                      
                        
                        # div(id = "msaDiv",
                        #     helpText("MSA as identified by 5/+ participants"),
                        #     checkboxGroupInput("sppCheck", NULL, choices = spp_list)),
                        
                        # helpText("MSA as identified by 5/+ participants"),
                        # checkboxGroupInput("sppCheck", NULL, #"Please select all that apply: ", 
                        #                    choices = spp_list),
                        
                        
                        helpText("MSA 1A - Important areas for commercial & recreational fishery"),
                        actionButton("msa1aButton", label = "Importance"),
                        shinyjs::hidden(
                          div(id = "msa1aDiv",
                              checkboxGroupInput("msa1aCheck", NULL, choices = imptList))
                        ),
                        
                        helpText("MSA 3A - Important areas for research"),
                        actionButton("msa3aButton", label = "Importance"),
                        shinyjs::hidden(
                          div(id = "msa3aDiv",
                              checkboxGroupInput("msa3aCheck", NULL, choices = imptList))
                        ),
                        
                        helpText("TCC 1A - Socially important areas"),
                        actionButton("tcc1aButton", label = " Importance"),
                        shinyjs::hidden(
                          div(id = "tcc1aDiv",
                              checkboxGroupInput("tcc1aCheck", NULL, choices = imptList))
                        ),
                        
                        helpText("TCC 2A - Socially important viewsheds"),
                        actionButton("tcc2aButton", label = "Importance"),
                        shinyjs::hidden(
                          div(id = "tcc2aDiv",
                              checkboxGroupInput("tcc2aCheck", NULL, choices = imptList))
                        ),
                        br(), br(), 
                        
                        helpText("To download user drawn polygon: "),
                        numericInput("usr", "Participant No.: ", "1", min = 1, max = 30, step = 1),
                        selectInput("use", "Question: ", choices = c("MSA_1A", "MSA_2A", "MSA_3A", "MSA_4A",
                                                                     "TCC_1A", "TCC_2A", "NRRD")),
                        selectInput("zones", "Importance: ", choices = c("High", "Med", "Low")),
                        downloadButton("dlshp", "Download polygon")
                        
                        

                        # helpText("RRD polygons"),
                        # checkboxGroupInput("rrdCheck", NULL, #"Please select all that apply: ", 
                        #                    choices = rrd_list)
                        
                        )),
             
             column(width = 8,
                    div(id = "mapCol", 
                        leafletOutput("map", height = 900, width = 700)))),
    
    br()
    
    # fluidRow(column(width = 12, offset = 0, #style = "background-color: #A7AFB2",
    #                 div(style = 'padding-left:10px',
    #                     helpText("To download user drawn polygon: "),
    #                     "Please select from the following: "))), 
    # br(), 
    # 
    # fluidRow(column(width = 2, offset = 0,
    #                 div(style = 'padding-left:10px',
    #                     id = "usrRow",
    #                     numericInput("usr", "Participant Number: ", "1", min = 1, max = 30, step = 1))),
    #          # column(width = 3, 
    #          #        id = "sppRow",
    #          #        selectInput("spp", "Species: ", choices = spp_list)),
    #          column(width = 1,
    #                 id = "usage",
    #                 selectInput("use", "Question: ", choices = c("MSA 1A", "MSA 2A", "MSA 3A", "MSA 4A",
    #                                                           "TCC 1A", "TCC 2A", "NNRD"))),
    #          column(width = 1, 
    #                 id = "zoneID",
    #                 selectInput("zones", "Importance: ", choices = c("High", "Med", "Low"))),
    #          column(width = 1, 
    #                 div(style = 'padding-top:25px', 
    #                 id = "downloaddiv",
    #                 downloadButton("dlshp", "Download polygon"))))
    
    # fluidRow(column(width = 5, offset = 0,
    #                 div(style = 'padding-left:10px', 
    #                     id = "downloaddiv",
    #                     downloadButton("dlshp", "Download polygon")))),
    # br()
)

###############
# Define server logic
###############
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(provider = providers$CartoDB.Positron) %>% 
            setView(lng = -57.80, lat = 49.60, zoom = 9.5) %>%
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
    
    observeEvent(input$bnestButton, {
      shinyjs::toggle(id = "bnestDiv")
    })
    
    observeEvent(input$sfishButton, {
      shinyjs::toggle(id = "sfishDiv")
    })
    
    observeEvent(input$aisButton, {
      shinyjs::toggle(id = "aisDiv")
    })
    
    observeEvent(input$sarButton, {
      shinyjs::toggle(id = "sarDiv")
    })
    
    observeEvent(input$mmButton, {
      shinyjs::toggle(id = "mmDiv")
    })
    
    observeEvent(input$habsButton, {
      shinyjs::toggle(id = "habsDiv")
    })
    
    observeEvent(input$scfcButton, {
      shinyjs::toggle(id = "scfcDiv")
    })
    
    observeEvent(input$geoButton, {
      shinyjs::toggle(id = "geoDiv")
    })
    
    observeEvent(input$msa1aButton, {
      shinyjs::toggle(id = "msa1aDiv")
    })
    
    observeEvent(input$msa3aButton, {
      shinyjs::toggle(id = "msa3aDiv")
    })
    
    observeEvent(input$tcc1aButton, {
      shinyjs::toggle(id = "tcc1aDiv")
    })
    
    observeEvent(input$tcc2aButton, {
      shinyjs::toggle(id = "tcc2aDiv")
    })
    
    #---allow users to turn layers on/off 
    observe({
      
      # spp_csub <- spp_proj[spp_proj$Species %in% input$sppCheck, ]
      comFish_csub <- comFish[comFish$Species %in% input$comFishCheck, ]
      spawn_csub <- spawn[spawn$Species %in% input$spawnCheck, ]
      fish_csub <- fish[fish$Species %in% input$fishCheck, ]
      bird_csub <- bird[bird$Species %in% input$birdCheck, ]
      bnest_csub <- bnest[bnest$Species %in% input$bnestCheck, ]
      sfish_csub <- sfish[sfish$Species %in% input$sfishCheck, ]
      ais_csub <- ais[ais$Species %in% input$aisCheck, ]
      sar_csub <- sar[sar$Species %in% input$sarCheck, ]
      mm_csub <- mm[mm$Species %in% input$mmCheck, ]
      habs_csub <- habs[habs$Species %in% input$habsCheck, ]
      scfc_csub <- scfc[scfc$Species %in% input$scfcCheck, ]
      geo_csub <- geo[geo$Species %in% input$geoCheck, ]
      
      msa1a_csub <- msa_1a_proj[msa_1a_proj$Zone %in% input$msa1aCheck, ]
      msa3a_csub <- msa_3a_proj[msa_3a_proj$Zone %in% input$msa3aCheck, ]
      tcc1a_csub <- tcc_1a_proj[tcc_1a_proj$Zone %in% input$tcc1aCheck, ]
      tcc2a_csub <- tcc_2a_proj[tcc_2a_proj$Zone %in% input$tcc2aCheck, ]
      
      
      #rrd_csub <- rrd_proj[rrd_proj$Energy %in% input$rrdCheck, ]
      
      leafletProxy("map") %>%
        
        clearShapes() %>%
        
        # addPolygons(data = spp_proj[spp_proj$Species %in% input$sppCheck, ], 
        #             weight = 1, color = "grey", smoothFactor = 0.5, 
        #             fillColor = spp_csub$countCol,
        #             popup = ~paste("Species: ", spp_csub$Species, "<br/>",
        #                            "No. of Participants: ", spp_csub$COUNT_)) %>% 
        
        addPolygons(data = comFish_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = comFish_csub$countCol,
                    popup = ~paste("Species: ", comFish_csub$Species, "<br/>",
                                   "No. of Participants: ", comFish_csub$parts)) %>% 
        
        addPolygons(data = spawn_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = spawn_csub$countCol,
                    popup = ~paste("Species: ", spawn_csub$Species, "<br/>",
                                   "No. of Participants: ", spawn_csub$parts)) %>%
        
        addPolygons(data = fish_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = fish_csub$countCol,
                    popup = ~paste("Species: ", fish_csub$Species, "<br/>",
                                   "No. of Participants: ", fish_csub$parts)) %>% 
        
        addPolygons(data = bird_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = bird_csub$countCol,
                    popup = ~paste("Species: ", bird_csub$Species, "<br/>",
                                   "No. of Participants: ", bird_csub$parts)) %>% 
        
        addPolygons(data = bnest_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = bnest_csub$countCol,
                    popup = ~paste("Speices: ", bnest_csub$Species, "<br/>",
                                   "No. of Participants: ", bnest_csub$parts)) %>% 
        
        addPolygons(data = sfish_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = sfish_csub$countCol,
                    popup = ~paste("Species: ", sfish_csub$Species, "<br/>",
                                   "No. of Participants: ", sfish_csub$parts)) %>% 
        
        addPolygons(data = ais_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = ais_csub$countCol,
                    popup = ~paste("Species: ", ais_csub$Species, "<br/>",
                                   "No. of Participants: ", ais_csub$parts)) %>% 
        
        addPolygons(data = sar_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = sar_csub$countCol, 
                    popup = ~paste("Species: ", sar_csub$Species, "<br/>",
                                   "No. of Participants: ", sar_csub$parts)) %>% 
        
        addPolygons(data = mm_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = mm_csub$countCol,
                    popup = ~paste("Species: ", mm_csub$Species, "<br/>",
                                   "No. of Participants: ", mm_csub$parts)) %>% 
        
        addPolygons(data = habs_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = habs_csub$countCol,
                    popup = ~paste("Habitat: ", habs_csub$Species, "<br/>",
                                   "No. of Participants: ", habs_csub$parts)) %>% 
        
        addPolygons(data = scfc_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = scfc_csub$countCol,
                    popup = ~paste("Collapse/Closure: ", scfc_csub$Species, "<br/>",
                                   "No. of Participants: ", scfc_csub$parts)) %>% 
        
        addPolygons(data = geo_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = geo_csub$countCol,
                    popup = ~paste("Feature: ", geo_csub$Species, "<br/>",
                                   "No. of Participants: ", geo_csub$parts)) %>% 
        
        
        addPolygons(data = msa1a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = msa1a_csub$countCol,
                    popup = ~paste("MSA 1A <br/> Importance: ", msa1a_csub$Zone, "<br/>",
                                   "No. of Participants: ", msa1a_csub$COUNT_)) %>% 
        
        addPolygons(data = msa3a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = msa3a_csub$countCol,
                    popup = ~paste("MSA 3A <br/> Importance: ", msa3a_csub$Zone, "<br/>",
                                   "No. of Participants: ", msa3a_csub$COUNT_)) %>% 
        
        addPolygons(data = tcc1a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = tcc1a_csub$countCol,
                    popup = ~paste("TCC 1A <br/> Importance: ", tcc1a_csub$Zone, "<br/>",
                                   "No. of Participants: ", tcc1a_csub$COUNT_)) %>% 
        
        addPolygons(data = tcc2a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = tcc2a_csub$countCol,
                    popup = ~paste("TCC 2A <br/> Importance: ", tcc2a_csub$Zone, "<br/>",
                                   "No. of Participants: ", tcc2a_csub$COUNT_)) 
      
        
        
        
        # addPolygons(data = rrd_proj[rrd_proj$Energy %in% input$rrdCheck, ], 
        #             weight = 1, color = "green",
        #             popup = ~paste("Energy: ", rrd_csub$Energy))
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
          paste("P", usr, "_", lyrName, "_", zoneID, ".zip", sep = "")
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
