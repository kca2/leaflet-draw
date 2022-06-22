###############
# load libraries
###############
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf) # reproject from UTM to WGS84
library(shinyjs)
library(rgdal) # to read in polygons
library(marmap) # get bathymetry data
library(raster) # display bathymetry as raster 

options(warn = 0) # suppress empty polygon warnings 

###############
# reproject data as needed & get names for each unique layer 
###############
#--- circle & ID
spp <- readOGR("./data", layer = "all_spp_kc", GDAL1_integer64_policy = TRUE)
spp$Species <- gsub("[[:punct:]]", " ", spp$Species) # remove special characters
# spp$countCol <- ifelse(spp$COUNT_ == 500, "red",
#                        ifelse(spp$COUNT_ <= 4, "grey", "blue")) # colour polygons based on # of participants
# spp$countCol <- ifelse(spp$COUNT_ >=10 & spp$COUNT_ <500, "cyan", spp$countCol)

spp$countCol <- ifelse(spp$COUNT_ <= 4, "grey", 
                       ifelse(spp$COUNT_ >= 10, "cyan", "blue"))
spp$countCol <- ifelse(spp$COUNT_ == 500, "green", spp$countCol)


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

# salmon
salmon <- readOGR("./data", layer = "Salmonrivers_KC", GDAL1_integer64_policy = TRUE)
salmon$countCol <- ifelse(is.na(salmon$MapFile), "green", "Grey")
salmon$SPECIES <- as.character(salmon$SPECIES)
salmon$SPECIES <- ifelse(salmon$SPECIES == "Salmon & Sea Trout", "Salmon", salmon$SPECIES)
salmon$REF <- ifelse(is.na(salmon$MapFile), "ICZM Atlas of Sig. Coastal & Marine Areas",
                     "Previous workshops")
sal_proj <- spTransform(salmon, "+proj=longlat +datum=WGS84")

# birds & nesting
birdSpp <- c("Waterfowl Seabird", "Waterfowl Seabirds",
             "Eider Duck Nesting", "Migratory Bird Nesting", "Nesting Birds", "Piping Plover Nesting",
             "Saltmarsh Goose Staging Area", "Waterfowl Spp   Seabirds Nesting")
bird <- spp_proj[spp_proj$Species %in% birdSpp, ]
bird$Species <- ifelse(bird$Species == "Waterfowl Seabird" | bird$Species == "Waterfowl Seabirds",
                       "Waterfowl/Seabirds", bird$Species)
bird$Species <- ifelse(bird$Species == "Waterfowl Spp   Seabirds Nesting",
                       "Waterfowl Spp & Seabirds Nesting", bird$Species)
bird$Species <- ifelse(bird$Species == "Saltmarsh Goose Staging Area",
                       "Goose Staging Area", bird$Species)
birdList <- unique(bird$Species)

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
habsSpp <- c("Eelgrass", "Saltmarsh")
habs <- spp_proj[spp_proj$Species %in% habsSpp, ] # not those habs

# spawning collapse & fisheries closures
scfcSpp <- c("Lobster Closure", "No Herring Spawning", "Snow Crab Closure")
scfc <- spp_proj[spp_proj$Species %in% scfcSpp, ]

# geological importance
geoSpp <- c("Artifact Fossil")
geo <- spp_proj[spp_proj$Species %in% geoSpp, ]
geo$Species <- "Artifact/Fossil"
geoList <- unique(geo$Species)

# sewage outflow
ss <- readOGR("./data", layer = "SS_KC", GDAL1_integer64_policy = TRUE)
ss$countCol <- ifelse(ss$MapFile == "Atlas", "green", "Grey")
ss$REF <- ifelse(ss$MapFile == "Atlas", "ICZM Atlas of Sig. Coastal & Marine Areas",
                 "Previous workshops")
ss_proj <- spTransform(ss, "+proj=longlat +datum=WGS84")

#---importance & impact polygons
imptList <- c("High", "Medium", "Low")
zoneFxn <- function(new_col, old_col){
  new_col <- ifelse(grepl("High", old_col), "High",
                    ifelse(grepl("Med", old_col), "Medium", "Low"))
}
colCountFxn <- function(new_col, old_col){
  new_col <- ifelse(old_col <= 4, "grey", 
                    ifelse(old_col >= 10, "cyan", "blue"))
}

polys <- readOGR("./data", layer = "all_overlap_polys", GDAL1_integer64_policy = TRUE)
polys$Name <- as.character(polys$Name)
polys$Name <- ifelse(polys$Name == "Med", "Medium", polys$Name)
polys$countCol <- colCountFxn(polys$countCol, polys$COUNT_)
polys$Zone <- polys$Name

polys_proj <- spTransform(polys, "+proj=longlat +datum=WGS84")

# msa 1a
msa_1a <- polys_proj[polys_proj$Map_No == "MSA_1A", ]

# msa 3a
msa_3a <- polys_proj[polys_proj$Map_No == "MSA_3A", ]

# msa 4a
# THIS IS == TCC_10A! 
nmca <- polys_proj[polys_proj$Map_No == "TCC_10A", ]
nmca$nmca_zone <- ifelse(nmca$Zone == "High", "Zone 1",
                         ifelse(nmca$Zone == "Medium", "Zone 2", "Zone 3"))
nmcaList <- c("Zone 1", "Zone 2", "Zone 3")

# tcc 1a
tcc_1a <- polys_proj[polys_proj$Map_No == "TCC_1A", ]

# tcc 2a
tcc_2a <- polys_proj[polys_proj$Map_No == "TCC_2A", ]

# rec areas
recList <- c("TCC_4A", "TCC_5A", "TCC_6A", "TCC_7A", "TCC_8A")
rec <- polys_proj[polys_proj$Map_No %in% recList, ]
rec <- rec[rec$Name == "High", ]
rec$acts <- ifelse(rec$Map_No == "TCC_4A", "Kayaking/Canoeing/SUP",
                   ifelse(rec$Map_No == "TCC_5A", "Hiking/Camping/Tidepool Walks",
                          ifelse(rec$Map_No == "TCC_6A", "Photography",
                                 ifelse(rec$Map_No == "TCC_7A", "Bird Watching", "Hunting"))))
actList <- unique(rec$acts)

#--- buffer
buff <- readOGR("./data", layer = "all_buffers", GDAL1_integer64_policy = TRUE)
buff_proj <- spTransform(buff, "+proj=longlat +datum=WGS84")
buffList <- unique(buff_proj$Buffer_Dis)

#--- bath
# bath <- readOGR("./data", layer = "bathCont", GDAL1_integer64_policy = TRUE)
# bath_proj <- spTransform(bath, "+proj=longlat +datum=WGS84")
# bathList <- unique(bath_proj$ID)
# 
# gmBath <- getNOAA.bathy(lon1 = -60.5, lon2 = -51, lat1 = 46, lat2 = 52.13, resolution = 1,
#                         keep = T, path = "./data")

gmBath <- read.bathy("./data/marmap_coord_-60.5;46;-51;52.13_res_1.csv", header = T)

gmRast <- marmap::as.raster(gmBath)

nfld <- readOGR("./data", layer = "select_divs", GDAL1_integer64_policy = TRUE)
nfld_proj <-  spTransform(nfld, "+proj=longlat +datum=WGS84")

# remove land 
gmMask <- mask(gmRast, nfld_proj)
gmMask[gmMask@data@values > 0] <- NA

# remove all elevations above sea level
# gmMask <- marmap::as.raster(gmBath)
# gmMask[gmMask@data@values > 0] <- NA

bPal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(gmMask), na.color = "transparent")

#bPal <- colorNumeric(c("#06114f", "#710096", "#ffffcc"), values(gmMask), na.color = "transparent")

#---RRD
# rrd <- readOGR("./data", layer = "RRD", GDAL1_integer64_policy = TRUE)
# 
# rrd_proj <- spTransform(rrd, "+proj=longlat +datum=WGS84")
# 
# rrd_list <- unique(rrd_proj$Energy)

###############
# Define UI 
# Empty SpatialPolygonsDataFrame warning when no options are selected 
# 8C979A
###############
ui <- bootstrapPage(
    tags$head(
      tags$style(HTML(
        "body {background-color: #366677; color: white;}

        h2{color: white;}

        hr{color: white;}

        .tabbable > .nav > li > a {
            background-color: #366677; color: white; border: 0px solid white;}
        .tabbable > .nav > li[class = active] > a {
            background-color: #366677; color: white; border: 2px solid white;}
        .tabbable > .nav {border: 0px solid white;}

        .help-block{color: white;}"
      ))
    ),
    
    useShinyjs(),
    
    div(id = "main", style = 'padding-left:10px; padding-right:5px',
        
        h2("EAC Dashboard - Gros Morne Region"),
        
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #366677;",
            width = 6, 
            tabsetPanel(type = "tabs",
                        tabPanel("Circle & Identify",
                                 tags$hr(),
                                 actionButton("comFishButton", label = "Commercial Fisheries"),
                                 shinyjs::hidden(
                                   div(id = "comFishDiv",
                                       checkboxGroupInput("comFishCheck", NULL, choices = comFishSpp))
                                 ), tags$p(),  
                                 
                                 actionButton("spawnButton", label = "Spawning"),
                                 shinyjs::hidden(
                                   div(id = "spawnDiv",
                                       checkboxGroupInput("spawnCheck", NULL, choices = spawnSpp))
                                 ), tags$p(), 
                                 
                                 actionButton("fishButton", label = "Non-commercial Fisheries"),
                                 shinyjs::hidden(
                                   div(id = "fishDiv",
                                       checkboxGroupInput("fishCheck", NULL, choices = fishSpp))
                                 ), tags$p(), 
                                 
                                 actionButton("salButton", label = "Salmon Rivers"),
                                 shinyjs::hidden(
                                   div(id = "salDiv",
                                       checkboxInput("salCheck", "Salmon", FALSE))
                                 ), tags$p(), 
                                 
                                 actionButton("birdButton", label = "Bird & Nesting Areas"),
                                 shinyjs::hidden(
                                   div(id = "birdDiv",
                                       checkboxGroupInput("birdCheck", NULL, choices = birdList))
                                 ), tags$p(), 
                                 
                                 actionButton("sfishButton", label = "Shellfish"),
                                 shinyjs::hidden(
                                   div(id = "sfishDiv",
                                       checkboxGroupInput("sfishCheck", NULL, choices = sfishSpp))
                                 ), tags$p(), 
                                 
                                 actionButton("aisButton", label = "AIS"),
                                 shinyjs::hidden(
                                   div(id = "aisDiv",
                                       checkboxGroupInput("aisCheck", NULL, choices = aisSpp))
                                 ),
                                 
                                 actionButton("sarButton", label = "SAR"),
                                 shinyjs::hidden(
                                   div(id = "sarDiv",
                                       checkboxGroupInput("sarCheck", NULL, choices = sarSpp))
                                 ), tags$p(), 
                                 
                                 actionButton("mmButton", label = "Marine Mammals"),
                                 shinyjs::hidden(
                                   div(id = "mmDiv",
                                       checkboxGroupInput("mmCheck", NULL, choices = mmList))
                                 ), tags$p(), 
                                 
                                 actionButton("habsButton", label = "Sig. Marine Habitats"),
                                 shinyjs::hidden(
                                   div(id = "habsDiv",
                                       checkboxGroupInput("habsCheck", NULL, choices = habsSpp))
                                 ), tags$p(), 
                                 
                                 actionButton("scfcButton", label = "Collapse/Closures"),
                                 shinyjs::hidden(
                                   div(id = "scfcDiv",
                                       checkboxGroupInput("scfcCheck", NULL, choices = scfcSpp))
                                 ), tags$p(), 
                                 
                                 actionButton("geoButton", label = "Geologically Important"),
                                 shinyjs::hidden(
                                   div(id = "geoDiv",
                                       checkboxGroupInput("geoCheck", NULL, choices = geoList))
                                 ), tags$p(),  
                                  
                                 actionButton("ssButton", label = "Sewage Outflows"),
                                 shinyjs::hidden(
                                   div(id = "ssDiv",
                                       checkboxInput("ssCheck", "Sewage", FALSE))
                                 ), br()
                                 ), # end of circle & ID tab
                        
                        tabPanel("MSA",
                                 tags$hr(), 
                                 helpText("MSA 1A - Important areas for commercial & recreational fisheries"),
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
                                 
                                 helpText("MSA 4A - NMCA zones"),
                                 actionButton("nmcaButton", label = "Zones"),
                                 shinyjs::hidden(
                                   div(id = "nmcaDiv",
                                       checkboxGroupInput("nmcaCheck", NULL, choices = nmcaList))
                                 )
                                 ), # end of MSA tab 
                        
                        tabPanel("TCC",
                                 tags$hr(), 
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
                                 
                                 helpText("TCC 4A:8A - Important recreation areas"),
                                 actionButton("recButton", label = "Activities"),
                                 shinyjs::hidden(
                                   div(id = "recDiv",
                                       checkboxGroupInput("recCheck", NULL, choices = actList))
                                 )
                                 ), # end of TCC tab
                        tabPanel("Misc.",
                                 tags$hr(), 
                                 helpText("Bathymetry"),
                                 actionButton("bathButton", label = "Bathymetry (m)"),
                                 shinyjs::hidden(
                                   div(id = "bathDiv",
                                       checkboxInput("bathCheck", "Bathymetry", FALSE))
                                 ),
                                 
                                 helpText("Buffer"),
                                 actionButton("buffButton", label = "Distance from National Park Boundary (km)"),
                                 shinyjs::hidden(
                                   div(id = "buffDiv",
                                       checkboxGroupInput("buffCheck", NULL, choices = buffList))
                                 )
                                 ) 
                        
                        ), # end of tabset panel 
            fluidRow(tags$hr(),
                     style = "padding-left:10px; padding-right: 10px;",
                     helpText("To download user drawn polygon: "),
                     helpText("Participant No.:"),
                     numericInput("usr", NULL, "1", min = 1, max = 30, step = 1),
                     helpText("Question:"),
                     selectInput("use", NULL, choices = c("MSA_1A", "MSA_2A", "MSA_3A", "MSA_4A",
                                                                  "TCC_1A", "TCC_2A", "TCC_4A", "TCC_5A",
                                                                  "TCC_6A","TCC_7A", "TCC_8A", "NRRD",
                                                                  "1_A", "1_B", "1_C", "1_D", "1_E",
                                                                  "2", "3", "4", "5", "6", "7", "8",
                                                                  "9", "10", "11", "12", "13", "14", "15",
                                                                  "16", "17", "18", "19", "20")),
                     helpText("Importance:"),
                     selectInput("zones", NULL, choices = c("High", "Med", "Low")),
                     downloadButton("dlshp", "Download polygon")
                     
                     ) # end fluid row 
          ), # end sidebar panel 
          mainPanel(
            width = 6, 
            leafletOutput("map", height = 1000, width = 800))
        ) # end side bar layout 
        
        ) # end main div
    

    
) # end ui




#                         
#                         
# 
#                         # helpText("RRD polygons"),
#                         # checkboxGroupInput("rrdCheck", NULL, #"Please select all that apply: ", 
#                         #                    choices = rrd_list)
#                         
#                         )),
#              
#              column(width = 8,
#                     div(id = "mapCol", 
#                         leafletOutput("map", height = 1000, width = 800)))),
#     
#     br()
#     
#     # fluidRow(column(width = 12, offset = 0, #style = "background-color: #A7AFB2",
#     #                 div(style = 'padding-left:10px',
#     #                     helpText("To download user drawn polygon: "),
#     #                     "Please select from the following: "))), 
#     # br(), 
#     # 
#     # fluidRow(column(width = 2, offset = 0,
#     #                 div(style = 'padding-left:10px',
#     #                     id = "usrRow",
#     #                     numericInput("usr", "Participant Number: ", "1", min = 1, max = 30, step = 1))),
#     #          # column(width = 3, 
#     #          #        id = "sppRow",
#     #          #        selectInput("spp", "Species: ", choices = spp_list)),
#     #          column(width = 1,
#     #                 id = "usage",
#     #                 selectInput("use", "Question: ", choices = c("MSA 1A", "MSA 2A", "MSA 3A", "MSA 4A",
#     #                                                           "TCC 1A", "TCC 2A", "NNRD"))),
#     #          column(width = 1, 
#     #                 id = "zoneID",
#     #                 selectInput("zones", "Importance: ", choices = c("High", "Med", "Low"))),
#     #          column(width = 1, 
#     #                 div(style = 'padding-top:25px', 
#     #                 id = "downloaddiv",
#     #                 downloadButton("dlshp", "Download polygon"))))
#     
#     # fluidRow(column(width = 5, offset = 0,
#     #                 div(style = 'padding-left:10px', 
#     #                     id = "downloaddiv",
#     #                     downloadButton("dlshp", "Download polygon")))),
#     # br()
# )

###############
# Define server logic
###############
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(provider = providers$Esri.WorldTopoMap) %>%
            #addProviderTiles(provider = providers$Esri.OceanBasemap) %>% 
            setView(lng = -57.80, lat = 49.60, zoom = 9.5) %>%
            addScaleBar() %>% 
            addMeasure(primaryLengthUnit = "kilometers", 
                       primaryAreaUnit = "sqmeters",
                       position = "topleft") %>% 
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

    observeEvent(input$salButton, {
      shinyjs::toggle(id = "salDiv")
    })

    observeEvent(input$birdButton, {
      shinyjs::toggle(id = "birdDiv")
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

    observeEvent(input$ssButton, {
      shinyjs::toggle(id = "ssDiv")
    })

    observeEvent(input$msa1aButton, {
      shinyjs::toggle(id = "msa1aDiv")
    })


    observeEvent(input$msa3aButton, {
      shinyjs::toggle(id = "msa3aDiv")
    })

    observeEvent(input$nmcaButton, {
      shinyjs::toggle(id = "nmcaDiv")
    })

    observeEvent(input$tcc1aButton, {
      shinyjs::toggle(id = "tcc1aDiv")
    })

    observeEvent(input$tcc2aButton, {
      shinyjs::toggle(id = "tcc2aDiv")
    })

    observeEvent(input$recButton, {
      shinyjs::toggle(id = "recDiv")
    })

    observeEvent(input$bathButton, {
      shinyjs::toggle(id = "bathDiv")
    })

    observeEvent(input$buffButton, {
      shinyjs::toggle(id = "buffDiv")
    })


    
    #---allow users to turn layers on/off 
    observe({
      
      # spp_csub <- spp_proj[spp_proj$Species %in% input$sppCheck, ]
      comFish_csub <- comFish[comFish$Species %in% input$comFishCheck, ]
      spawn_csub <- spawn[spawn$Species %in% input$spawnCheck, ]
      fish_csub <- fish[fish$Species %in% input$fishCheck, ]
      bird_csub <- bird[bird$Species %in% input$birdCheck, ]
      sfish_csub <- sfish[sfish$Species %in% input$sfishCheck, ]
      ais_csub <- ais[ais$Species %in% input$aisCheck, ]
      sar_csub <- sar[sar$Species %in% input$sarCheck, ]
      mm_csub <- mm[mm$Species %in% input$mmCheck, ]
      habs_csub <- habs[habs$Species %in% input$habsCheck, ]
      scfc_csub <- scfc[scfc$Species %in% input$scfcCheck, ]
      geo_csub <- geo[geo$Species %in% input$geoCheck, ]

      msa1a_csub <- msa_1a[msa_1a$Zone %in% input$msa1aCheck, ]
      msa3a_csub <- msa_3a[msa_3a$Zone %in% input$msa3aCheck, ]
      nmca_csub <- nmca[nmca$nmca_zone %in% input$nmcaCheck, ]

      tcc1a_csub <- tcc_1a[tcc_1a$Zone %in% input$tcc1aCheck, ]
      tcc2a_csub <- tcc_2a[tcc_2a$Zone %in% input$tcc2aCheck, ]

      rec_csub <- rec[rec$acts %in% input$recCheck, ]

      # #bath_csub <- bath_proj[bath_proj$ID %in% input$bathCheck, ]
      buff_csub <- buff_proj[buff_proj$Buffer_Dis %in% input$buffCheck, ]

      # #rrd_csub <- rrd_proj[rrd_proj$Energy %in% input$rrdCheck, ]
      # 
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

        addPolygons(data = nmca_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = nmca_csub$countCol,
                    popup = ~paste("MSA 4A <br/> NMCA Zone: ", nmca_csub$nmca_zone, "<br/>",
                                   "No. of Participants: ", nmca_csub$COUNT_)) %>% 

        addPolygons(data = tcc1a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = tcc1a_csub$countCol,
                    popup = ~paste("TCC 1A <br/> Importance: ", tcc1a_csub$Zone, "<br/>",
                                   "No. of Participants: ", tcc1a_csub$COUNT_)) %>%

        addPolygons(data = tcc2a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = tcc2a_csub$countCol,
                    popup = ~paste("TCC 2A <br/> Importance: ", tcc2a_csub$Zone, "<br/>",
                                   "No. of Participants: ", tcc2a_csub$COUNT_)) %>%

        addPolygons(data = rec_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = rec_csub$countCol,
                    popup = ~paste("Rec. Areas: ", rec_csub$acts, "<br/>",
                                   "No. of Participants: ", rec_csub$COUNT_)) %>% 
        # 
        # # addPolygons(data = bath_csub, weight = 1, color = "grey", smoothFactor = 0.5,
        # #             fillColor = bath_csub$countCol, fillOpacity = 1, 
        # #             popup = ~paste("Depth: ", bath_csub$ID, " m")) %>% 
        # 
        # 
        addPolygons(data = buff_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    popup = ~paste("Distance from National Park Boundary: ",
                                   buff_csub$Buffer_Dis, " km"))



        # # addPolygons(data = rrd_proj[rrd_proj$Energy %in% input$rrdCheck, ], 
        # #             weight = 1, color = "green",
        # #             popup = ~paste("Energy: ", rrd_csub$Energy))
      
    })
    
    # allow users to turn markers on/off
    observe({
      prox <- leafletProxy("map")
      prox %>% clearMarkers()
      if (input$salCheck){
        prox %>% addCircleMarkers(data = sal_proj[sal_proj$SPECIES == "Salmon", ],
                                  color = "grey", fillColor = sal_proj$countCol, radius = 5,
                                  popup = ~paste("Source: ", sal_proj$REF))
      }

      if(input$ssCheck){
        prox %>%
          addCircleMarkers(data = ss_proj, radius = 5,
                           color = "grey", fillColor = ss_proj$countCol,
                           popup = ~paste("Location: ", ss_proj$SiteName, "<br/>",
                                          "Source: ", ss_proj$REF))
      }



    })

    # allow users to turn bathymetry on/off
    observe({
      prox <- leafletProxy("map")
      prox %>% clearImages() %>% clearControls()
      if(input$bathCheck){
        prox %>%
          addRasterImage(gmMask, opacity = 0.4, colors = bPal) %>%
          addLegend(pal = bPal, values = values(gmMask), title = "Depth (m)")
        }
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
