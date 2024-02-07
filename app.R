###############
# download libraries
###############
dep01 <- list.files("./libs/dep01", recursive = F, full.names = T)
lapply(dep01, function(i){install.packages(i, repos = FULL)})

dep02 <- list.files("./libs/dep02", recursive = F, full.names = T)
lapply(dep_files, function(i){install.packages(i, repos = NULL)})

pkgs <- list.files("./libs/pkgs", recursive = F, full.names = T)
lapply(pkgs, function(i){install.packages(i, repos = NULL)})

###############
# load libraries
###############
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf) # read in SHP & reproject to WGS 84
library(shinyjs)
library(marmap) # get bathymetry data
library(raster) # display bathymetry as raster
library(leaflet.esri) # esri REST

options(warn = 0) # suppress empty polygon warnings 

###############
# reproject data as needed & get names for each unique layer 
###############
#--- circle & ID
#spp <- rgdal::readOGR("./data", layer = "all_spp_msa_aug", GDAL1_integer64_policy = TRUE)
spp <- read_sf(dsn = "./data", layer = "all_spp_msa_aug")

spp$Species <- gsub("[[:punct:]]", " ", spp$Species) # remove special characters


colCountFxn <- function(new_col, old_col){
  new_col <- ifelse(old_col <= 4, "black", 
                    ifelse(old_col >= 10, "cyan", "blue"))
}

spp$countCol <- colCountFxn(spp$countCol, spp$COUNT_)
spp$countCol <- ifelse(spp$COUNT_ == 500, "red", spp$countCol)

spp$parts <- ifelse(spp$COUNT_ == 500, 
                    "ICZM Atlas of Sig. Coastal & Marine Areas", 
                    spp$COUNT_)

#spp_proj <- spTransform(spp, "+proj=longlat +datum=WGS84")
spp_proj <- st_transform(spp, "+proj=longlat +datum=WGS84")


spp_list <- unique(spp_proj$Species)

# commercial fisheries
comFishList <- c("Capelin", "Cod", "Crab", 
                 "Halibut", "Herring", "Lobster", "Mackerel", "Redfish", "Shrimp", "Smelt", 
                 "Snow Crab", "Squid", "Tuna", "Turbot")
comFish <- spp_proj[spp_proj$Species %in% comFishList, ]

# spawning
spawnList<- c("Capelin Spawning", "Herring Spawning", "Mackerel Spawning")
spawn <- spp_proj[spp_proj$Species %in% spawnList, ]

# regular fish
fishList <- c("Sunfish", "Swordfish")
fish <- spp_proj[spp_proj$Species %in% fishList, ]

# salmon - old file
#salmon <- readOGR("./data", layer = "Salmonrivers_KC", GDAL1_integer64_policy = TRUE)
# salmon <- read_sf(dsn = "./data", layer = "Salmonrivers_KC")
# salmon$countCol <- ifelse(is.na(salmon$MapFile), "green", "black")
# salmon$SPECIES <- as.character(salmon$SPECIES)
# salmon$SPECIES <- ifelse(salmon$SPECIES == "Salmon & Sea Trout", "Salmon", salmon$SPECIES)
# salmon$REF <- ifelse(is.na(salmon$MapFile), "ICZM Atlas of Sig. Coastal & Marine Areas",
#                      "Previous workshops")
# #sal_proj <- spTransform(salmon, "+proj=longlat +datum=WGS84")
# sal_proj <- st_transform(salmon, "+proj=longlat +datum=WGS84")

# salmon & sea trout
sal_trout <- read_sf(dsn = "./data", layer = "all_salmon_seatrout_aug")
sal_trout$countCol <- ifelse(sal_trout$COUNT_ == 500, "red", "black")
sal_trout$parts <- ifelse(sal_trout$COUNT_ == 500, "ICZM Atlas of Sig. Coastal & Marine Areas", sal_trout$COUNT_)

sal_proj <- st_transform(sal_trout[sal_trout$SPECIES == "Salmon", ], "+proj=longlat +datum=WGS84")
trout_proj <- st_transform(sal_trout[sal_trout$SPECIES == "Sea Trout", ], "+proj=longlat +datum=WGS84")

# birds & nesting
birdSpp <- c("Waterfowl Seabird", "Waterfowl Seabirds",
             "Eider Duck Nesting", "Migratory Bird Nesting", "Nesting Birds", "Piping Plover Nesting",
             "Saltmarsh Goose Staging Area", "Waterfowl Spp   Seabirds Nesting")
bird <- spp_proj[spp_proj$Species %in% birdSpp, ]
bird$Species <- ifelse(bird$Species == "Waterfowl Seabird" | bird$Species == "Waterfowl Seabirds",
                       "Waterfowl/Seabirds", bird$Species)
bird$Species <- ifelse(bird$Species == "Waterfowl Spp   Seabirds Nesting",
                       "Waterfowl/Seabirds Nesting", bird$Species)
bird$Species <- ifelse(bird$Species == "Saltmarsh Goose Staging Area",
                       "Goose Staging Area", bird$Species)
birdList <- unique(bird$Species)

# shellfish
sfishList <- c("Clams", "Mussels", "Scallop", "Soft Shell Clam")
sfish <- spp_proj[spp_proj$Species %in% sfishList, ]

# ais
aisList <- c("Coffin Box", "Golden Star Tunicate", "Green Crab", "Rainbow Trout")
ais <- spp_proj[spp_proj$Species %in% aisList, ]

#sar - Griscombs Armica is a misspelling 
sarList <- c("American Eel", "American Marten", "Atlantic Wolffish", "Northern Wolffish", "Banded Killfish", 
            "Barrows Goldeneye", "Crowded Wormseed Mustard", "Goldeneye", "Griscombs Armica", "Harlequin Duck",
            "Ivory Gull", "Leatherback Turtle", "Mountain Fern", "Peregrine Falcon", "Piping Plover", "Red Knot",
            "Rusty Blackbird", "Short Eared Owl", "Woodland Caribou", "Wooly Arnica")
sar <- spp_proj[spp_proj$Species %in% sarList, ]

# marine mammals
mmSpp <- c("Bay Seals", "Seals", "Whale Dolphin")
mm <- spp_proj[spp_proj$Species %in% mmSpp, ]
mm$Species <- ifelse(mm$Species == "Whale Dolphin", "Whale/Dolphin", mm$Species)
mmList <- unique(mm$Species)

# significant marine habitats 
habsList <- c("Eelgrass", "Saltmarsh")
habs <- spp_proj[spp_proj$Species %in% habsList, ] # not those habs

# spawning collapse & fisheries closures
scfcList <- c("Lobster Closure", "No Herring Spawning", "Snow Crab Closure")
scfc <- spp_proj[spp_proj$Species %in% scfcList, ]

# geological importance
geoSpp <- c("Artifact Fossil")
geo <- spp_proj[spp_proj$Species %in% geoSpp, ]
geo$Species <- "Artifact/Fossil"
geoList <- unique(geo$Species)

# sewage outflow
#ss <- readOGR("./data", layer = "SS_KC", GDAL1_integer64_policy = TRUE)
ss <- read_sf(dsn = "./data", layer = "SS_KC")
ss$countCol <- ifelse(ss$MapFile == "Atlas", "red", "black")
ss$REF <- ifelse(ss$MapFile == "Atlas", "ICZM Atlas of Sig. Coastal & Marine Areas",
                 "Previous workshops")
#ss_proj <- spTransform(ss, "+proj=longlat +datum=WGS84")
ss_proj <- st_transform(ss, "+proj=longlat +datum=WGS84")

#---importance & impact polygons
imptList <- c("High", "Medium", "Low")
# zoneFxn <- function(new_col, old_col){
#   new_col <- ifelse(grepl("High", old_col), "High",
#                     ifelse(grepl("Med", old_col), "Medium", "Low"))
# }

#polys <- readOGR("./data", layer = "all_overlap_polys_aug", GDAL1_integer64_policy = TRUE)
polys <- read_sf(dsn = "./data", layer = "all_overlap_polys_aug")

# polys$Name <- as.character(polys$Name)
# polys$Name <- ifelse(polys$Name == "Med", "Medium", polys$Name)
polys <- polys %>% mutate(Zone = ifelse(Name == "Med", "Medium", Name))

polys$countCol <- colCountFxn(polys$countCol, polys$COUNT_)

#polys_proj <- spTransform(polys, "+proj=longlat +datum=WGS84")
polys_proj <- st_transform(polys, "+proj=longlat +datum=WGS84")

# msa 1a
msa_1a <- polys_proj[polys_proj$Map_No == "MSA_1A", ]

# msa 3a
msa_3a <- polys_proj[polys_proj$Map_No == "MSA_3A", ]

# msa 4a
# THIS IS == TCC_10A! 
# nmca <- polys_proj[polys_proj$Map_No == "TCC_10A", ]
# nmca$nmca_zone <- ifelse(nmca$Zone == "High", "Zone 1",
#                          ifelse(nmca$Zone == "Medium", "Zone 2", "Zone 3"))
# nmcaList <- c("Zone 1", "Zone 2", "Zone 3")

nmca <- polys_proj[polys_proj$Map_No == "NMCA", ]
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
# buff <- readOGR("./data", layer = "all_buffers", GDAL1_integer64_policy = TRUE)
# buff_proj <- spTransform(buff, "+proj=longlat +datum=WGS84")

buff <- read_sf(dsn = "./data", layer = "all_buffers")
buff_proj <- st_transform(buff, "+proj=longlat +datum=WGS84")
buffList <- unique(buff_proj$Buffer_Dis)

#--- bathymetry
# bath <- readOGR("./data", layer = "bathCont", GDAL1_integer64_policy = TRUE)
# bath_proj <- spTransform(bath, "+proj=longlat +datum=WGS84")
# bathList <- unique(bath_proj$ID)
# 
# gmBath <- getNOAA.bathy(lon1 = -60.5, lon2 = -51, lat1 = 46, lat2 = 52.13, resolution = 1,
#                         keep = T, path = "./data")

gmBath <- read.bathy("./data/marmap_coord_-60.5;46;-51;52.13_res_1.csv", header = T)

gmRast <- marmap::as.raster(gmBath)

# nfld <- readOGR("./data", layer = "select_divs", GDAL1_integer64_policy = TRUE)
# nfld_proj <-  spTransform(nfld, "+proj=longlat +datum=WGS84")

nfld <- read_sf(dsn = "./data", layer = "select_divs")
nfld_proj <- st_transform(nfld, "+proj=longlat +datum=WGS84")

# remove land 
gmMask <- mask(gmRast, nfld_proj)
gmMask[gmMask@data@values > 0] <- NA

# remove all elevations above sea level
# gmMask <- marmap::as.raster(gmBath)
# gmMask[gmMask@data@values > 0] <- NA

bPal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(gmMask), na.color = "transparent")

#bPal <- colorNumeric(c("#06114f", "#710096", "#ffffcc"), values(gmMask), na.color = "transparent")

#--- research locations
# study <- read_sf(dsn = "./data", layer = "all_research_locs")
# colnames(study)[c(2, 7:8, 14:15)] <- c("Date_Research_Conducted", "Author_Lead_Researcher", "Proper_Citation",
#                                        "Common_Name", "MSA_Category")
# study <- study %>% arrange(Category)
# study_proj <- st_transform(study, "+proj=longlat +datum=WGS84")
# studyList <- unique(study$Category)

study <- read_sf(dsn = "./data", layer = "all_research_locs_edit")
colnames(study)[c(8:9)] <- c("Common_Name", "MSA_Category")
study <- study %>% arrange(Category)
studyList <- unique(study$Category)
study_proj <- st_transform(study, "+proj=longlat +datum=WGS84")

overlap <- study %>% filter(!is.na(MSA_Category)) %>% arrange(MSA_Category)
overlapList <- unique(overlap$MSA_Category)
overlap_proj <- st_transform(overlap, "+proj=longlat +datum=WGS84")


# ahoi --------------------------------------------------------------------
ahoiSpp <- read_sf(dsn = "./data", layer = "ahoi_spp_pts")
ahoiSppList <- unique(ahoiSpp$Legend)
ahoiSpp_proj <- st_transform(ahoiSpp, "+proj=longlat +datum=WGS84")

ahoiPolys <- read_sf(dsn = "./data", layer = "ahoi_polys_diss")
ahoiPolysList <- unique(ahoiPolys$Colour)
ahoiPolys <- ahoiPolys %>% 
  mutate(parts = case_when(Bin == 1 ~ "1 - 4",
                           Bin == 3 ~ "10+",
                           TRUE ~ "5 - 9"))
ahoiPolys_proj <- st_transform(ahoiPolys, "+proj=longlat +datum=WGS84")

ahoiOthers <- read_sf(dsn = "./data", layer = "ahoi_other_polys")
ahoiOthersList <- unique(ahoiOthers$Legend)
ahoiOthers_proj <- st_transform(ahoiOthers, "+proj=longlat +datum=WGS84")

m1 <- read_sf(dsn = "./data", layer = "updated_2022_M1")
m1List <- unique(m1$MapFile)
m1_proj <- st_transform(m1, "+proj=longlat +datum=WGS84")

#---mgmt
gmz_file <- "./data/mgmt/tif/gmz.tif"
gmz <- raster(gmz_file)
gPal <- colorFactor(c("#000000", "#000000", "#808080", "#808080",
                      "#FFFFFF", "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF"),
                    values(gmz), levels = c(1,2,3,4,5,6,7,8,9,10,11), na.color = "transparent")

high_file <- "./data/mgmt/tif/pmz_high.tif"
pmz_high <- raster(high_file)
hPal <- colorFactor(c("#0C2C84","#0C2C84", "#41B6C4", "#41B6C4", "#797EF6","#797EF6","#797EF6","#797EF6","#797EF6"), 
                    values(pmz_high), levels = c(1,2,3,4,5,6,7,8,9), na.color = "transparent")

ml_file <- "./data/mgmt/tif/pmz_ml.tif"
pmz_ml <- raster(ml_file)
mPal <- colorFactor(c("#CCCC84", "#CCCC84", "#408E7F", "#408E7F", "#13404A", "#13404A", "#13404A"),
                    values(pmz_ml), levels = c(1,2,3,4,5,6,7), na.color = "transparent")



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

groupCheckFxn <- function(df, dfChoices){
  shinyjs::hidden(
    div(id = paste0(df, "Div"),
        checkboxGroupInput(inputId = paste0(df, "Check"), NULL, choices = dfChoices))
  )
}

checkFxn <- function(df, dfLabel){
  shinyjs::hidden(
    div(id = paste0(df, "Div"),
        checkboxInput(inputId = paste0(df, "Check"), label = dfLabel))
  )
}

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

        .help-block{color: white;}
  
        .selectize-input{width: 200px;}
        
        .form-control{width: 200px;}"
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
                                 groupCheckFxn("comFish", comFishList), tags$p(),  
                                 
                                 actionButton("spawnButton", label = "Spawning"),
                                 groupCheckFxn("spawn", spawnList), tags$p(), 
                                 
                                 actionButton("fishButton", label = "Non-commercial Fisheries"),
                                 groupCheckFxn("fish", fishList), tags$p(), 
                                 
                                 actionButton("salButton", label = "Salmon Rivers"),
                                 checkFxn("sal", "Salmon"), tags$p(),
                                 
                                 actionButton("troutButton", label = "Sea Trout Rivers"),
                                 checkFxn("trout", "Sea Trout"), tags$p(),
                                 
                                 actionButton("birdButton", label = "Bird & Nesting Areas"),
                                 groupCheckFxn("bird", birdList), tags$p(), 
                                 
                                 actionButton("sfishButton", label = "Shellfish"),
                                 groupCheckFxn("sfish", sfishList), tags$p(), 
                                 
                                 actionButton("aisButton", label = "AIS"),
                                 groupCheckFxn("ais", aisList),
                                 
                                 actionButton("sarButton", label = "SAR"),
                                 groupCheckFxn("sar", sarList), tags$p(), 
                                 
                                 actionButton("mmButton", label = "Marine Mammals"),
                                 groupCheckFxn("mm", mmList), tags$p(), 
                                 
                                 actionButton("habsButton", label = "Sig. Marine Habitats"),
                                 groupCheckFxn("habs", habsList), tags$p(), 
                                 
                                 actionButton("scfcButton", label = "Collapse/Closures"),
                                 groupCheckFxn("scfc", scfcList), tags$p(), 
                                 
                                 actionButton("geoButton", label = "Geologically Important"),
                                 groupCheckFxn("geo", geoList), tags$p(),  
                                 
                                 actionButton("ssButton", label = "Sewage Outflows"),
                                 checkFxn("ss", "Sewage"), br()
                                 ), # end of circle & ID tab
                        
                        tabPanel("MSA",
                                 tags$hr(), 
                                 helpText("MSA 1A - Important areas for commercial & recreational fisheries"),
                                 actionButton("msa1aButton", label = "Importance"),
                                 groupCheckFxn("msa1a", imptList), 
                                 
                                 helpText("MSA 3A - Important areas for research"),
                                 actionButton("msa3aButton", label = "Importance"),
                                 groupCheckFxn("msa3a", imptList),
                                 
                                 helpText("MSA 4A - NMCA zones"),
                                 actionButton("nmcaButton", label = "Zones"),
                                 groupCheckFxn("nmca", nmcaList)
                                 ), # end of MSA tab 
                        
                        tabPanel("TCC",
                                 tags$hr(), 
                                 helpText("TCC 1A - Socially important areas"),
                                 actionButton("tcc1aButton", label = " Importance"),
                                 groupCheckFxn("tcc1a", imptList),
                                 
                                 helpText("TCC 2A - Socially important viewsheds"),
                                 actionButton("tcc2aButton", label = "Importance"),
                                 groupCheckFxn("tcc2a", imptList),
                                 
                                 helpText("TCC 4A:8A - Important recreation areas"),
                                 actionButton("recButton", label = "Activities"),
                                 groupCheckFxn("rec", actList)
                                 ), # end of TCC tab
                        
                        tabPanel("Misc.",
                                 tags$hr(), 
                                 helpText("Bathymetry"),
                                 actionButton("bathButton", label = "Bathymetry (m)"),
                                 checkFxn("bath", "Bathymetry"),
                                 
                                 helpText("Buffer"),
                                 actionButton("buffButton", label = "Distance from National Park Boundary (km)"),
                                 groupCheckFxn("buff", buffList),
                                 
                                 helpText("All Research Locations"),
                                 actionButton("studyButton", label = "Research Locations"),
                                 groupCheckFxn("study", studyList),
                                 
                                 helpText("Research locations that have corresponding Circle & ID polygons"),
                                 actionButton("overlapButton", label = "Research Locations/Circle & ID"),
                                 groupCheckFxn("overlap", overlapList)
                                 ), # end of Misc tab
                        
                        tabPanel("Gov.",
                                 tags$hr(),
                                 tags$b("The data in this tab is from the Open Government Portal and accessed under the 
                                 following license: https://open.canada.ca/en/open-government-licence-canada"), 
                                 tags$p(),
                                 
                                 actionButton("ebsaButton", label = "Ecologically or Biologically Sig. Marine Areas"),
                                 checkFxn("ebsa", "EBSAs"), tags$p(),
                                 
                                 actionButton("sharButton", label = "Shellfish Harvest Area Classification"),
                                 #groupCheckFxn("shar", sharList)
                                 checkFxn("shar", "Shellfish Harvest Areas"), tags$p(),
                                 
                                 actionButton("spongeButton", label = "Sponge Fields in the Gulf"),
                                 checkFxn("sponge", "Sponge Fields"), tags$p(),
                                 
                                 actionButton("martenButton", label = "American Marten Critical Habitat"),
                                 checkFxn("marten", "American Marten Habitat"), tags$p(),
                                 
                                 actionButton("ploverButton", label = "Piping Plover Critical Habitat"),
                                 checkFxn("plover", "Piping Plover Habitat"), tags$p(),
                                 
                                 helpText("Harp Seal Distribution: do not use this lyr - unstable atm"),
                                 actionButton("harpButton", label = "Harp Seal Distribution"),
                                 checkFxn("harp", "Harp Seal Distribution"), tags$p(),
                                 
                                 helpText("Blue Whale Areas: do not use this lyr - unstable atm"),
                                 actionButton("whaleButton", label = "Blue Whale Migration and Feeding Areas"),
                                 checkFxn("whale", "Blue Whale Areas"), tags$p(),
                                 
                                 actionButton("shoreButton", label = "Atlantic Shoreline Classification"),
                                 checkFxn("shore", "Shoreline Classification"), tags$p(),
                      
                                 actionButton("wasteButton", label = "Wastewater Treatment"),
                                 checkFxn("waste", "Wastewater Treatment"), tags$p(),
                                 
                                 actionButton("calanusButton", label = "Calanus spp. size & lipid content metrics"),
                                 checkFxn("calanus", "Calanus spp."), tags$p()
                                 
                                 
                                 ), # end of Gov tab
                        tabPanel("AHOI",
                                 tags$hr(),
                                 actionButton("ahoiSppButton", label = "C&I Species Points"),
                                 groupCheckFxn("ahoiSpp", ahoiSppList), tags$p(),
                                 
                                 actionButton("ahoiPolysButton", label = "MSA & TCC Dissolved by Count Polygons"),
                                 groupCheckFxn("ahoiPolys", ahoiPolysList), tags$p(),
                                 
                                 actionButton("ahoiOthersButton", label = "Species Polygons"),
                                 groupCheckFxn("ahoiOthers", ahoiOthersList), tags$p()
                                 
                                 ), # end of AHOI tab
                        tabPanel("M1 2022",
                                 tags$hr(),
                                 actionButton("m1Button", label = "2022 Meeting 1 Polygons"),
                                 groupCheckFxn("m1", m1List), tags$p()
                          
                                ), # end of M1 2022
                        tabPanel("Mgmt.",
                                 tags$hr(),
                                 actionButton("gmzButton", label = "GMZ"),
                                 checkFxn("gmz", "GMZ"), tags$p(),
                                 
                                 actionButton("pmzHighButton", label = "PMZ High"),
                                 checkFxn("pmzHigh", "High"), tags$p(),
                                 
                                 actionButton("pmzMLButton", label = "PMZ Med-Low"),
                                 checkFxn("pmzML", "Med-Low")
                                 
                                 ) # end mgmt 
                        
                        ), # end of tabset panel 
            
            fluidRow(tags$hr(),
                     style = "padding-left:10px; padding-right:10px;",
                     actionButton("clearLyrs", "Clear All Displayed Layers")
                     ), # end clear all fluid row
            
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
                     ) # end polygon d/l fluid row 
            
          ), # end sidebar panel
          
          mainPanel(
            width = 6, 
            leafletOutput("map", height = 1000, width = 800))
        ) # end side bar layout 
        
        ) # end main div
    
) # end ui



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
                           editOptions = editToolbarOptions(edit = FALSE, remove = TRUE),
                           polylineOptions = FALSE,
                           circleOptions = FALSE,
                           circleMarkerOptions = FALSE,
                           markerOptions = FALSE)
 
    })
    
    #--- toggle buttons 
    catList <- c("comFish", "spawn", "fish", "sal", "trout", "bird", "sfish", "ais", "sar", 
                 "mm", "habs", "scfc", "geo", "ss", "msa1a", "msa3a", "nmca",
                 "tcc1a", "tcc2a", "rec", "bath", "buff", "study", "overlap", 
                 "ebsa", "shar", "sponge", "marten", "plover", "harp", "whale",
                 "shore", "waste", "calanus", "ahoiSpp", "ahoiPolys", "ahoiOthers",
                 "m1", "gmz", "pmzHigh", "pmzML")
    
    
    lapply(catList, FUN = function(i){
      observeEvent(input[[paste0(i, "Button")]], {
        shinyjs::toggle(id = paste0(i, "Div"))
      })
    })
    
    # observeEvent(input$comFishButton, {
    #   shinyjs::toggle(id = "comFishDiv")
    # })

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
      
      ahoiPolys_csub <- ahoiPolys_proj[ahoiPolys_proj$Colour %in% input$ahoiPolysCheck, ]
      ahoiOthers_csub <- ahoiOthers_proj[ahoiOthers_proj$Legend %in% input$ahoiOthersCheck, ]
      
      m1_csub <- m1_proj[m1_proj$MapFile %in% input$m1Check, ]

      # #rrd_csub <- rrd_proj[rrd_proj$Energy %in% input$rrdCheck, ]
      # 
      
      sppPopFxn <- function(sppCol, partsCol){
        spp_pop <- paste("Species: ", sppCol, "<br/>", "No. of Participants: ", partsCol)
      }
      
      imptPopFxn <- function(imptPoly, zoneCol, countCol){
        impt_pop <- paste(imptPoly, "<br /> Importance: ", zoneCol, "<br />", "No. of Participants: ", countCol)
      }
      
      leafletProxy("map") %>%
        
        clearShapes() %>%
        
        # addPolygons(data = comFish_csub, weight = 1, color = "grey", smoothFactor = 0.5,
        #             fillColor = comFish_csub$countCol,
        #             popup = ~paste("Species: ", comFish_csub$Species, "<br/>",
        #                            "No. of Participants: ", comFish_csub$parts)) %>% 
        
        addPolygons(data = comFish_csub, weight = 1, color = "grey", smoothFactor = 0.5, 
                    fillColor = comFish_csub$countCol,
                    popup = sppPopFxn(comFish_csub$Species, comFish_csub$parts)) %>%
        
        addPolygons(data = spawn_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = spawn_csub$countCol,
                    popup = sppPopFxn(spawn_csub$Species, spawn_csub$parts)) %>% 

        addPolygons(data = fish_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = fish_csub$countCol,
                    popup = sppPopFxn(fish_csub$Species, fish_csub$parts)) %>% 

        addPolygons(data = bird_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = bird_csub$countCol,
                    popup = sppPopFxn(bird_csub$Species, bird_csub$parts)) %>%

        addPolygons(data = sfish_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = sfish_csub$countCol,
                    popup = sppPopFxn(sfish_csub$Species, sfish_csub$parts)) %>%

        addPolygons(data = ais_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = ais_csub$countCol,
                    popup = sppPopFxn(ais_csub$Species, ais_csub$parts)) %>%

        addPolygons(data = sar_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = sar_csub$countCol,
                    popup = sppPopFxn(sar_csub$Species, sar_csub$parts)) %>%

        addPolygons(data = mm_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = mm_csub$countCol,
                    popup = sppPopFxn(mm_csub$Species, mm_csub$parts)) %>%

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
                    popup = imptPopFxn("MSA 1A", msa1a_csub$Zone, msa1a_csub$COUNT_)) %>% 

        addPolygons(data = msa3a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = msa3a_csub$countCol,
                    popup = imptPopFxn("MSA 3A", msa3a_csub$Zone, msa3a_csub$COUNT_)) %>% 
        
        addPolygons(data = nmca_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = nmca_csub$countCol,
                    popup = imptPopFxn("MSA 4A", nmca_csub$nmca_zone, nmca_csub$COUNT_)) %>% 

        addPolygons(data = tcc1a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = tcc1a_csub$countCol,
                    popup = imptPopFxn("TCC 1A", tcc1a_csub$Zone, tcc1a_csub$COUNT_)) %>% 

        addPolygons(data = tcc2a_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = tcc2a_csub$countCol,
                    popup = imptPopFxn("TCC 2A", tcc2a_csub$Zone, tcc2a_csub$COUNT_)) %>% 

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
                                   buff_csub$Buffer_Dis, " km")) %>% 
        
        addPolygons(data = ahoiPolys_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    fillColor = ahoiPolys_csub$Dashboard,
                    popup = ~paste("Category: ", ahoiPolys_csub$Colour, "<br/>",
                                   "No. of Participants: ", ahoiPolys_csub$parts)) %>% 
        
        addPolygons(data = ahoiOthers_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    popup = ~paste("Species: ", ahoiOthers_csub$Legend)) %>% 
        
        addPolygons(data = m1_csub, weight = 1, color = "grey", smoothFactor = 0.5,
                    popup = ~paste("File Name: ", m1_csub$MapFile))



        # # addPolygons(data = rrd_proj[rrd_proj$Energy %in% input$rrdCheck, ], 
        # #             weight = 1, color = "green",
        # #             popup = ~paste("Energy: ", rrd_csub$Energy))
      
    })
    
    # allow users to turn markers on/off
    observe({
      prox <- leafletProxy("map")
      prox %>% clearMarkers()
      if (input$salCheck){
        prox %>% addCircleMarkers(data = sal_proj,
                                  color = "grey", fillColor = sal_proj$countCol, radius = 5,
                                  popup = ~paste("No. of Participants: ", sal_proj$parts))
      }
      
      if(input$troutCheck){
        prox %>% addCircleMarkers(data = trout_proj,
                                  color = "grey", fillColor = trout_proj$countCol, radius = 5,
                                  popup = ~paste("No. of Participants: ", trout_proj$parts))
      }

      if(input$ssCheck){
        prox %>%
          addCircleMarkers(data = ss_proj, radius = 5,
                           color = "grey", fillColor = ss_proj$countCol,
                           popup = ~paste("Location: ", ss_proj$SiteName, "<br/>",
                                          "Source: ", ss_proj$REF))
      }
      
      study_csub <- study_proj[study_proj$Category %in% input$studyCheck, ]
      prox %>% 
        addCircleMarkers(data = study_csub, color = "grey", radius = 5,
                         popup = ~paste("Category: ", study_csub$Category, "<br />",
                                        "Taxonomic Class: ", study_csub$Class, "<br />",
                                        "Common Name: ", study_csub$Common_Name))
      
      overlap_csub <- overlap_proj[overlap_proj$MSA_Category %in% input$overlapCheck, ]
      prox %>% 
        addCircleMarkers(data = overlap_csub, color = "grey", radius = 5,
                         popup = ~paste("Category: ", overlap_csub$Category, "<br />",
                                        "Taxonomic Class: ", overlap_csub$Class, "<br />",
                                        "Common Name: ", overlap_csub$Common_Name))
      
      ahoiSpp_csub <- ahoiSpp_proj[ahoiSpp_proj$Legend %in% input$ahoiSppCheck, ]
      prox %>% 
        addCircleMarkers(data = ahoiSpp_csub, color = "grey", radius = 5,
                         popup = ~paste("Species: ", ahoiSpp_csub$Legend))

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
    
    # allow users to turn gov. layers on/off
    observe({
      chab = "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CriticalHabitatAlanticHabitatEnPerilAtlantique/MapServer/"
      oilhab = "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region_en/MapServer/"
      
      prox <- leafletProxy("map")
      prox %>% clearGeoJSON()
      
      if(input$ebsaCheck){
        prox %>% addEsriFeatureLayer(url = "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Ecologically_or_Biologically_Significant_Marine_Areas_Newfoundland_and_Labrador/MapServer/0",
                                    color = "red")
      }
      
      if(input$sharCheck){
        prox %>% addEsriFeatureLayer(url = "https://maps-cartes.ec.gc.ca/arcgis/rest/services/Shellfish_Classification_Mollusques/MapServer/5",
                                     useServiceSymbology = TRUE, labelProperty = "class_en", color = "red",
                                     featureLayerOptions(where = "province = 'NL'"))
      }
      
      if(input$spongeCheck){
        prox %>% addEsriFeatureLayer(url = "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/CSAS_Corals_Sponges_2010_EN/MapServer/10",
                                     color = "red")
      }
      
      if(input$martenCheck){
        prox %>% addEsriFeatureLayer(url = paste0(chab, "14"), color = "red")
      }
      
      
      if(input$ploverCheck){
        prox %>% addEsriFeatureLayer(url = paste0(chab, "3"), useServiceSymbology = TRUE, 
                                     featureLayerOptions(where = "Province = 'NL'"),
                                     markerType = "circleMarker", markerOptions = leaflet::markerOptions(radius = 5, color = "red"), 
                                     labelProperty = "Beach_Name")
      }
      
      # if(input$harpCheck){
      #   prox %>% addEsriFeatureLayer(url = paste0(oilhab, "14"), color = "red")
      # }
      # 
      # if(input$whaleCheck){
      #   prox %>% addEsriFeatureLayer(url = paste0(oilhab, "16"), color = "red")
      # }
      
      if(input$shoreCheck){
        prox %>% addEsriFeatureLayer(url = "https://ec.gc.ca/arcgis/rest/services/EPB_EPO/ShorelineSegmentationWithSCATClassification/MapServer/4",
                                     color = "red", labelProperty = "SCAT_Class_EN")
      }
      
      if(input$wasteCheck){
        prox %>% addEsriFeatureLayer(url = "https://maps-cartes.ec.gc.ca/arcgis/rest/services/DMS/Type_de_Traitement_Treatment_Type/MapServer/0",
                                     useServiceSymbology = TRUE, 
                                     featureLayerOptions(where = "Province_EN = 'Newfoundland and Labrador'"),
                                     markerType = "circleMarker", markerOptions = leaflet::markerOptions(radius = 5, color = "red"),
                                     labelProperty = "Treatment_Types")
      }
      
      if(input$calanusCheck){
        prox %>% addEsriFeatureLayer(url = "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Calanus_spp_Size_and_Lipid_Content_Metrics_in_North_Atlantic_1977_2019/MapServer/0",
                                     useServiceSymbology = TRUE,
                                     markerType = "circleMarker", markerOptions = leaflet::markerOptions(radius = 5, color = "red"),
                                     labelProperty = "Species")
      }
      
      
    })
    
    # allow users to turn gmz 
    observe({
      prox <- leafletProxy("map")
      prox %>% clearImages() %>% clearControls()
      if(input$gmzCheck){
        prox %>%
          addRasterImage(x = gmz, opacity = 0.5, colors = gPal)
      }
      
      # if(input$pmzHighCheck){
      #   prox %>% 
      #     addRasterImage(x = pmz_high, opacity = 0.5, colors = hPal) %>% 
      #     addLegend(pal = hPal, values = values(pmz_high), title = "Categories")
      # }
      
      if(input$pmzHighCheck){
        prox %>%
          addRasterImage(x = pmz_high, opacity = 0.5, colors = hPal)
      }
      
      if(input$pmzMLCheck){
        prox %>% 
          addRasterImage(x = pmz_ml, opacity = 0.5, colors = mPal)
      }
    })
    
    # allow users to clear all layers displayed
    checkList <- c("sal", "trout", "ss", "bath", 
                   "ebsa", "shar", "sponge", "marten", "plover", "harp", "whale",
                   "shore", "waste", "calanus")
    lapply(checkList, FUN = function(i){
      observeEvent(input$clearLyrs, {
        updateCheckboxInput(session = session, inputId = paste0(i, "Check"), value = FALSE)
      })
    })
    
    groupImptList <- c("msa1a", "msa3a", "tcc1a", "tcc2a")
    lapply(groupImptList, FUN = function(i){
      observeEvent(input$clearLyrs, {
        updateCheckboxGroupInput(session = session, inputId = paste0(i, "Check"), choices = imptList, selected = NULL)
      })
    })
    
    
    
    observeEvent(input$clearLyrs, {
      updateCheckboxGroupInput(session, "comFishCheck", choices = comFishList, selected = NULL)
      updateCheckboxGroupInput(session, "spawnCheck", choices = spawnList, selected = NULL)
      updateCheckboxGroupInput(session, "fishCheck", choices = fishList, selected = NULL)
      updateCheckboxGroupInput(session, "birdCheck", choices = birdList, selected = NULL)
      updateCheckboxGroupInput(session, "sfishCheck", choices = sfishList, selected = NULL)
      updateCheckboxGroupInput(session, "aisCheck", choices = aisList, selected = NULL)
      updateCheckboxGroupInput(session, "sarCheck", choices = sarList, selected = NULL)
      updateCheckboxGroupInput(session, "mmCheck", choices = mmList, selected = NULL)
      updateCheckboxGroupInput(session, "habsCheck", choices = habsList, selected = NULL)
      updateCheckboxGroupInput(session, "scfcCheck", choices = scfcList, selected = NULL)
      updateCheckboxGroupInput(session, "geoCheck", choices = geoList, selected = NULL)
      updateCheckboxGroupInput(session, "nmcaCheck", choices = nmcaList, selected = NULL)
      updateCheckboxGroupInput(session, "recCheck", choices = actList, selected = NULL)
      updateCheckboxGroupInput(session, "buffCheck", choices = buffList, selected = NULL)
      updateCheckboxGroupInput(session, "studyCheck", choices = studyList, selected = NULL)
      updateCheckboxGroupInput(session, "overlapCheck", choices = overlapList, selected = NULL)
      updateCheckboxGroupInput(session, "ahoiSppCheck", choices = ahoiSppList, selected = NULL)
      updateCheckboxGroupInput(session, "ahoiPolysCheck", choices = ahoiPolysList, selected = NULL)
      updateCheckboxGroupInput(session, "ahoiOthersCheck", choices = ahoiOthersList, selected = NULL)
      updateCheckboxGroupInput(session, "m1Check", choices = m1List, selected = NULL)
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
