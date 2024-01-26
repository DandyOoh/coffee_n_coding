library("tidyverse")
library("sf")
library("leaflet")
library("RColorBrewer")
library("ggmap")
library("rjson")
library("purrr")

#### Load ICB boundaries ####

icb_map <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/ICB_JUL_2022_EN_BUC_V3/FeatureServer/0/query?where=1%3D1&outFields=ICB22CD,ICB22NM,BNG_E,BNG_N&outSR=4326&f=json")
icb_map <- as(icb_map, "Spatial")

#### set up hospital locations ####

phase <- c(1,1,1,1,1,1,2,2,2,3,2,2)
hospital_map_code <- c(1,2,3,4,5,6,7,8,9,10,11,12)
hospital_name <- c('Airedale','Frimley','Hinchinbrooke','James Paget','Kings Lynn','Leighton','Watford','Whipps Cross','Hillingdon','North Manchester','PAH (Harlow)','Hampshire')
label_pos <- c('top','top','top','top','top','top','left','right','bottom','top','top','top')
postcode <- c('BD20 6TD','GU16 7UJ','PE29 6NT','NR31 6LA','PE30 4ET','CW1 4QJ','WD18 0HB','E11 1NR','UB8 3NN','M8 5RB','CM20 1QX','SO22 5DG')

nhp_list_1 <- data.frame(phase, hospital_map_code, hospital_name, label_pos, postcode)

## functions for geocoding with easting and northing ##

geocode_addys_geteast <- function(x){
  
  geocode_result <- fromJSON(readLines(paste0("http://api.getthedata.com/postcode/",gsub(" ", "", x))))
  return(ifelse(!is.null(geocode_result$data$longitude), geocode_result$data$longitude, NA))
}

geocode_addys_getnorth <- function(x){
  
  geocode_result <- fromJSON(readLines(paste0("http://api.getthedata.com/postcode/",gsub(" ", "", x))))
  return(ifelse(!is.null(geocode_result$data$latitude), geocode_result$data$latitude, NA))
}

nhp_list_1 <- nhp_list_1 |> 
  mutate(long = map_chr(postcode, geocode_addys_geteast)
         ,lat = map_chr(postcode, geocode_addys_getnorth)
         ,long = as.numeric(long)
         ,lat = as.numeric(lat))

#### generate and print map - labels overlapping ####

## may need to filter on phases for different stages of NHP project...

map <- leaflet(data = icb_map,
               options = leafletOptions(zoomControl = FALSE)) |> 
  ##addTiles(options = tileOptions(opacity = 0.7)) |> 
  addProviderTiles(providers$CartoDB) |> 
  ##addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              ##opacity = 0.7, fillOpacity = 0) |>
  addCircleMarkers(data = nhp_list_1 |> filter(phase == 2), lng = ~long, lat = ~lat, color = "blue") |> 
  addLabelOnlyMarkers(data = nhp_list_1 |> filter(phase == 2), lng=~long, lat=~lat, label =~hospital_name, 
                      labelOptions = labelOptions(noHide = T, direction = "top", textOnly = T, textsize = "12px",
                                                  style = list("color" = "red", "font-weight" = "bold")))

map

#### generate and print map - fixed labels overlapping (fudge!) ####

nhp_list_1_left <- nhp_list_1 |> filter(label_pos == "left", phase == 2)
nhp_list_1_right <- nhp_list_1 |> filter(label_pos == "right", phase == 2)
nhp_list_1_top <- nhp_list_1 |> filter(label_pos == "top", phase == 2)
nhp_list_1_bottom <- nhp_list_1 |> filter(label_pos == "bottom", phase == 2)

map_2 <- leaflet(data = icb_map,
               options = leafletOptions(zoomControl = FALSE)) |> 
  ##addTiles(options = tileOptions(opacity = 0.7)) |> 
  addProviderTiles(providers$CartoDB) |> 
  ##addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
  ##opacity = 0.7, fillOpacity = 0) |>
  addCircleMarkers(data = nhp_list_1 |> filter(phase == 2), lng = ~long, lat = ~lat, color = "blue") |> 
  addLabelOnlyMarkers(data = nhp_list_1_left |> filter(phase == 2), lng=~long, lat=~lat, label =~hospital_name, 
                      labelOptions = labelOptions(noHide = T, direction = "left", offset = c(-15, 0), textOnly = T, textsize = "12px",
                                                  style = list("color" = "red", "font-weight" = "bold"))) |> 
  addLabelOnlyMarkers(data = nhp_list_1_right |> filter(phase == 2), lng=~long, lat=~lat, label =~hospital_name, 
                      labelOptions = labelOptions(noHide = T, direction = "right", offset = c(15, 0), textOnly = T, textsize = "12px",
                                                  style = list("color" = "red", "font-weight" = "bold"))) |> 
  addLabelOnlyMarkers(data = nhp_list_1_top |> filter(phase == 2), lng=~long, lat=~lat, label =~hospital_name, 
                      labelOptions = labelOptions(noHide = T, direction = "top", textOnly = T, textsize = "12px",
                                                  style = list("color" = "red", "font-weight" = "bold"))) |> 
  addLabelOnlyMarkers(data = nhp_list_1_bottom |> filter(phase == 2), lng=~long, lat=~lat, label =~hospital_name, 
                      labelOptions = labelOptions(noHide = T, direction = "bottom", textOnly = T, textsize = "12px",
                                                  style = list("color" = "red", "font-weight" = "bold")))

map_2






