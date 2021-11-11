library(leaflet)
library(maps)

rda_path <- '../rdas'
load(file.path(rda_path, "map.rda"))

# map_sp <- geojsonio::geojson_read("../data/municipalities.json", what = "sp")
# 
map_detail_sp <- geojsonio::geojson_read("../data/LIMITES_LEGALES_MUNICIPIOS_EDICION_MARZO2009_10PERC.json", what = "sp")
sp::proj4string(map_detail_sp) <- sp::CRS("+init=epsg:32161")
map_detail_sp <- sp::spTransform(map_detail_sp, sp::CRS("+init=epsg:4326"))
class(map_detail_sp)
map_sp <- map_detail_sp

# save(map, map_centers, map_sp, file = file.path(rda_path, 'map.rda'))

mapStates = map("state", fill = TRUE, plot = FALSE)
mapStates$names <- replace(mapStates$names, mapStates$names=='new york:staten island', 'new york:main')
vec <- mapStates$x
rl <- rle(is.na(vec))
i1 <- rl$lengths==1 & rl$values

lst <- split(vec, rep(cumsum(c(TRUE, i1[-length(i1)])), rl$lengths)) 
lst.out <- lapply(lst, function(x) x[seq_len(tail(which(!is.na(x)), 1))])


leaflet(map_sp) %>%
  # setView(-66.25789, 18.22132, 8) %>%
  fitBounds(lng2=-67.27135, lat2=17.92687,
            lng1=-65.24442, lat1=18.51576,
            options = list(padding = c(0,0))) %>%
  addTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    # lng = ~X,
    # lat = ~Y,
    # group = ~paste(municipio, part),
    fillColor = "white",
    weight = 0.15,
    opacity = 1,
    color = "black",
    dashArray = "",
    fillOpacity = 1.0,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE))


