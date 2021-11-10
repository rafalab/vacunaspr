library(leaflet)

rda_path <- '../rdas'
load(file.path(rda_path, "map.rda"))

map_sp <- geojsonio::geojson_read("../data/municipalities.json", what = "sp")

map_detail_sp <- geojsonio::geojson_read("../data/LIMITES_LEGALES_MUNICIPIOS_EDICION_MARZO2009.json", what = "sp")
sp::proj4string(map_detail_sp) <- sp::CRS("+init=epsg:32161")
map_detail_sp <- sp::spTransform(map_detail_sp, sp::CRS("+init=epsg:4326"))
class(map_detail_sp)

# save(map, map_centers, map_sp, map_detail_sp, file = file.path(rda_path, 'map.rda'))

leaflet(map_sp) %>%
  # setView(-66.25789, 18.22132, 8) %>%
  fitBounds(lng2=-67.27135, lat2=17.92687,
            lng1=-65.24442, lat1=18.51576,
            options = list(padding = c(0,0))) %>%
  addTiles(providers$CartoDB.Positron) %>%
  addPolygons(
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


