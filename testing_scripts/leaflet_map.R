library(leaflet)

rda_path <- '../rdas'
load(file.path(rda_path, "map.rda"))

map_sp <- geojsonio::geojson_read("../data/municipalities.json", what = "sp")
class(map_sp)

# save(map, map_centers, map_sp, file = file.path(rda_path, 'map.rda'))

leaflet(map_sp) %>%
  setView(-66.25789, 18.22132, 8) %>%
  # fitBounds(lng2=-67.27135, lat2=17.92687,
  #           lng1=-65.24442, lat1=18.51576,
  #           options = list(padding = c(0,0))) %>%
  addTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    # fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE))


