library(leaflet)

# rda_path <- '../rdas'
# load(file.path(rda_path, "map.rda"))

muni_sp <- geojsonio::geojson_read("../data/municipalities.json", what = "sp")
class(muni_sp)

leaflet(muni_sp) %>%
  setView(-66.30, 18.2208, 8.1) %>%
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


