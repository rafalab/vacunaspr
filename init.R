# -- Libraries
library(kableExtra)
library(tidyverse)
library(lubridate)
library(leaflet)
source("functions.R")


## if on the server get the latest data
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/vacunaspr/rdas"
} else{
  rda_path <- "rdas"
}
# -- Set locale
Sys.setlocale("LC_TIME", "es_ES")


load(file.path(rda_path, "dates.rda"))

manu_labels <- c(UNV = "No vacunados", MOD = "Moderna",
                 PFR = "Pfizer", JSN = "J & J")

manu_colors <- c(UNV =RColorBrewer::brewer.pal(9, "Set1")[1] , MOD = "#00BFC4", PFR = "#C77CFF", JSN = "#7CAE00")

municipios <- c("Adjuntas", "Aguada", "Aguadilla", "Aguas Buenas", "Aibonito", "Añasco",
                "Arecibo", "Arroyo", "Barceloneta", "Barranquitas", "Bayamón", "Cabo Rojo", 
                "Caguas", "Camuy", "Canóvanas", "Carolina", "Cataño", "Cayey", "Ceiba",
                "Ciales", "Cidra", "Coamo", "Comerío", "Corozal", "Culebra", "Dorado", 
                "Fajardo", "Florida", "Guánica", "Guayama", "Guayanilla", "Guaynabo", 
                "Gurabo", "Hatillo", "Hormigueros", "Humacao", "Isabela", "Jayuya", 
                "Juana Díaz", "Juncos", "Lajas", "Lares", "Las Marías", "Las Piedras", 
                "Loíza", "Luquillo", "Manatí", "Maricao", "Maunabo", "Mayagüez", "Moca", 
                "Morovis", "Naguabo", "Naranjito", "Orocovis", "Patillas", "Peñuelas", 
                "Ponce", "Quebradillas", "Rincón", "Río Grande", "Sabana Grande", "Salinas", 
                "San Germán", "San Juan", "San Lorenzo", "San Sebastián", "Santa Isabel", 
                "Toa Alta", "Toa Baja", "Trujillo Alto", "Utuado", "Vega Alta", "Vega Baja", 
                "Vieques", "Villalba", "Yabucoa", "Yauco")
names(municipios) <- municipios