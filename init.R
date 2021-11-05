# -- Libraries
library(kableExtra)
library(tidyverse)
library(lubridate)
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
