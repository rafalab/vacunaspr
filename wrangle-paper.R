library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(tidycensus)

rda_path <- "rdas"
pop_year <- 2019

# Age groups --------------------------------------------------------------

collapse_age <- function(tab, age_starts){
  breaks <- sort(age_starts)
  labels <- c(paste(breaks[-length(breaks)], c(breaks[-1]-1), sep="-"),
              paste0(breaks[length(breaks)], "+"))
  
  ret <- copy(tab)
  ret[, ageRange := cut(start, c(age_starts, Inf), 
                        right = FALSE, include.lowest = TRUE, 
                        labels = labels)]
  ret[, c("start", "end") := NULL]
  
  vars <- "poblacion"
  
  if("se" %in% names(ret)){ ret[, se := se^2]; vars <- c(vars, "se")}
  
  cols <- setdiff(names(ret), vars)
  
  ret <- ret[, lapply(.SD, sum), keyby = cols, .SDcols = vars]
  
  if("se" %in% names(ret)) ret[, se := sqrt(se)]
  
  ret[, ageRange := factor(ageRange, levels = labels)]
  
  return(ret[])
}

split_10_14 <- function(tab){
  tmp1 <- tab[!start %in% c(10,12)]
  tmp2 <- tab[start %in% c(10,12)]
  
  if("se" %in% names(tmp1)){
    f <- function(tab) data.table(start=c(10,12), end=c(11,14), 
                                  poblacion=tab$poblacion*c(2,3)/5,
                                  se = tab$se*sqrt(c(2,3)/5))
  } else{
    f <- function(tab) data.table(start=c(10,12), end=c(11,14), 
                                  poblacion=tab$poblacion*c(2,3)/5)
  }
    
  keys <- setdiff(names(tmp2), c("start", "end", "poblacion", "se"))
  tmp2 <- tmp2[, f(.SD), by = keys]
  setcolorder(tmp2, names(tmp1))
  ret <- rbindlist(list(tmp1, tmp2))[order(start, gender)]
  return(ret[])
}

load(file.path(rda_path, "population-tabs-acs.rda"))
muni_levels <- c(levels(raw_pop_municipio$municipio), "No reportado")

## pick a year to use as population estimates
setnames(raw_pop, paste0("poblacion_", pop_year), "poblacion")
out <- str_subset(names(raw_pop), "poblacion_")
raw_pop[, (out) := NULL]
setnames(raw_pop_municipio, paste0("poblacion_", pop_year), "poblacion")
out <- str_subset(names(raw_pop_municipio), "poblacion_")
raw_pop_municipio[, (out) := NULL]

## split 10-14
raw_pop <- split_10_14(raw_pop)
raw_pop_municipio <- split_10_14(raw_pop_municipio)

age_starts <- c(0, 5, 12, 18, 30, 40, 50, 60, 70, 80)
pop_by_age_gender <- collapse_age(raw_pop, age_starts)
pop_by_age_gender_municipio <- collapse_age(raw_pop_municipio, age_starts)

age_levels <- levels(pop_by_age_gender$ageRange)

pr_pop <- sum(raw_pop$poblacion)
pr_pop_se <- sqrt(sum(raw_pop$se^2))

pr_adult_pop <- sum(raw_pop[end<=17]$poblacion)
pr_adult_pop_se <- sqrt(sum(raw_pop[end<=17]$se^2))


message("Wrangling cases.")

source("counpute-counts.R")

save(dat_cases_vax, file = file.path(rda_path, "dat_cases_vax.rda"))

rm(dat_cases_vax); gc();gc()
rm(pop_vax); gc(); gc()
rm(pop_par); gc(); gc()
rm(all_bst_combs); gc(); gc()
rm(pop_unvax); gc();gc()

# Prepare data for dashboard ----------------------------------------------

### collapse ageRanges
save(counts, file=file.path(rda_path ,"paper-counts.rda"))
