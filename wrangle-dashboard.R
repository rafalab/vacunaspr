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

## Check if it matches
# tmp <- pop_by_age_gender_municipio[,.(poblacion=sum(poblacion), se=sqrt(sum(se^2))),
#                          by = c("ageRange", "gender")]
# tmp <- merge(tmp, pop_by_age_gender, by = c("ageRange", "gender"))
# tmp%>%ggplot(aes(poblacion.x, poblacion.y, label=ageRange))+
#   geom_text()+geom_abline()
# tmp%>%ggplot(aes(se.x, se.y, label=ageRange))+
#   geom_text()+geom_abline()

# Define global variables -------------------------------------------------
load(file.path(rda_path, "dates_SYA.rda"))
manu_levels <- c("UNV", "MOD", "PFR", "JSN")
first_ped_day <- make_date(2021, 11, 04)
first_booster_day <- make_date(2021, 8, 13) 
first_jnj_booster_day <- make_date(2021, 10, 22) 


message("Wrangling vaccine data.")

# Load data ---------------------------------------------------------------

load(file.path(rda_path, "dat_vax_SYA.rda"))

## collapse ages
collapse_func <- function(x) cut(x, c(age_starts, Inf),  right = FALSE, include.lowest = TRUE, labels = age_levels)
cols <- str_subset(names(dat_vax), "ageRange")
dat_vax[, (cols) := lapply(.SD, collapse_func), .SDcols = cols]

## define last_immunne_date
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 != "JSN", last_immune_date := date_2 + days(150)]
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 == "JSN", last_immune_date := date_1 + days(60)]


## load population


all_dates <- data.table(date = seq(first_day, last_day, "days"))

all_combs <- CJ(date = all_dates$date, 
                ageRange = levels(dat_vax$ageRange_1),
                gender = levels(dat_vax$gender),
                manu = levels(dat_vax$manu_1))

all_combs_muni <- CJ(date = all_dates$date, 
                ageRange = levels(dat_vax$ageRange_1),
                gender = levels(dat_vax$gender),
                manu = levels(dat_vax$manu_1),
                municipio = muni_levels)

# daily_vax_counts --------------------------------------------------------
cat("Computing daily vax counts: ")

## Fully vaxed
cat("fully vaxed, ")

daily_counts_vax_age_gender_manu <- dat_vax[!is.na(vax_date), .(full = .N),
                                            keyby = .(vax_date, vax_ageRange, gender, manu_1)]
names(daily_counts_vax_age_gender_manu) <- c("date", "ageRange", "gender", "manu", "full")
daily_counts_vax_age_gender_manu <- merge(all_combs, daily_counts_vax_age_gender_manu, all.x=TRUE)
daily_counts_vax_age_gender_manu[is.na(daily_counts_vax_age_gender_manu)] <- 0 
counts_vax_age_gender_manu <- copy(daily_counts_vax_age_gender_manu)
counts_vax_age_gender_manu[, full := cumsum(full), keyby = .(ageRange, gender, manu)]
setnames(counts_vax_age_gender_manu, "full", "n")

## One dose
cat("one dose, ")

daily_counts_onedose_age_gender_manu <- dat_vax[!is.na(date_1), .(onedose = .N),
                                            keyby = .(date_1, ageRange_1, gender, manu_1)]
names(daily_counts_onedose_age_gender_manu) <- c("date", "ageRange", "gender", "manu", "onedose")
daily_counts_onedose_age_gender_manu <- merge(all_combs, 
                                              daily_counts_onedose_age_gender_manu, 
                                              all.x=TRUE)
daily_counts_onedose_age_gender_manu[is.na(daily_counts_onedose_age_gender_manu)] <- 0 
counts_onedose_age_gender_manu <- copy(daily_counts_onedose_age_gender_manu)
counts_onedose_age_gender_manu[, onedose := cumsum(onedose), keyby = .(ageRange, gender, manu)]
setnames(counts_onedose_age_gender_manu, "onedose", "n")

## Partially vaxed
cat("partial, ")

counts_partial_age_gender_manu<- merge(counts_vax_age_gender_manu, 
                                       counts_onedose_age_gender_manu, 
                                       all = TRUE, by = c("date", "ageRange", "gender", "manu"))
counts_partial_age_gender_manu[ , n := n.y - n.x]
counts_partial_age_gender_manu <- counts_partial_age_gender_manu[, !c("n.y", "n.x")]

## Boosters
cat("booster, ")

daily_counts_booster_age_gender_manu <- dat_vax[!is.na(booster_date), .(booster = .N),
                                                keyby = .(booster_date, booster_ageRange, gender, booster_manu)]
names(daily_counts_booster_age_gender_manu) <- c("date", "ageRange", "gender", "manu", "booster")
daily_counts_booster_age_gender_manu <- merge(all_combs, daily_counts_booster_age_gender_manu, all.x=TRUE)
daily_counts_booster_age_gender_manu[is.na(daily_counts_booster_age_gender_manu)] <- 0 
counts_booster_age_gender_manu <- copy(daily_counts_booster_age_gender_manu)
counts_booster_age_gender_manu[, booster := cumsum(booster), keyby = .(ageRange, gender, manu)]
setnames(counts_booster_age_gender_manu, "booster", "n")

## Lost original immunnity
cat("lost immunity.\n")

daily_counts_lost_age_gender_manu <- dat_vax[!is.na(last_immune_date), .(lost = .N),
                                            keyby = .(last_immune_date, vax_ageRange, gender, manu_1)]
names(daily_counts_lost_age_gender_manu) <- c("date", "ageRange", "gender", "manu", "lost")
#daily_counts_lost_age_gender_manu %>% group_by(week=round_date(date,"week")) %>% summarize(need_booster=sum(lost)) %>% ggplot(aes(week, need_booster)) + scale_y_continuous(labels = scales::comma) + geom_line() 
daily_counts_lost_age_gender_manu <- merge(all_combs, daily_counts_lost_age_gender_manu, all.x=TRUE)
daily_counts_lost_age_gender_manu[is.na(daily_counts_lost_age_gender_manu)] <- 0 
counts_lost_age_gender_manu <- copy(daily_counts_lost_age_gender_manu)
counts_lost_age_gender_manu[, lost := cumsum(lost), keyby = .(ageRange, gender, manu)]
immune <- merge(counts_lost_age_gender_manu, counts_vax_age_gender_manu, 
                all = TRUE, by = c("date", "ageRange", "gender", "manu"))
immune[ , n := n - lost]
immune <- immune[, !c("lost")]

daily_vax_counts <- merge(daily_counts_onedose_age_gender_manu, 
                      daily_counts_vax_age_gender_manu, 
                      all = TRUE, by = c("date", "ageRange", "gender", "manu"))

daily_vax_counts <- merge(daily_vax_counts, 
                      daily_counts_booster_age_gender_manu, 
                      all = TRUE, by = c("date", "ageRange", "gender", "manu"))

daily_vax_counts <- merge(daily_vax_counts, 
                          daily_counts_lost_age_gender_manu, 
                          all = TRUE, by = c("date", "ageRange", "gender", "manu"))

daily_vax_counts$ageRange <- factor(daily_vax_counts$ageRange, levels = age_levels)

# daily_vax_counts_by_municipio -------------------------------------------

cat("Computing daily vax counts for municipios: ")

## Fully vaxed
cat("fully vaxed, ")

daily_counts_vax_age_gender_manu_muni <- dat_vax[!is.na(vax_date), .(full = .N),
                                            keyby = .(vax_date, vax_ageRange, gender, manu_1, municipio)]
names(daily_counts_vax_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "full")
daily_counts_vax_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_vax_age_gender_manu_muni, all.x=TRUE)
daily_counts_vax_age_gender_manu_muni[is.na(daily_counts_vax_age_gender_manu_muni)] <- 0 
counts_vax_age_gender_manu_muni <- copy(daily_counts_vax_age_gender_manu_muni)
counts_vax_age_gender_manu_muni[, full := cumsum(full), keyby = .(ageRange, gender, manu, municipio)]
setnames(counts_vax_age_gender_manu_muni, "full", "n")

## One dose
cat("one dose, ")

daily_counts_onedose_age_gender_manu_muni <- dat_vax[!is.na(date_1), .(onedose = .N),
                                                keyby = .(date_1, ageRange_1, gender, manu_1, municipio)]
names(daily_counts_onedose_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "onedose")
daily_counts_onedose_age_gender_manu_muni <- merge(all_combs_muni, 
                                              daily_counts_onedose_age_gender_manu_muni, 
                                              all.x=TRUE)
daily_counts_onedose_age_gender_manu_muni[is.na(daily_counts_onedose_age_gender_manu_muni)] <- 0 
counts_onedose_age_gender_manu_muni <- copy(daily_counts_onedose_age_gender_manu_muni)
counts_onedose_age_gender_manu_muni[, onedose := cumsum(onedose), keyby = .(ageRange, gender, manu, municipio)]
setnames(counts_onedose_age_gender_manu_muni, "onedose", "n")

## Partially vaxed
cat("partial, ")
counts_partial_age_gender_manu_muni<- merge(counts_vax_age_gender_manu_muni,
                                       counts_onedose_age_gender_manu_muni,
                                       all = TRUE, by = c("date", "ageRange", "gender", "manu", "municipio"))
counts_partial_age_gender_manu_muni[ , n := n.y - n.x]
counts_partial_age_gender_manu_muni <- counts_partial_age_gender_manu_muni[, !c("n.y", "n.x")]

## Boosters
cat("boosters, ")

daily_counts_booster_age_gender_manu_muni <- dat_vax[!is.na(booster_date), .(booster = .N),
                                                keyby = .(booster_date, booster_ageRange, gender, booster_manu, municipio)]
names(daily_counts_booster_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "booster")
daily_counts_booster_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_booster_age_gender_manu_muni, all.x=TRUE)
daily_counts_booster_age_gender_manu_muni[is.na(daily_counts_booster_age_gender_manu_muni)] <- 0 
counts_booster_age_gender_manu_muni <- copy(daily_counts_booster_age_gender_manu_muni)
counts_booster_age_gender_manu_muni[, booster := cumsum(booster), keyby = .(ageRange, gender, manu, municipio)]
setnames(counts_booster_age_gender_manu_muni, "booster", "n")

## Lost immunnity
cat("lost immuninty.\n")
daily_counts_lost_age_gender_manu_muni <- dat_vax[!is.na(last_immune_date), .(lost = .N),
                                             keyby = .(last_immune_date, vax_ageRange, gender, manu_1, municipio)]
names(daily_counts_lost_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "lost")
#daily_counts_lost_age_gender_manu_muni %>% group_by(week=round_date(date,"week")) %>% summarize(need_booster=sum(lost)) %>% ggplot(aes(week, need_booster)) +  scale_y_continuous(labels = scales::comma) + geom_line() 
daily_counts_lost_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_lost_age_gender_manu_muni, all.x=TRUE)
daily_counts_lost_age_gender_manu_muni[is.na(daily_counts_lost_age_gender_manu_muni)] <- 0 
counts_lost_age_gender_manu_muni <- copy(daily_counts_lost_age_gender_manu_muni)
counts_lost_age_gender_manu_muni[, lost := cumsum(lost), keyby = .(ageRange, gender, manu, municipio)]

daily_vax_counts_by_municipio <- merge(daily_counts_onedose_age_gender_manu_muni, 
                          daily_counts_vax_age_gender_manu_muni, 
                          all = TRUE, by = c("date", "ageRange", "gender", "manu", "municipio"))

daily_vax_counts_by_municipio <- merge(daily_vax_counts_by_municipio, 
                          daily_counts_booster_age_gender_manu_muni, 
                          all = TRUE, by = c("date", "ageRange", "gender", "manu", "municipio"))

daily_vax_counts_by_municipio <- merge(daily_vax_counts_by_municipio, 
                          daily_counts_lost_age_gender_manu_muni, 
                          all = TRUE, by = c("date", "ageRange", "gender", "manu", "municipio"))

daily_vax_counts_by_municipio$ageRange <- factor(daily_vax_counts_by_municipio$ageRange, levels = age_levels)

daily_vax_counts_by_municipio$municipio <- 
  factor(daily_vax_counts_by_municipio$municipio, levels = muni_levels)

# Creating table with population sizes of each vax status
message("Computing per day totals.")
unvax <- counts_onedose_age_gender_manu[ , .(total = sum(n)), 
                                         keyby = .(date, ageRange, gender)]
unvax <- merge(unvax, pop_by_age_gender, all.x = TRUE, 
               by = c("ageRange", "gender")) 
unvax[,n := poblacion - total]
unvax[,manu := "UNV"]
unvax <- unvax[, c("date", "ageRange", "gender", "manu", "n")]

## counting only vax
counts_vax_age_gender_manu <- merge(counts_vax_age_gender_manu, counts_booster_age_gender_manu, by = c("date","ageRange","gender","manu"), all.x=TRUE)
counts_vax_age_gender_manu[, n:=n.x-n.y]
counts_vax_age_gender_manu <- counts_vax_age_gender_manu[, !c("n.x","n.y")]
poblacion <- rbind(counts_booster_age_gender_manu[,status:="BST"],
                   counts_vax_age_gender_manu[,status:="VAX"],
                   counts_partial_age_gender_manu[,status:="PAR"],
                   unvax[,status:="UNV"])

poblacion$status <- factor(poblacion$status, levels = c("UNV", "PAR", "VAX", "BST"))
poblacion$ageRange <- factor(poblacion$ageRange, levels = age_levels)
#poblacion %>%  filter(status == "PAR") %>% ggplot(aes(date, n, color = gender)) + geom_line() + facet_grid(manu~ageRange) 
poblacion[gender=="O" & status == "UNV", n := 0]

### by municipio
unvax <- counts_onedose_age_gender_manu_muni[ , .(total = sum(n)), 
                                              keyby = .(date, ageRange, gender, municipio)]
unvax <- merge(unvax, pop_by_age_gender_municipio, all.x = TRUE, 
               by = c("ageRange", "gender", "municipio")) 
unvax[, n := poblacion - total]
unvax[ ,manu := "UNV"]
unvax <- unvax[,c("date", "municipio","ageRange","gender","manu", "n")]

counts_vax_age_gender_manu_muni <- merge(counts_vax_age_gender_manu_muni, counts_booster_age_gender_manu_muni, by = c("date","ageRange","gender","manu", "municipio"), all.x=TRUE)
counts_vax_age_gender_manu_muni[, n:=n.x-n.y]
counts_vax_age_gender_manu_muni <- counts_vax_age_gender_manu_muni[, !c("n.x","n.y")]

poblacion_muni <- rbind(counts_booster_age_gender_manu_muni[,status:="BST"],
                        counts_vax_age_gender_manu_muni[,status:="VAX"],
                   counts_partial_age_gender_manu_muni[,status:="PAR"],
                   unvax[,status:="UNV"])
poblacion_muni$status <- factor(poblacion_muni$status, levels = c("UNV", "PAR", "VAX","BST"))
poblacion_muni$ageRange <- factor(poblacion_muni$ageRange, levels = age_levels)
poblacion_muni$municipio <- 
  factor(poblacion_muni$municipio, levels = muni_levels)
poblacion_muni[gender=="O" & status == "UNV", n := 0]

message("Computing piramdes.")

### PIRAMIDE
tab <- poblacion %>% 
  filter(date == max(date) & gender %in% c("M","F")) %>%
  #collapse_age(vars="n") %>%
  group_by(ageRange, gender, status) %>%
  summarize(n = sum(n), .groups = "drop") %>%
  mutate(n = pmax(0, n)) %>%
  mutate(n = ifelse(gender=="M", -n, n)) %>%
  arrange(gender, ageRange) %>%
  mutate(n = n/sum(abs(n))) %>%
  mutate(estatus = recode(status, UNV = "No vacunados", PAR = "Parcial", VAX = "Vacunados sin booster", BST = "Con booster")) %>%
  mutate(municipio = "Todos")

tab_muni <- poblacion_muni %>% 
  filter(municipio!="No reportado") %>%
  filter(date == max(date) & gender %in% c("M","F")) %>%
  #collapse_age(vars="n") %>%
  group_by(municipio, ageRange, gender, status) %>%
  summarize(n = sum(n), .groups = "drop") %>%
  mutate(n = pmax(0, n)) %>%
  mutate(n = ifelse(gender=="M", -n, n)) %>%
  arrange(municipio, gender, ageRange) %>%
  group_by(municipio) %>%
  mutate(n = n/sum(abs(n))) %>%
  ungroup() %>%
  mutate(estatus = recode(status, UNV = "No vacunados", PAR = "Parcial", VAX = "Vacunados sin booster", BST = "Con booster")) 

piramide <- bind_rows(tab, tab_muni)


## piramide tab

piramide_tab <- daily_vax_counts %>%   
  left_join(pop_by_age_gender, by = c("ageRange", "gender")) %>%
  filter(gender %in% c("M", "F")) %>%
  group_by(ageRange, gender) %>%
  summarize(onedose = sum(onedose), full = sum(full),
            booster=sum(booster), lost = sum(lost), 
            poblacion=poblacion[1], .groups = "drop") %>%
  mutate(immune = full - lost) %>%
  mutate(poblacion = pmax(poblacion, onedose)) %>%
  mutate(municipio = "Todos")

piramide_tab_muni <- daily_vax_counts_by_municipio %>% 
  left_join(pop_by_age_gender_municipio, by = c("municipio", "ageRange", "gender")) %>%
  filter(gender %in% c("M", "F") & municipio!="No reportado") %>%
  group_by(municipio, ageRange, gender) %>%
  summarize(onedose = sum(onedose), full = sum(full),
            booster=sum(booster), lost = sum(lost), 
            poblacion=poblacion[1], .groups = "drop") %>%
  mutate(poblacion=pmax(poblacion, onedose)) %>%
  mutate(immune = full - lost) 
 
piramide_tab <- bind_rows(piramide_tab, piramide_tab_muni)
 
## Proveedores

message("Wrangling proveedores.")

cols1 <- grep("_1", names(dat_vax), value=TRUE)
cols2 <- grep("_2", names(dat_vax), value=TRUE)
cols3 <- grep("_3", names(dat_vax), value=TRUE)
the_colnames <- gsub("_1", "", cols1)
proveedores <- rbindlist(list(
  setnames(dat_vax[,..cols1], the_colnames)[,dose:="Primera"],
  setnames(dat_vax[,..cols2], the_colnames)[,dose:="Segunda"],
  setnames(dat_vax[,..cols3], the_colnames)[,dose:="Tercera"])
)
proveedores[, index := insert_date >= today() -weeks(1)]  
proveedores[, diff := as.numeric(insert_date) - as.numeric(date)]

my_func <- function(tab){
  with(tab, list(total = length(diff),
                 rezago = mean(diff), 
                 entradas_esta_semana = sum(index), 
                 rezago_esta_semana = mean(diff[index])))
}
proveedores <- proveedores[!is.na(proveedor) & !(dose=="Segunda" & manu == "JSN"),
                           my_func(.SD), 
                           by=.(proveedor, date, dose, manu, ageRange),
                           .SDcols = c("diff", "index")]



rm(daily_counts_booster_age_gender_manu,
   daily_counts_lost_age_gender_manu,
   daily_counts_vax_age_gender_manu,
   counts_partial_age_gender_manu, 
   counts_vax_age_gender_manu,
   daily_counts_onedose_age_gender_manu_muni, 
   daily_counts_booster_age_gender_manu_muni,
   daily_counts_lost_age_gender_manu_muni,
   daily_counts_vax_age_gender_manu_muni,
   counts_onedose_age_gender_manu_muni, 
   counts_partial_age_gender_manu_muni, 
   counts_vax_age_gender_manu_muni,
   piramide_tab_muni,
   tab, tab_muni, unvax,
   all_combs, all_combs_muni)
gc();gc()
## save files

save(proveedores, file=file.path(rda_path ,"proveedores.rda"))
save(poblacion, poblacion_muni, file = file.path(rda_path ,"poblacion.rda"))

rm(proveedores, poblacion, poblacion_muni); gc(); gc()


## Prepare for dashboard


### Summary tab
primera <- sum(!is.na(dat_vax$date_1))
primera_prop <- primera/pr_pop

completa <- sum(!is.na(dat_vax$vax_date))
completa_prop <- completa/pr_pop

the_immune <- immune %>% filter(date == last_day) %>% summarize(n=sum(n)) %>% pull(n)
the_immune_prop <- the_immune/pr_pop

booster <- sum(!is.na(dat_vax$booster_date))
booster_prop <- booster/pr_pop

pediatric_primera <- daily_vax_counts %>% filter(ageRange=="5-11" & date >= first_ped_day) %>%
  pull(onedose) %>% sum(na.rm=TRUE)

ped_pop <- pop_by_age_gender %>% 
  filter(ageRange=="5-11") %>% 
  pull(poblacion) %>% 
  sum()

ped2_pop <- pop_by_age_gender %>% 
  filter(ageRange=="12-17") %>% 
  pull(poblacion) %>% 
  sum()

pediatric_primera_prop <- pediatric_primera / ped_pop

pediatric_completa <- daily_vax_counts %>% filter(ageRange=="5-11" & date >= first_ped_day) %>%
  pull(full) %>% sum(na.rm=TRUE)

pediatric_completa_prop <- pediatric_completa/ ped_pop

lost <-  sum(daily_vax_counts$lost)
lost_prop <-lost/pr_pop

four_dose <- sum(!is.na(dat_vax$extra_dose))

booster_ped <- daily_vax_counts %>% filter(ageRange=="12-17" & date >= make_date(2022,01,06)) %>%
  pull(booster) %>% sum(na.rm=TRUE)

booster_ped_prop <- booster_ped/ ped2_pop

lost_ped <- daily_vax_counts %>% filter(ageRange=="12-17" & date >= make_date(2022,01,06)) %>%
  pull(lost) %>% sum(na.rm=TRUE)

lost_ped_prop <- lost_ped/ ped2_pop

tasas <- daily_vax_counts %>% 
  filter(date %in% span_range) %>%
  summarize(onedose = sum(onedose)/length(span_range) * 7, 
            full = sum(full)/length(span_range)* 7 , 
            immune = (sum(full) - sum(lost))/length(span_range)*7,
            booster = sum(booster)/length(span_range)  * 7, 
            lost = sum(lost)/length(span_range)*7)


tasas_ped <- daily_vax_counts %>% 
  filter(date %in% span_range & ageRange=="5-11") %>%
  summarize(onedose = sum(onedose)/length(span_range) * 7, 
            full= sum(full)/length(span_range) * 7)


tasas_ped_2 <- daily_vax_counts %>% 
  filter(date %in% span_range & ageRange=="12-17") %>%
  summarize(onedose = sum(onedose)/length(span_range) * 7, 
            full= sum(full)/length(span_range) * 7, 
            immune = (sum(full) - sum(lost))/length(span_range)*7,
            booster = sum(booster)/length(span_range)  * 7, 
            lost = sum(lost)/length(span_range)*7)


summary_tab <- data.frame(names = c("Vacunas administradas",
                                    "Personas con vacunación al día",
                                    "Personas con por lo menos 1 dosis",
                                    "Personas con serie primaria completa",
                                    "Personas con boosters",
                                    "Personas con serie primaria completa con necesidad de booster (vacunación no al día)",
                                    "Menores (12-15 años) con booster",
                                    "Menores (12-15 años) con necesidad de booster (vacunación no al día)",
                                    "Menores (5-11 años) con por lo menos 1 dosis",
                                    "Menores (5-11 años) con serie primaria completa"),
                          total = c(administradas, the_immune, primera, completa, booster, lost, booster_ped, lost_ped, pediatric_primera, pediatric_completa),
                          porciento = c(NA, the_immune_prop, primera_prop, completa_prop,  booster_prop,  lost_prop, booster_ped_prop, lost_ped_prop,  pediatric_primera_prop, pediatric_completa_prop),
                          tasas = c(administradas_tasa, tasas$immune, tasas$onedose, tasas$full,  tasas$booster,  tasas$lost, tasas_ped_2$booster, tasas_ped_2$lost, tasas_ped$onedose, tasas_ped$full))



# Cases -------------------------------------------------------------------

message("Wrangling cases.")

counts_age_starts <- c(0, 5, 12, 18, 40, 60)

source("counpute-counts.R")

save(dat_cases_vax, file = file.path(rda_path, "dat_cases_vax.rda"))

rm(dat_cases_vax); gc();gc()
rm(pop_vax); gc(); gc()
rm(pop_par); gc(); gc()
rm(all_bst_combs); gc(); gc()
rm(pop_unvax); gc();gc()

# Prepare data for dashboard ----------------------------------------------

counts <- counts[!(ageRange == "0-4" |
                                 (ageRange %in% c("5-11", "12-17") & manu%in%c("MOD", "JSN")) |
                                 (ageRange == "5-11" & status != "UNV" & vax_date<first_ped_day) |
                                 (status == "BST" & manu == "JSN"))]

counts[, status := fcase(
  status == "PAR", "Vacunación parcial",
  status == "UNV", "No vacunados",
  (status == "VAX" & ((manu!="JSN" & day < 150) | (manu=="JSN" & day < 60))) | status=="BST", "Vacunación al día",  
  default = "Vacunación no al día")]
counts$status <- factor(counts$status, levels=c("No vacunados", "Vacunación parcial", "Vacunación no al día", "Vacunación al día"))
 
counts <- counts[, keyby = .(date, ageRange, gender, outcome, status, manu), 
            lapply(.SD, sum), 
            .SDcols = c("obs", "poblacion")]

counts <- counts[order(date)]

#counts %>% ggplot(aes(date, poblacion, color = manu, lty=gender))+geom_line() + facet_grid(status~ageRange)
## Compute cases, hosp and death rates
## Include other genders in totals
last_day_counts <- last_day
tmp <- counts %>%
  filter( date > last_day_counts - days(14) - days(28) & date<=last_day_counts - days(14)) 

outcome_tab_totals <- tmp %>%
  group_by(outcome, ageRange, status, manu) %>%
  summarize(obs = sum(obs),
            .groups = "drop") 

## compute rates only on M and F as we don't have unvax populations for others
outcome_tab_rates <- tmp %>%
  filter(gender != "O") %>%
  group_by(date,outcome, ageRange, status, manu) %>%
  summarize(poblacion = sum(poblacion), obs=sum(obs), .groups = "drop")  %>%
  group_by(outcome, ageRange, status, manu) %>%
  summarize(n = mean(poblacion), 
            rate = sum(obs),
            .groups = "drop") %>%
  mutate(rate = rate/7/n) 

outcome_tab_details <- left_join(outcome_tab_totals, outcome_tab_rates, 
                                 by = c("outcome", "ageRange", "status", "manu")) %>%
  arrange(desc(ageRange)) %>%
  filter(!(status == "BST" & manu == "JSN")) %>%
  pivot_wider(names_from = outcome, values_from = c("obs","rate"))

tmp <- filter(tmp, status != "Vacunación parcial") 

outcome_tab_totals <- tmp %>%
  group_by(outcome, ageRange, status) %>%
  summarize(obs = sum(obs), .groups = "drop") 

## compute rates only on M and F as we don't have unvax populations for others
outcome_tab_rates <- tmp %>%
  filter(gender != "O") %>%
  group_by(outcome, date, ageRange, status) %>%
  summarize(n = sum(poblacion), obs= sum(obs), .groups = "drop")  %>%
  group_by(outcome, ageRange, status) %>%
  summarize(n = mean(n), 
            rate=sum(obs),
            .groups = "drop") %>%
  mutate(rate = rate/7/n)  

outcome_tab <- left_join(outcome_tab_totals, outcome_tab_rates, 
                                 by = c("outcome", "ageRange", "status")) %>%
  arrange(desc(ageRange)) %>%
  pivot_wider(names_from = outcome, values_from = c("obs","rate"))

# totals <- poblacion %>% 
#   filter(date == last_day) %>%
#   group_by(manu, status) %>%
#   summarize(total = sum(n, na.rm=TRUE), .groups = "drop")  

#outcome_tab <- left_join(outcome_tab, totals, by = c("manu", "status"))

message("Saving data.")

### collapse ageRanges
save(counts, file=file.path(rda_path ,"dashboard-counts.rda"))

save(summary_tab, outcome_tab, outcome_tab_details, file=file.path(rda_path ,"tabs.rda"))

save(daily_vax_counts, pop_by_age_gender, file = file.path(rda_path, "daily_vax_counts.rda"))

save(pop_by_age_gender_municipio, daily_vax_counts_by_municipio, file = file.path(rda_path, "daily_vax_counts_by_municipio.rda"))

save(piramide, piramide_tab, file = file.path(rda_path, "piramide.rda"))

save(immune, file = file.path(rda_path ,"immune.rda"))

save("counts_age_levels", "age_levels", file = file.path(rda_path, "dashboard-age-levels.rda"))
