library(data.table)
library(tidyverse)
library(lubridate)
library(scales)

collapse_age <- function(tab, vars){
  tab <- copy(tab)
  cols <- setdiff(names(tab), vars)
  tab[, ageRange := forcats::fct_collapse(ageRange, 
                                          "18-29" = c("18-24", "25-29"),
                                          "30-39" = c("30-34", "35-39"),
                                          "40-59" = c("40-44", "45-49"),
                                          "50-59" = c("50-54", "55-59"),
                                          "60-69" = c("60-64", "65-69"),
                                          "70-79" = c("70-74", "75-79"),
                                          "80+" = c("80-84", "85+"))]
  ret <- tab[, lapply(.SD, sum), keyby = cols, .SDcols=vars]
  return(ret)
}

## if on the server get the latest data
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/vacunaspr/rdas"
} else{
  rda_path <- "rdas"
}


# Define global variables -------------------------------------------------

manu_levels <- c("UNV", "MOD", "PFR", "JSN")
first_ped_day <- make_date(2021, 11, 04)
first_booster_day <- make_date(2021, 8, 13) 
first_jnj_booster_day <- make_date(2021, 10, 22) 

# Load data ---------------------------------------------------------------

dates_rda_variables <- load(file.path(rda_path, "dates.rda"))

load(file.path(rda_path, "dat_vax.rda"))
## define last_immunne_date
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 != "JSN", last_immune_date := date_2 + days(150)]
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 == "JSN", last_immune_date := date_1 + days(60)]

load(file.path(rda_path, "dat_cases_vax.rda"))
dat_cases_vax[is.na(date) & !is.na(date_death), date := date_death]

## load population

load(file.path(rda_path, "population-tabs.rda"))
muni_levels <- c(levels(pop_by_age_gender_municipio$municipio), "No reportado")

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

## Fully vaxed
daily_counts_vax_age_gender_manu <- dat_vax[!is.na(vax_date), .(full = .N),
                                            keyby = .(vax_date, vax_ageRange, gender, manu_1)]
names(daily_counts_vax_age_gender_manu) <- c("date", "ageRange", "gender", "manu", "full")
daily_counts_vax_age_gender_manu <- merge(all_combs, daily_counts_vax_age_gender_manu, all.x=TRUE)
daily_counts_vax_age_gender_manu[is.na(daily_counts_vax_age_gender_manu)] <- 0 
counts_vax_age_gender_manu <- copy(daily_counts_vax_age_gender_manu)
counts_vax_age_gender_manu[, full := cumsum(full), keyby = .(ageRange, gender, manu)]
setnames(counts_vax_age_gender_manu, "full", "n")

## One dose
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
counts_partial_age_gender_manu<- merge(counts_vax_age_gender_manu, 
                                       counts_onedose_age_gender_manu, 
                                       all = TRUE, by = c("date", "ageRange", "gender", "manu"))
counts_partial_age_gender_manu[ , n := n.y - n.x]
counts_partial_age_gender_manu <- counts_partial_age_gender_manu[, !c("n.y", "n.x")]

## Boosters
daily_counts_booster_age_gender_manu <- dat_vax[!is.na(booster_date), .(booster = .N),
                                                keyby = .(booster_date, booster_ageRange, gender, booster_manu)]
names(daily_counts_booster_age_gender_manu) <- c("date", "ageRange", "gender", "manu", "booster")
daily_counts_booster_age_gender_manu <- merge(all_combs, daily_counts_booster_age_gender_manu, all.x=TRUE)
daily_counts_booster_age_gender_manu[is.na(daily_counts_booster_age_gender_manu)] <- 0 
counts_booster_age_gender_manu <- copy(daily_counts_booster_age_gender_manu)
counts_booster_age_gender_manu[, booster := cumsum(booster), keyby = .(ageRange, gender, manu)]
setnames(counts_booster_age_gender_manu, "booster", "n")

## Lost original immunnity
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

## Fully vaxed
daily_counts_vax_age_gender_manu_muni <- dat_vax[!is.na(vax_date), .(full = .N),
                                            keyby = .(vax_date, vax_ageRange, gender, manu_1, municipio)]
names(daily_counts_vax_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "full")
daily_counts_vax_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_vax_age_gender_manu_muni, all.x=TRUE)
daily_counts_vax_age_gender_manu_muni[is.na(daily_counts_vax_age_gender_manu_muni)] <- 0 
counts_vax_age_gender_manu_muni <- copy(daily_counts_vax_age_gender_manu_muni)
counts_vax_age_gender_manu_muni[, full := cumsum(full), keyby = .(ageRange, gender, manu, municipio)]
setnames(counts_vax_age_gender_manu_muni, "full", "n")

## One dose
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
counts_partial_age_gender_manu_muni<- merge(counts_vax_age_gender_manu_muni,
                                       counts_onedose_age_gender_manu_muni,
                                       all = TRUE, by = c("date", "ageRange", "gender", "manu", "municipio"))
counts_partial_age_gender_manu_muni[ , n := n.y - n.x]
counts_partial_age_gender_manu_muni <- counts_partial_age_gender_manu_muni[, !c("n.y", "n.x")]

## Boosters
daily_counts_booster_age_gender_manu_muni <- dat_vax[!is.na(booster_date), .(booster = .N),
                                                keyby = .(booster_date, booster_ageRange, gender, booster_manu, municipio)]
names(daily_counts_booster_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "booster")
daily_counts_booster_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_booster_age_gender_manu_muni, all.x=TRUE)
daily_counts_booster_age_gender_manu_muni[is.na(daily_counts_booster_age_gender_manu_muni)] <- 0 
counts_booster_age_gender_manu_muni <- copy(daily_counts_booster_age_gender_manu_muni)
counts_booster_age_gender_manu_muni[, booster := cumsum(booster), keyby = .(ageRange, gender, manu, municipio)]
setnames(counts_booster_age_gender_manu_muni, "booster", "n")

## Lost immunnity
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

unvax <- counts_onedose_age_gender_manu[ , .(total = sum(n)), 
                                         keyby = .(date, ageRange, gender)]
unvax <- merge(unvax, pop_by_age_gender, all.x = TRUE, 
               by = c("ageRange", "gender")) 
unvax[,n := poblacion - total]
unvax[,manu := "UNV"]
unvax <- unvax[,c("date", "ageRange", "gender", "manu", "n")]

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

### PIRAMIDE
tab <- poblacion %>% 
  filter(date == max(date) & gender %in% c("M","F")) %>%
  collapse_age(vars="n") %>%
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
  collapse_age(vars="n") %>%
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
  collapse_age(vars=c("onedose", "full","booster","lost", "poblacion")) %>%
  filter(gender %in% c("M", "F")) %>%
  group_by(ageRange, gender) %>%
  summarize(onedose = sum(onedose), full = sum(full),
            booster=sum(booster), lost = sum(lost), 
            poblacion=poblacion[1], .groups = "drop") %>%
  mutate(immune = full - lost) %>%
  mutate(poblacion=pmax(poblacion,onedose)) %>%
  mutate(municipio = "Todos")

piramide_tab_muni <- daily_vax_counts_by_municipio %>% 
  left_join(pop_by_age_gender_municipio, by = c("municipio", "ageRange", "gender")) %>%
  filter(gender %in% c("M", "F") & municipio!="No reportado") %>%
  collapse_age(vars=c("onedose", "full","booster","lost", "poblacion")) %>%
  group_by(municipio, ageRange, gender) %>%
  summarize(onedose = sum(onedose), full = sum(full),
            booster=sum(booster), lost = sum(lost), 
            poblacion=poblacion[1], .groups = "drop") %>%
  mutate(poblacion=pmax(poblacion,onedose)) %>%
  mutate(immune = full - lost) 
 
piramide_tab <- bind_rows(piramide_tab, piramide_tab_muni)
 
## Proveedores

cols1 <-  grep("_1", names(dat_vax), value=TRUE)
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
proveedores[,ageRange := forcats::fct_collapse(ageRange, 
                                              "18-29" = c("18-24", "25-29"),
                                              "30-39" = c("30-34", "35-39"),
                                              "40-59" = c("40-44", "45-49"),
                                              "50-59" = c("50-54", "55-59"),
                                              "60-69" = c("60-64", "65-69"),
                                              "70-79" = c("70-74", "75-79"),
                                              "80+" = c("80-84", "85+"))]
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
   tab, tab_muni, unvax)


# Cases -------------------------------------------------------------------

## remove correctional cases to avoid bias introduced by outbreak
# dat_vax <- dat_vax[(is.na(proveedor_1) | proveedor_1!="Correccional") &
#                      (is.na(proveedor_2) | proveedor_2!="Correccional"),]
# dat_cases_vax <- dat_cases_vax[(is.na(proveedor_1) | proveedor_1!="Correccional") &
#                                  (is.na(proveedor_2) | proveedor_2!="Correccional"),]


## define status and manufacturers
dat_cases_vax[, manu := factor(fifelse(is.na(manu_1), "UNV", as.character(manu_1)), levels=manu_levels)]
dat_cases_vax[, status := "UNV"]
dat_cases_vax[date > date_1, status := "PAR"]
dat_cases_vax[date > vax_date, status := "VAX"]
dat_cases_vax[date > booster_date, status := "BST"]
dat_cases_vax[date <= date_1, manu := "UNV"]
dat_cases_vax$status <- factor(dat_cases_vax$status, levels = c("UNV", "PAR", "VAX", "BST"))

## Compute number of unvaccinated that were suceuptible
all_combs <- with(dat_cases_vax[!is.na(ageRange)],
                  CJ(date = seq(min(all_dates$date)-2-days(90), max(all_dates$date), by="day"),
                     ageRange = unique(ageRange),
                     gender = unique(gender)))
moving_sum <- function(y, k = 90) as.numeric(stats::filter(y, c(0, rep(1, k)), side = 1))
recently_infected <- dat_cases_vax[!is.na(ageRange) & status == "UNV", .(cases = .N), keyby = .(ageRange, gender, date)][order(date)]
recently_infected <- merge(all_combs, recently_infected, by = c("date", "ageRange", "gender"), all.x=TRUE)
recently_infected[is.na(cases), cases := 0]
recently_infected[, cases := moving_sum(cases), keyby = .(ageRange, gender)]
#recently_infected[date>=first_day & gender %in% c("F","M")] %>% ggplot(aes(date, cases, color = gender))+geom_line() + facet_wrap(~ageRange)
pop_susceptible <- merge(recently_infected[date>=first_day & gender %in% c("F","M")], 
                         pop_by_age_gender,  by = c("ageRange", "gender"), all.x=TRUE)
pop_susceptible[, poblacion := poblacion-cases]
#pop_susceptible %>% ggplot(aes(date, poblacion, color = gender)) + geom_line() + geom_line(aes(y=poblacion+cases), lty=2) + facet_wrap(~ageRange, scales="free_y")
pop_susceptible[, cases:=NULL]



### Compute number with and without booster for each day after vaccinated
the_ndays <- as.numeric(last_day - min(dat_vax$vax_date, na.rm = TRUE))
the_booster_ndays <- as.numeric(last_day - first_booster_day)

compute_date_comb_counts <- function(tab, ndays = the_ndays, other_date = "booster_date"){
  the_date <- unique(tab$date)
  all_dates <- data.table(tmp = seq(the_date, min(the_date + the_ndays, last_day), "days"))
  setnames(all_dates, "tmp", other_date)
  ind <- !is.na(tab[[other_date]])
  ret <- tab[ind, .(poblacion = .N), keyby = other_date]
  ret <- merge(all_dates, ret, by = other_date, all.x = TRUE)
  ret[is.na(ret)] <- 0
  ret <- ret[, poblacion := cumsum(poblacion)]
  ret[, poblacion := nrow(tab) - poblacion]
  setnames(ret, other_date, "date")
  return(ret)
}

## population of vaccinated without booster for each day and date of vaccination
dat_vax[, date := vax_date] ##make date vax_date for compute_date_comb_counts
pop_vax <- dat_vax[!is.na(vax_date) & vax_date <= last_day &
                     gender %in% c("F", "M") & ageRange_1 != "0-4",
                   compute_date_comb_counts(tab = .SD), 
                   keyby = c("vax_date", "manu_1", "vax_ageRange", "gender")]

setnames(pop_vax, c("vax_date", "manu", "ageRange", "gender", "date", "poblacion"))
setcolorder(pop_vax, c("date", "vax_date", "manu", "ageRange", "gender", "poblacion"))
pop_vax$manu <- factor(pop_vax$manu, levels = manu_levels)
pop_vax$ageRange <- factor(pop_vax$ageRange, levels = age_levels)
pop_vax <- pop_vax[order(manu, ageRange, gender, date, vax_date)]
#pop_vax %>% group_by(date,ageRange, manu) %>% summarize(n=sum(poblacion)) %>% ggplot(aes(date,n,color=manu))+geom_line()+facet_wrap(~ageRange)

dat_vax[, date := date_1] ##make date vax_date for compute_date_comb_counts

## population of partially vaccinated
pop_par <- dat_vax[!is.na(date_1) & date_1 <= last_day &
                     gender %in% c("F", "M") & ageRange_1 != "0-4",
                   compute_date_comb_counts(tab = .SD, other_date = "vax_date"), 
                   keyby = c("date_1", "manu_1", "ageRange_1", "gender")]
setnames(pop_par, c("vax_date", "manu", "ageRange", "gender", "date", "poblacion"))
setcolorder(pop_par, c("date", "vax_date", "manu", "ageRange", "gender", "poblacion"))
pop_par$manu <- factor(pop_par$manu, levels = manu_levels)
pop_par$ageRange <- factor(pop_par$ageRange, levels = age_levels)
pop_par <- pop_par[order(manu, ageRange, gender, date, vax_date)]
#pop_par %>% group_by(date,ageRange, manu) %>% summarize(n=sum(poblacion)) %>% ggplot(aes(date,n,color=manu))+geom_line()+facet_wrap(~ageRange)

## remove temporary column
dat_vax[,date :=NULL]

# Unvaccinated per day ----------------------------------------------------

#start by counting individuals with one dose again becasue we removed correctional data

all_dates <- data.table(date = seq(first_day, last_day, "days"))
all_combs <- CJ(date = all_dates$date, 
                ageRange_1 = sort(unique(dat_vax$ageRange_1)),
                gender = sort(unique(dat_vax$gender)),
                manu = sort(unique(dat_vax$manu_1)))
## First one dose
daily_counts_onedose_age_gender_manu <- dat_vax[!is.na(date_1), .(onedose = .N),
                                                keyby = .(date_1, ageRange_1, gender, manu_1)]
names(daily_counts_onedose_age_gender_manu) <- c("date", "ageRange_1", "gender", "manu", "onedose")
daily_counts_onedose_age_gender_manu <- merge(all_combs, 
                                              daily_counts_onedose_age_gender_manu, 
                                              all.x=TRUE)
daily_counts_onedose_age_gender_manu[is.na(daily_counts_onedose_age_gender_manu)] <- 0 
counts_onedose_age_gender_manu <- copy(daily_counts_onedose_age_gender_manu)
counts_onedose_age_gender_manu[, onedose := cumsum(onedose), keyby = .(ageRange_1, gender, manu)]
setnames(counts_onedose_age_gender_manu, "onedose", "n")

pop_unvax <- counts_onedose_age_gender_manu[gender %in% c("F", "M") & 
                                              date <= last_day,
                                            .(total = sum(n)), 
                                            keyby = .(date, ageRange_1, gender)]
setnames(pop_unvax, "ageRange_1", "ageRange")
pop_unvax <- merge(pop_unvax, pop_susceptible, all.x = TRUE, 
                   by = c("date", "ageRange", "gender")) 
pop_unvax[, poblacion := poblacion - total]
pop_unvax <- pop_unvax[,c("date", "ageRange","gender", "poblacion")]

#pop_unvax %>% ggplot(aes(date,poblacion, color=gender))+geom_line()+facet_wrap(~ageRange)

# Booster counts ----------------------------------------------------------

### Booster counts - a bit different because we dont have to remove boosters... it's monotically increasing
daily_counts_booster_age_gender_manu <- dat_vax[!is.na(booster_date), .(poblacion = .N),
                                                keyby = .(booster_date, booster_ageRange, gender, manu_1, booster_manu)]
names(daily_counts_booster_age_gender_manu) <- c("booster_date", "booster_ageRange", "gender", "manu_1", "booster_manu", "poblacion")

all_dates <-  seq(first_booster_day, last_day, "days")
all_bst_combs <- CJ(date = all_dates,
                    booster_date = all_dates, 
                    booster_ageRange = sort(unique(dat_vax$booster_ageRange))[-1], ##-1 to take out 0-4
                    gender = factor(c("F", "M")),
                    manu_1 = sort(unique(dat_vax$manu_1)),
                    booster_manu = levels(dat_vax$booster_manu))
all_bst_combs <- all_bst_combs[as.numeric(date - booster_date) >=0 & 
                                 as.numeric(date - booster_date) <= the_booster_ndays,]


dat_cases_vax <- dat_cases_vax[date>= first_day & date <= last_day,]


vax <- dat_cases_vax[status == "VAX" & gender %in% c("F", "M") & ageRange != "0-4"]
vax <- vax[, .(cases = .N, hosp = sum(hosp), death = sum(death)), keyby = .(manu, ageRange, gender, date, vax_date)]
vax <- merge(pop_vax, vax, by = c("manu", "ageRange", "gender", "date", "vax_date"), all.x = TRUE) 
vax[is.na(vax)] <- 0
vax <- melt(vax, measure.vars = c("cases", "hosp", "death"),
            variable.name = "outcome", value.name = "obs")
vax[,`:=`(primary_manu=NA, status = "VAX")]

par <- dat_cases_vax[status == "PAR"  & gender %in% c("F", "M") & ageRange != "0-4" & date <= last_day]
setnames(par, "date_1", "vax_date")
par <- par[, .(cases = .N, hosp = sum(hosp), death = sum(death)), keyby = .(manu, ageRange, gender, date, vax_date)]
par <- merge(pop_par, par, by = c("manu", "ageRange", "gender", "date", "vax_date"), all.x = TRUE) 
par[is.na(par)] <- 0
par <- melt(par, measure.vars = c("cases", "hosp", "death"), variable.name = "outcome", value.name = "obs")
par[,`:=`(primary_manu=NA, status = "PAR")]

bst <- dat_cases_vax[status == "BST" & gender %in% c("F", "M") & booster_ageRange != "0-4"]
bst <- bst[, .(cases = .N, hosp = sum(hosp), death = sum(death)), keyby = .(manu_1, booster_manu, booster_ageRange, gender, date, booster_date)]
bst <- merge(all_bst_combs, bst, by = c("date","booster_date", "booster_ageRange", "gender", "manu_1", "booster_manu"), all.x = TRUE) 
bst[is.na(bst)] <- 0
bst <- merge(bst, daily_counts_booster_age_gender_manu,
             by = c("manu_1", "booster_manu", "booster_ageRange", "gender", "booster_date"), 
             all.x = TRUE) 
bst <- bst[!is.na(poblacion),]
setnames(bst, c("manu_1","booster_manu", "booster_date", "booster_ageRange"), 
         c("primary_manu", "manu", "vax_date", "ageRange"))
bst <- melt(bst, measure.vars = c("cases", "hosp", "death"), variable.name = "outcome", value.name = "obs")
bst[, status := "BST"]

unvax <- dat_cases_vax[status == "UNV" & gender %in% c("F", "M")]
unvax <- unvax[, .(cases = .N, hosp = sum(hosp), death = sum(death)), keyby = .(date, ageRange, gender)]
unvax <- merge(pop_unvax, unvax, by = c("date", "ageRange", "gender"), all.x = TRUE) 
unvax[is.na(unvax)] <- 0
unvax <- melt(unvax, measure.vars = c("cases", "hosp", "death"), variable.name = "outcome", value.name = "obs")

unvax[,`:=`(primary_manu=NA, manu="UNV", vax_date=make_date(NA), status = "UNV")]

## store counts for each vax date and case date combination
counts <- rbindlist(list(par, vax, bst, unvax), use.names = TRUE)
counts[, outcome := factor(outcome, levels = c("cases", "hosp","death"))]
counts[, status := factor(status, levels = c("UNV","PAR","VAX","BST"))]
counts[, variant := fcase(date < make_date(2021, 6, 15), "alpha",
                       date >= make_date(2021, 6, 15) & date < make_date(2021,12,8), "delta",
                       date >= make_date(2021, 12, 8), "omicron")]
counts[, day :=fifelse(status!="UNV", as.numeric(date - vax_date), 0)]



# Prepare data for dashboard ----------------------------------------------


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
                                    "Personas con serie primaria completa con necesidad de booster (vacunación expirada)",
                                    "Menores (12-17 años) con booster",
                                    "Menores (12-17 años) con necesidad de booster (vacunación expirada)",
                                    "Menores (5-11 años) con por lo menos 1 dosis",
                                    "Menores (5-11 años) con serie primaria completa"),
                  total = c(administradas, the_immune, primera, completa, booster, lost, booster_ped, lost_ped, pediatric_primera, pediatric_completa),
                  porciento = c(NA, the_immune_prop, primera_prop, completa_prop,  booster_prop,  lost_prop, booster_ped_prop, lost_ped_prop,  pediatric_primera_prop, pediatric_completa_prop),
                  tasas = c(administradas_tasa, tasas$immune, tasas$onedose, tasas$full,  tasas$booster,  tasas$lost, tasas_ped_2$booster, tasas_ped_2$lost, tasas_ped$onedose, tasas_ped$full))


# collapse age groups to minimize denominator variance --------------------

collapsed_age_levels <- c("0-4", "5-11", "12-17", "18-44", "45-64", "65+")
                                
daily_counts <- copy(counts)
daily_counts$ageRange <- fct_collapse(daily_counts$ageRange, 
                                      "18-44" = c("18-24", "25-29", "30-34", "35-39", "40-44"),
                                      "45-64" = c("45-49", "50-54", "55-59","60-64"),
                                      "65+" = c("65-69", "70-74", "75-79","80-84", "85+"))


daily_counts <- daily_counts[!(ageRange == "0-4" |
                                 (ageRange %in% c("5-11", "12-17") & manu%in%c("MOD", "JSN")) |
                                 (ageRange == "5-11" & status != "UNV" & vax_date<first_ped_day) |
                                 (status == "BST" & manu == "JSN"))]

  
  
daily_counts[, status := fcase(
  status == "PAR", "Vacunación parcial",
  status == "UNV", "No vacunados",
  (status == "VAX" & ((manu!="JSN" & day < 150) | (manu=="JSN" & day < 60))) | status=="BST", "Vacunación al día",  
  default = "Vacunación expirada")]
daily_counts$status <- factor(daily_counts$status, levels=c("No vacunados", "Vacunación parcial", "Vacunación expirada", "Vacunación al día"))
 
daily_counts <- daily_counts[, keyby = .(date, ageRange, gender, outcome, status, manu), 
            lapply(.SD, sum), 
            .SDcols = c("obs", "poblacion")]

daily_counts <- daily_counts[order(date)]

#daily_counts %>% ggplot(aes(date, poblacion, color = manu, lty=gender))+geom_line() + facet_grid(status~ageRange)
## Compute cases, hosp and death rates
## Include other genders in totals
tmp <- daily_counts %>%
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


### collapse ageRanges
odaily_vax_counts <- copy(daily_vax_counts)
save(proveedores, file=file.path(rda_path ,"proveedores.rda"))
save(daily_counts, file=file.path(rda_path ,"daily_counts.rda"))
save(counts, file=file.path(rda_path ,"counts.rda"))
save(summary_tab, outcome_tab, outcome_tab_details, file=file.path(rda_path ,"tabs.rda"))
poblacion <- collapse_age(poblacion, vars = "n")
poblacion_muni <- collapse_age(poblacion_muni, vars = "n")
save(poblacion, poblacion_muni, file = file.path(rda_path ,"poblacion.rda"))
daily_vax_counts <- collapse_age(daily_vax_counts, vars = c("onedose", "full", "booster","lost"))
pop_by_age_gender<-collapse_age(pop_by_age_gender, "poblacion")
save(daily_vax_counts, pop_by_age_gender, file = file.path(rda_path, "daily_vax_counts.rda"))
daily_vax_counts_by_municipio <- collapse_age(daily_vax_counts_by_municipio, vars = c("onedose", "full", "booster","lost"))
pop_by_age_gender_municipio <-collapse_age(pop_by_age_gender_municipio, "poblacion")
save(pop_by_age_gender_municipio, daily_vax_counts_by_municipio, file = file.path(rda_path, "daily_vax_counts_by_municipio.rda"))
immune <- collapse_age(immune, vars = "n")
save(immune, file = file.path(rda_path ,"immune.rda"))
save(piramide, piramide_tab, file = file.path(rda_path, "piramide.rda"))
dashboard_age_levels <- levels(poblacion$ageRange)
save(list = c(dates_rda_variables, "collapsed_age_levels", "dashboard_age_levels"), file = file.path(rda_path, "dates.rda"))
