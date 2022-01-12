library(data.table)
library(tidyverse)
library(lubridate)
library(scales)

## if on the server get the latest data
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/vacunaspr/rdas"
} else{
  rda_path <- "rdas"
}

manu_levels <- c("UNV", "MOD", "PFR", "JSN")
first_ped_day <- make_date(2021, 11, 04)
first_booster_day <- make_date(2021, 8, 13) 
first_jnj_booster_day <- make_date(2021, 10, 22) 

load(file.path(rda_path, "dat_vax.rda"))
load(file.path(rda_path, "population-tabs.rda"))

muni_levels <- c(levels(pop_by_age_gender_municipio$municipio), "No reportado")

load(file.path(rda_path, "dates.rda"))

administradas <- sum(!is.na(dat_vax$date_1)) +
  sum(!is.na(dat_vax$date_2)) +
  sum(!is.na(dat_vax$date_3))

administradas_tasa <- 
  (filter(dat_vax, date_1 %in% span_range) %>% nrow +
  filter(dat_vax, date_2 %in% span_range) %>% nrow +
  filter(dat_vax, date_3 %in% span_range) %>% nrow )/length(span_range) * 7

dat_vax <- dat_vax[!manu_1 %in% c("ATZ","OTR") & 
                     !manu_2 %in% c("ATZ","OTR") &
                     !manu_3 %in% c("ATZ","OTR"), ] 
dat_vax$manu_1 <- droplevels(dat_vax$manu_1)
dat_vax$manu_2 <- droplevels(dat_vax$manu_2)
dat_vax$manu_3 <- droplevels(dat_vax$manu_3)


dat_vax[estado %in% c("PR", "No reportado") , 
        c("vax_date", "booster_date", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange") := 
          .(date_2, date_3, manu_3, insert_date_3, proveedor_3, ageRange_3)]
dat_vax[manu_1 == "JSN", 
        c("vax_date", "booster_date", "ageRange_2", "manu_2", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange") := 
          .(date_1, date_2, ageRange_1, manu_1, manu_2, date_2, proveedor_2, ageRange_2)]
dat_vax[!is.na(vax_date), vax_date := vax_date + days(14)]
dat_vax[!is.na(booster_date), booster_date := booster_date + days(14)]
dat_vax[vax_date > today(), vax_date := NA] ## not fully vax yet

## to early to be a booster, so we assume booster date is error
dat_vax[!is.na(booster_date) & manu_1 != "JSN" & (booster_date - days(14) < first_booster_day | booster_date - days(14) - date_2 < days(30)), 
        c("booster_date", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange") := .(NA, NA, NA, NA, NA)]
dat_vax[!is.na(booster_date) & manu_1 == "JSN" & (booster_date - days(14) < first_jnj_booster_day | booster_date - days(14) - date_1 < days(30)), 
        c("booster_date", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange") := .(NA, NA, NA, NA, NA)]
# not vaxed, can't be boosted
dat_vax[!is.na(booster_date) & is.na(vax_date), 
        c("booster_date", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange") := .(NA, NA, NA, NA, NA)]


## last data without needing a booster
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 != "JSN", last_immune_date := date_2 + days(180)]
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 == "JSN", last_immune_date := date_1 + days(60)]


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
                                            keyby = .(vax_date, ageRange_2, gender, manu_1)]
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
                                            keyby = .(last_immune_date, ageRange_2, gender, manu_1)]
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
                                            keyby = .(vax_date, ageRange_2, gender, manu_1, municipio)]
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
                                                keyby = .(booster_date, ageRange_3, gender, manu_3, municipio)]
names(daily_counts_booster_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "booster")
daily_counts_booster_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_booster_age_gender_manu_muni, all.x=TRUE)
daily_counts_booster_age_gender_manu_muni[is.na(daily_counts_booster_age_gender_manu_muni)] <- 0 
counts_booster_age_gender_manu_muni <- copy(daily_counts_booster_age_gender_manu_muni)
counts_booster_age_gender_manu_muni[, booster := cumsum(booster), keyby = .(ageRange, gender, manu, municipio)]
setnames(counts_booster_age_gender_manu_muni, "booster", "n")

## Lost immunnity
daily_counts_lost_age_gender_manu_muni <- dat_vax[!is.na(last_immune_date), .(lost = .N),
                                             keyby = .(last_immune_date, ageRange_2, gender, manu_1, municipio)]
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

# Other -------------------------------------------------------------------

unvax <- counts_onedose_age_gender_manu[ , .(total = sum(n)), 
                                         keyby = .(date, ageRange, gender)]
unvax <- merge(unvax, pop_by_age_gender, all.x = TRUE, 
               by = c("ageRange", "gender")) 
unvax[,n := poblacion - total]
unvax[,manu := "UNV"]
unvax <- unvax[,c("date", "ageRange","gender","manu", "n")]

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

### PIRAMIDE
tab <- poblacion %>% 
  filter(date == max(date) & gender %in% c("M","F")) %>%
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
  filter(gender %in% c("M", "F")) %>%
  group_by(ageRange, gender) %>%
  summarize(onedose = sum(onedose), full = sum(full),
            booster=sum(booster), lost = sum(lost), .groups = "drop") %>%
  mutate(immune = full - lost) %>%
  left_join(pop_by_age_gender, by = c("ageRange", "gender")) %>%
  mutate(municipio = "Todos")

piramide_tab_muni <- daily_vax_counts_by_municipio %>% 
  filter(gender %in% c("M", "F") & municipio!="No reportado") %>%
  group_by(municipio, ageRange, gender) %>%
  summarize(onedose = sum(onedose), full = sum(full),
            booster=sum(booster), lost = sum(lost), .groups = "drop") %>%
  mutate(immune = full - lost) %>%
  left_join(pop_by_age_gender_municipio, by = c("municipio","ageRange", "gender"))

piramide_tab <- bind_rows(piramide_tab, piramide_tab_muni)
  
rm(daily_counts_onedose_age_gender_manu, 
   daily_counts_booster_age_gender_manu,
   daily_counts_lost_age_gender_manu,
   daily_counts_vax_age_gender_manu,
   counts_onedose_age_gender_manu, 
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
   tab, tab_muni)

## cases
load(file.path(rda_path, "dat_cases_vax.rda"))

dat_cases <- dat_cases_vax[!manu_1 %in% c("ATZ","OTR") & 
                             !manu_2 %in% c("ATZ","OTR") &
                             !manu_3 %in% c("ATZ","OTR") &
                             (is.na(estado) | estado %in% c("PR", "No reportado")),]
dat_cases <- dat_cases[(is.na(proveedor_1) | proveedor_1 != "Correccional") & 
                         (is.na(proveedor_2) | proveedor_2 != "Correccional"),]
dat_cases$manu_1 <- factor(dat_cases$manu_1, levels = manu_levels)
dat_cases$manu_2 <- factor(dat_cases$manu_2, levels = manu_levels)
dat_cases$manu_3 <- factor(dat_cases$manu_3, levels = manu_levels)

dat_cases[manu_1 != "JSN", c("vax_date", "booster_date", "booster_manu") := .(date_2, date_3, manu_3)]
dat_cases[manu_1 == "JSN", 
          c("vax_date", "booster_date", "manu_2", "booster_manu") := .(date_1, date_2, manu_1, manu_2)] 
dat_cases$manu <- factor(replace_na(as.character(dat_cases$manu_1), "UNV"), levels = manu_levels)

# Ajustar para incluir 14 dias para dosis tomar efecto
dat_cases[!is.na(vax_date), vax_date := vax_date + days(14)]
dat_cases[!is.na(booster_date), booster_date := booster_date + days(14)]

dat_cases[!is.na(booster_date) & manu != "JSN" & (booster_date - days(14) < first_booster_day | booster_date - days(14) - date_1 < days(30)), 
          c("booster_date", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange") := .(NA, NA, NA, NA, NA)]
dat_cases[!is.na(booster_date) & manu == "JSN" & (booster_date - days(14) < first_jnj_booster_day | booster_date - days(14) - date_1 < days(30)), 
          c("booster_date", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange") := .(NA, NA, NA, NA, NA)]
# not vaxed, can't be boosted
dat_cases[!is.na(booster_date) & is.na(vax_date), 
          c("booster_date", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange") := .(NA, NA, NA, NA, NA)]
dat_cases[, status := "UNV"]
dat_cases[date > date_1, status := "PAR"]
dat_cases[date > vax_date, status := "VAX"]
dat_cases[date > booster_date, status := "BST"]
dat_cases[date <= date_1, manu := "UNV"]
dat_cases$status <- factor(dat_cases$status, levels = c("UNV", "PAR", "VAX", "BST"))


all_combs <- CJ(date = all_dates$date, 
                ageRange = levels(dat_cases$ageRange),
                gender = levels(dat_cases$gender),
                status = levels(dat_cases$status),
                manu = manu_levels)
all_combs <- all_combs[!(manu == "UNV" & status !="UNV") & 
                         !(manu != "UNV" & status =="UNV"),]

counts_cases <- dat_cases[,.(cases = .N), 
                              keyby = .(date, ageRange, gender, manu, status)]
counts_hosp <- dat_cases[hosp==TRUE, .(hosp = .N), 
                            keyby = .(date_hosp, ageRange, gender, manu, status)]
setnames(counts_hosp, "date_hosp", "date")
counts_death <- dat_cases[death==TRUE,.(death = .N), 
                              keyby = .(date_death, ageRange, gender, manu, status)]
setnames(counts_death, "date_death", "date")

counts <- merge(merge(counts_cases, counts_hosp, all = TRUE), counts_death, all = TRUE)
counts <- merge(all_combs, counts, all.x = TRUE)
counts[is.na(counts)] <- 0
counts <- merge(counts, poblacion, all.x = TRUE)
counts[is.na(counts)] <- 0
counts$manu <- factor(counts$manu, levels = manu_levels)
counts$status <- factor(counts$status,  levels = c("UNV", "PAR", "VAX", "BST"))
counts$ageRange <- factor(counts$ageRange, levels = age_levels)


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
                                                   
ped_pop <- pop_by_age_gender %>% filter(ageRange=="5-11") %>% pull(poblacion) %>% sum()
pediatric_primera_prop <- pediatric_primera / ped_pop

pediatric_completa <- daily_vax_counts %>% filter(ageRange=="5-11" & date >= first_ped_day) %>%
  pull(full) %>% sum(na.rm=TRUE)

pediatric_completa_prop <- pediatric_completa/ ped_pop

lost <-  sum(daily_vax_counts$lost)
lost_prop <-lost/pr_pop

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

summary_tab <- data.frame(names = c("Vacunas administradas",
                            "Personas con por lo menos 1 dosis",
                            "Personas con dosis completa",
                            "Personas con dosis completa sin necesidad de booster",
                            "Personas con boosters",
                            "Personas con dosis completa con necesidad de booster",
                            "Menores (5-11 aÃ±os) con por lo menos 1 dosis",
                            "Menores (5-11 aÃ±os) con dosis completa"),
                  total = c(administradas, primera, completa,  the_immune, booster, lost,  pediatric_primera, pediatric_completa),
                  porciento = c(NA, primera_prop, completa_prop,  the_immune_prop, booster_prop, lost_prop,  pediatric_primera_prop, pediatric_completa_prop),
                  tasas = c(administradas_tasa, tasas$onedose, tasas$full, tasas$immune, tasas$booster, tasas$lost, tasas_ped$onedose, tasas_ped$full))

outcome_tab_details <- counts %>% 
  filter(date>last_day - weeks(3) & date<=last_day - weeks(1)) %>%
  mutate(complete_week = date <= last_day - weeks(2)) %>%
  group_by(complete_week, status, manu) %>%
  summarize(n = sum(n, na.rm = TRUE)/7,
            cases = sum(cases), hosp=sum(hosp), 
            death = sum(death),
            .groups = "drop") %>%
  mutate(rate_cases = cases/7/n, rate_hosp = hosp/7/n, rate_death = death/7/n) 

outcome_tab <- counts %>% 
  filter(status != "PAR" & date > last_day - weeks(3) & date<=last_day - weeks(1)) %>%
  mutate(complete_week = date <= last_day - weeks(2)) %>%
  group_by(complete_week, status) %>%
  summarize(n = sum(n, na.rm = TRUE)/7,
            cases = sum(cases), hosp=sum(hosp), 
            death = sum(death),
            .groups = "drop") %>%
  mutate(rate_cases = cases/7/n, rate_hosp = hosp/7/n, rate_death = death/7/n) 

# totals <- poblacion %>% 
#   filter(date == last_day) %>%
#   group_by(manu, status) %>%
#   summarize(total = sum(n, na.rm=TRUE), .groups = "drop")  

#outcome_tab <- left_join(outcome_tab, totals, by = c("manu", "status"))

## Proveedores

the_colnames <- c("date", "manu", "insert_date", "proveedor", "ageRange")
proveedores <- bind_rows(
  mutate(dose = "Primera", setNames(select(dat_vax, contains("_1")), the_colnames)),
  mutate(dose = "Segunda", setNames(select(dat_vax, contains("_2")), the_colnames)),
  mutate(dose = "Booster", setNames(select(dat_vax, contains("booster")), the_colnames))) %>%
  mutate(diff = as.numeric(insert_date) - as.numeric(date)) %>%
  filter(!is.na(proveedor) & !(dose=="Segunada" & manu == "JSN")) %>%
  group_by(proveedor, dose, manu, ageRange) %>%
  summarize(total = n(), 
            rezago = mean(diff),
            entradas_esta_semana = sum(insert_date >= today() - weeks(1)),
            rezago_esta_semana = mean(diff[insert_date >= today() - weeks(1)]),
            .groups = "drop")

## Seguimiento

manu_info <- data.table(
  manu=c("PFR", "MOD", "JSN"),
  days_from_1_to_2=c(21, 28, 0),
  days_from_2_to_boost=c(183, 183, 61)
)
rownames(manu_info) <- manu_info$manu
manu_info


dat_seguimiento <- dat_vax[!is.na(date_1)]
dat_seguimiento[, vax_date_sched := date_1 + manu_info$days_from_1_to_2[match(manu_1, manu_info$manu)] + days(14)]
dat_seguimiento[, booster_date_sched := vax_date + manu_info$days_from_2_to_boost[match(manu_1, manu_info$manu)]]

dat_seguimiento[, patient_row := 1:nrow(dat_seguimiento)]

# Print example
dat_seguimiento[, .(ageRange_2, date_1, vax_date_sched, vax_date, booster_date_sched, booster_date)]
dat_seguimiento[manu_1 == "JSN", .(manu_1, date_1, date_2, vax_date, booster_date)]

dat_seguimiento[(booster_date - booster_date_sched) > 200,
                .(ageRange_2, manu_1, date_1, vax_date_sched, vax_date, booster_date_sched, booster_date)] %>%
    .[order(booster_date_sched - booster_date),] 


dat_administradas <-
dat_seguimiento[, .(manu_1, date_1, vax_date, booster_date)] %>%
  melt(id=1, variable.name="date_var", value.name="date") %>%
  .[order(date,date_var,manu_1), .(total=.N), .(date, date_var, manu_1)] %>%
  .[!is.na(date),] %>% # No NA dates
  .[!((date_var=="vax_date")&(manu_1=="JSN"))] # Redundant incluir vax_date para JSN (ya estan en date_1)
dat_administradas[(date_var != "date_1"), date := date - days(14)]
administradas_by_date <- dat_administradas[, .(administradas=sum(total)), .(date)]


seguimiento_intervalos <- data.table(
  name=as.factor(c('1A-VS', 'VS-VA', 'VA-BS', 'BS-BA')),
  start_var=c('date_1', 'vax_date_sched', 'vax_date', 'booster_date_sched'),
  end_var  =c('vax_date_sched', 'vax_date', 'booster_date_sched', 'booster_date')
)

dat_seguimiento_melted <-
melt(dat_seguimiento, measure=list(
  seguimiento_intervalos$start_var,
  seguimiento_intervalos$end_var
), variable.name="interval", value.name=c('interval_start', 'interval_end')
) %>%
.[order(patient_row, interval)] %>%
  .[, interval := seguimiento_intervalos$name[interval]] %>%
  .[!(is.na(interval_start) & is.na(interval_end)),] %>%
.[, interval_length := interval_end - interval_start]

dat_seguimiento_onedose <-
dat_seguimiento_melted[interval == "1A-VS",] %>%
  count(interval_start) %>%
  rename(date=interval_start, onedose = n)

dat_seguimiento_complete <-
  dat_seguimiento_melted[interval == "VA-BS",] %>%
  count(interval_start) %>%
  rename(date=interval_start, complete = n)

dat_seguimiento_booster <-
  dat_seguimiento_melted[interval == "BS-BA",] %>%
  count(interval_end) %>%
  rename(date = interval_end, booster = n) %>%
  .[!is.na(date),]

dat_seguimiento_complete_expfill <-
dat_seguimiento_melted[(interval == "VS-VA"), ] %>%
  copy() %>%
  .[, anticipated_end := interval_end] %>%
  .[is.na(anticipated_end), anticipated_end := interval_start] %>%
  count(anticipated_end) %>%
  rename(date = anticipated_end, complete_exp = n)

dat_seguimiento_booster_expfill <-
  dat_seguimiento_melted[(interval == "BS-BA"), ] %>%
  copy() %>%
  .[, anticipated_end := interval_end] %>%
  .[is.na(anticipated_end), anticipated_end := interval_start] %>%
  count(anticipated_end) %>%
  rename(date = anticipated_end, booster_exp = n)

cases_by_date <- counts[,.(cases=sum(cases)), .(date)]

dat_seguimiento_bydate <-
  merge(dat_seguimiento_onedose, dat_seguimiento_complete, all=T, on='date') %>%
  merge(., dat_seguimiento_booster, all=T, on='date') %>%
  #dat_seguimiento_onedose[dat_seguimiento_twodose, on='date'] %>%
  merge(dat_seguimiento_complete_expfill, all=T, on='date') %>%
  merge(dat_seguimiento_booster_expfill, all=T, on='date') %>%
  #.[dat_seguimiento_twodose_expfill, on='date'] %>%
  mutate_if(is.numeric, list(~ coalesce(., 0))) %>%
  mutate(complete_deficit = complete_exp - complete,
         booster_deficit = booster_exp - booster,
         ontime = onedose - complete_deficit - booster_deficit) %>%
  mutate(onedose_cumu = cumsum(onedose),
         complete_cumu = cumsum(complete),
         complete_exp_cumu = cumsum(complete_exp),
         complete_deficit_cumu = cumsum(complete_deficit),
         booster_cumu = cumsum(booster),
         booster_exp_cumu = cumsum(booster_exp),
         booster_deficit_cumu = cumsum(booster_deficit),
         ontime_cumu = cumsum(ontime)) %>%
  mutate_if(is.numeric, list(~ ./pr_pop)) %>%
  left_join(cases_by_date, by="date") %>%
  left_join(administradas_by_date, by="date") %>%
  mutate_at(vars(ends_with('_cumu') | contains("cases") | contains("administradas")), list(ma7 = ~as.numeric(stats::filter(., rep(1/7, 7), side = 1)))) %>%
  filter(date <= last_day)

dat_seguimiento_plotting <- dat_seguimiento_bydate %>%
  melt(id.vars = c("date")) %>%
  mutate(variable = as.factor(variable))

dat_seguimiento_plotting %>%
  filter(variable %in% c('onedose_cumu','complete_cumu','complete_exp_cumu', 'booster_cumu', 'booster_exp_cumu')) %>%
  mutate(variable = factor(variable, levels=c('onedose_cumu','complete_exp_cumu','complete_cumu', 'booster_exp_cumu', 'booster_cumu'))) %>%
ggplot(aes(x = date, y = value, colour = variable)) + 
  theme_bw() +
  geom_ribbon(data=dat_seguimiento_bydate, 
              aes(x=date, ymin=complete_cumu,ymax=complete_exp_cumu), fill="darksalmon", alpha=0.7,
              inherit.aes = F) +
  geom_ribbon(data=dat_seguimiento_bydate,
              aes(x=date, ymin=booster_cumu,ymax=booster_exp_cumu), fill="darkred", alpha=0.7,
              inherit.aes = F) +
  geom_line(size=0.5) +
  # geom_line(aes(y = onedose_cumu, colour = "Una")) + 
  # geom_line(aes(y = twodose_exp_cumu, colour = "Completa (anticipado)")) +
  # geom_line(aes(y = twodose_cumu, colour = "Completa (en realidad)")) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) +
  scale_color_manual(name = "Dosis", 
                       values = c("onedose_cumu" = "black",
                                  "complete_exp_cumu" = "darksalmon",
                                  "complete_cumu" = "black",
                                  "booster_cumu" = "black",
                                  "booster_exp_cumu" = "darkred"
                                  ),
                       labels = c("onedose_cumu" = "Solo una",
                                  "complete_exp_cumu" = "Completa (anticipada)",
                                  "complete_cumu" = "Completa (en realidad)",
                                  "booster_cumu" = "Booster (en realidad)",
                                  "booster_exp_cumu" = "Booster (anticipada)"
                                  )
                     ) +
  theme(legend.title = element_text(size=8),
        legend.text = element_text(size=5),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0)) +
  xlab("Fecha") +
  ylab("Población") +
  ggtitle("Seguimiento de vacunación en Puerto Rico")
  
ggsave('seguimiento.png',dpi=300)

sec_axis_scale <- 2000

dat_seguimiento_bydate %>%
ggplot(aes(x = date)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_bar(aes(y = cases_ma7 / sec_axis_scale), colour="gray", stat="identity") +
  geom_ribbon(aes(ymin=ontime_cumu_ma7,
                  ymax=ontime_cumu_ma7 + booster_deficit_cumu_ma7,
                  fill='booster'),  alpha=0.4,
              inherit.aes = T) +
  geom_ribbon(aes(ymin=ontime_cumu_ma7+booster_deficit_cumu_ma7,
                  ymax=ontime_cumu_ma7 + booster_deficit_cumu_ma7+complete_deficit_cumu_ma7,
                  fill='complete'), alpha=0.4,
              inherit.aes = T) +
  geom_line(aes(y = ontime_cumu_ma7 + booster_deficit_cumu_ma7 +complete_deficit_cumu_ma7,
                linetype="Idealmente, todos"),
                                  size=1.0, colour="darkblue", alpha=0.4) +
  geom_line(aes(y = ontime_cumu_ma7, linetype="En realidad, pocos"),
                                  size=1.5, colour="darkblue") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits=c(0,1),
                     oob = rescale_none,
                     sec.axis = sec_axis(~ . *sec_axis_scale, name = "Promedio casos nuevos")
                     ) +
  scale_linetype(guide = guide_legend(reverse = T, order=1)) +
  scale_fill_manual(name='Aquellos aun sin recibir:',labels=c('Booster', 'Segunda dosis'), values=c('darkred', 'darksalmon'),
                    guide = guide_legend(reverse = TRUE, order=2)) +
  
  labs(linetype="¿Cuantos?") +
  xlab("Fecha") +
  ylab("Población") +
  ggtitle("¿Cuanta gente tiene sus vacunas COVID-19 al día?") 

ggsave('vacunados_up-to-date.png',dpi=300)

# Interval lengths between 1st and vaccinated status

days_betw_vs_va <-
dat_seguimiento_melted[(interval == "VS-VA") & 
                       !is.na(interval_length) & 
                        (manu_1 != "JSN"),] %>%
  .[(-10 < interval_length) & (interval_length < 20)] %>%
  .[, interval_length] %>% 
  as.numeric()
hist(days_betw_vs_va, prob=T,
     main="Retraso recibiendo segunda dosis",
     xlab="Dias desde fecha pronosticada", ylab= "PDF", )
#lines(density(days_betw_vs_va))
#View(dat_vax[c(2733996)])


days_betw_bs_ba <-
  dat_seguimiento_melted[(interval == "BS-BA") & 
                           !is.na(interval_length)
                           ,] %>%
  #.[(-10 < interval_length) & (interval_length < 40)] %>%
  #.[, interval_length] %>% 
  .[,interval_length := as.numeric(interval_length)]
hist(days_betw_bs_ba[,interval_length], prob=T,
     main="Retraso recibiendo dosis de refuerzo",
     xlab="Dias desde fecha pronosticada", ylab= "PDF", )
lines(density(days_betw_bs_ba[,interval_length]))

days_betw_bs_ba[,.(Mean_booster_delay=mean(interval_length), Sd=sd(interval_length)),.(manu_1 %in% c("PFR","MOD"))]

# Casos vs vac admin

vacunas_rescale <- 50

dat_seguimiento_bydate %>%
  ggplot(aes(x = date)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_bar(aes(y = cases_ma7), colour=alpha("black", 1), stat="identity") +
  geom_bar(aes(y = administradas_ma7 / vacunas_rescale), colour=alpha("blue", 0.2), stat="identity") +
  
  
  scale_y_continuous(
                     #labels = scales::percent_format(accuracy = 1),
                     limits=c(0,1500),
                     oob = rescale_none,
                     sec.axis = sec_axis(~ . *vacunas_rescale, name = "Vacunas administradas")
  ) +
  #scale_linetype(guide = guide_legend(reverse = T, order=1)) +
  # scale_fill_manual(name='Aquellos aun sin recibir:',labels=c('Booster', 'Segunda dosis'), values=c('darkred', 'darksalmon'),
                    # guide = guide_legend(reverse = TRUE, order=2)) +
  
  # labs(linetype="¿Cuantos?") +
  xlab("Fecha de muestra o administración") +
  ylab("Casos") +
  ggtitle("Casos y vacunas")



save(proveedores, file=file.path(rda_path ,"proveedores.rda"))
save(counts, file=file.path(rda_path ,"counts.rda"))
save(summary_tab, outcome_tab, outcome_tab_details, file=file.path(rda_path ,"tabs.rda"))
save(poblacion, poblacion_muni, file = file.path(rda_path ,"poblacion.rda"))
save(daily_vax_counts, file = file.path(rda_path, "daily_vax_counts.rda"))
save(daily_vax_counts_by_municipio, file = file.path(rda_path, "daily_vax_counts_by_municipio.rda"))
save(dat_cases, file =  file.path(rda_path ,"dat_cases.rda"))
save(immune, file = file.path(rda_path ,"immune.rda"))
save(piramide, piramide_tab, file = file.path(rda_path, "piramide.rda"))

