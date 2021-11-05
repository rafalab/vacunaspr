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

load(file.path(rda_path, "map.rda"))
muni_levels <- map_centers$municipio

manu_levels <- c("UNV", "MOD", "PFR", "JSN")
load(file.path(rda_path, "dat_vax.rda"))
load(file.path(rda_path, "population-tabs.rda"))

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
          !manu_2 %in% c("ATZ","OTR"), ] 
dat_vax$manu_1 <- droplevels(dat_vax$manu_1)
dat_vax$manu_2 <- droplevels(dat_vax$manu_2)
dat_vax$manu_3 <- droplevels(dat_vax$manu_3)

dat_vax[estado %in% c("PR", "No reportado") , c("vax_date", "booster_date", "booster_manu") := .(date_2, date_3, manu_3)]
dat_vax[manu_1 == "JSN", 
        c("vax_date", "booster_date", "ageRange_2", "manu_2", "booster_manu") := .(date_1, date_2, ageRange_1, manu_1, manu_2)]
dat_vax[!is.na(vax_date), vax_date := vax_date + days(14)]
dat_vax[vax_date > today(), vax_date := NA] ## not fully vax yet

## to early to be a booster
dat_vax[!is.na(booster_date) & manu_2 != "JSN" & booster_date < make_date(2021, 8, 13), c("booster_date", "booster_manu") := .(NA, NA)]
dat_vax[!is.na(booster_date) & manu_2 == "JSN" & booster_date < make_date(2021, 10, 22), c("booster_date", "booster_manu") := .(NA, NA)]

## last data without needing a booster
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 != "JSN", last_immune_date := date_2 + months(6)]
dat_vax[!is.na(booster_date) & manu_2 != "JSN", last_immune_date := booster_date + months(6)]
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 == "JSN", last_immune_date := date_1 + months(2)]
dat_vax[!is.na(booster_date) & manu_2 == "JSN", last_immune_date := booster_date + months(2)]


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
                                            keyby = .(vax_date, ageRange_2, gender, manu_2)]
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
                                            keyby = .(booster_date, ageRange_3, gender, manu_3)]
names(daily_counts_booster_age_gender_manu) <- c("date", "ageRange", "gender", "manu", "booster")
daily_counts_booster_age_gender_manu <- merge(all_combs, daily_counts_booster_age_gender_manu, all.x=TRUE)
daily_counts_booster_age_gender_manu[is.na(daily_counts_booster_age_gender_manu)] <- 0 

## Lost immunnity
daily_counts_lost_age_gender_manu <- dat_vax[!is.na(last_immune_date), .(lost = .N),
                                            keyby = .(last_immune_date, ageRange_2, gender, manu_2)]
names(daily_counts_lost_age_gender_manu) <- c("date", "ageRange", "gender", "manu", "lost")
daily_counts_lost_age_gender_manu %>% group_by(week=round_date(date,"week")) %>% summarize(need_booster=sum(lost)) %>% ggplot(aes(week, need_booster)) + 
    scale_y_continuous(labels = scales::comma) + geom_line() 
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
                                            keyby = .(vax_date, ageRange_2, gender, manu_2, municipio)]
names(daily_counts_vax_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "full")
daily_counts_vax_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_vax_age_gender_manu_muni, all.x=TRUE)
daily_counts_vax_age_gender_manu_muni[is.na(daily_counts_vax_age_gender_manu_muni)] <- 0 
# counts_vax_age_gender_manu_muni <- copy(daily_counts_vax_age_gender_manu_muni)
# counts_vax_age_gender_manu_muni[, full := cumsum(full), keyby = .(ageRange, gender, manu, municipio)]
# setnames(counts_vax_age_gender_manu_muni, "full", "n")

## One dose
daily_counts_onedose_age_gender_manu_muni <- dat_vax[!is.na(date_1), .(onedose = .N),
                                                keyby = .(date_1, ageRange_1, gender, manu_1, municipio)]
names(daily_counts_onedose_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "onedose")
daily_counts_onedose_age_gender_manu_muni <- merge(all_combs_muni, 
                                              daily_counts_onedose_age_gender_manu_muni, 
                                              all.x=TRUE)
daily_counts_onedose_age_gender_manu_muni[is.na(daily_counts_onedose_age_gender_manu_muni)] <- 0 
# counts_onedose_age_gender_manu_muni <- copy(daily_counts_onedose_age_gender_manu_muni)
# counts_onedose_age_gender_manu_muni[, onedose := cumsum(onedose), keyby = .(ageRange, gender, manu, municipio)]
# setnames(counts_onedose_age_gender_manu_muni, "onedose", "n")

## Partially vaxed
# counts_partial_age_gender_manu_muni<- merge(counts_vax_age_gender_manu_muni, 
#                                        counts_onedose_age_gender_manu_muni, 
#                                        all = TRUE, by = c("date", "ageRange", "gender", "manu", "municipio"))
# counts_partial_age_gender_manu_muni[ , n := n.y - n.x]
# counts_partial_age_gender_manu_muni <- counts_partial_age_gender_manu_muni[, !c("n.y", "n.x")]

## Boosters
daily_counts_booster_age_gender_manu_muni <- dat_vax[!is.na(booster_date), .(booster = .N),
                                                keyby = .(booster_date, ageRange_3, gender, manu_3, municipio)]
names(daily_counts_booster_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "booster")
daily_counts_booster_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_booster_age_gender_manu_muni, all.x=TRUE)
daily_counts_booster_age_gender_manu_muni[is.na(daily_counts_booster_age_gender_manu_muni)] <- 0 

## Lost immunnity
daily_counts_lost_age_gender_manu_muni <- dat_vax[!is.na(last_immune_date), .(lost = .N),
                                             keyby = .(last_immune_date, ageRange_2, gender, manu_2, municipio)]
names(daily_counts_lost_age_gender_manu_muni) <- c("date", "ageRange", "gender", "manu", "municipio", "lost")
daily_counts_lost_age_gender_manu_muni %>% group_by(week=round_date(date,"week")) %>% summarize(need_booster=sum(lost)) %>% ggplot(aes(week, need_booster)) + 
  scale_y_continuous(labels = scales::comma) + geom_line() 
daily_counts_lost_age_gender_manu_muni <- merge(all_combs_muni, daily_counts_lost_age_gender_manu_muni, all.x=TRUE)
daily_counts_lost_age_gender_manu_muni[is.na(daily_counts_lost_age_gender_manu_muni)] <- 0 
# counts_lost_age_gender_manu_muni <- copy(daily_counts_lost_age_gender_manu_muni)
# counts_lost_age_gender_manu_muni[, lost := cumsum(lost), keyby = .(ageRange, gender, manu, municipio)]
# immune <- merge(counts_lost_age_gender_manu, counts_vax_age_gender_manu_muni, 
#                 all = TRUE, by = c("date", "ageRange", "gender", "manu"))
# immune[ , n := n - lost]
# immune <- immune[, !c("lost")]

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


# Other -------------------------------------------------------------------



unvax <- counts_onedose_age_gender_manu[ , .(total = sum(n)), keyby = .(date, ageRange, gender)]
unvax <- merge(unvax, pop_by_age_gender, all.x = TRUE, by = c("ageRange", "gender")) 
unvax[,n := poblacion - total]
unvax[,manu := "UNV"]
unvax <- unvax[,c("date", "ageRange","gender","manu", "n")]

poblacion <- rbind(counts_vax_age_gender_manu[,status:="VAX"],
                   counts_partial_age_gender_manu[,status:="PAR"],
                   unvax[,status:="UNV"])
poblacion$status <- factor(poblacion$status, levels = c("UNV", "PAR", "VAX"))
poblacion$ageRange <- factor(poblacion$ageRange, levels = age_levels)
#poblacion %>%  filter(status == "PAR") %>% ggplot(aes(date, n, color = gender)) + geom_line() + facet_grid(manu~ageRange) 

rm(daily_counts_onedose_age_gender_manu, 
   daily_counts_booster_age_gender_manu,
   daily_counts_lost_age_gender_manu,
   daily_counts_vax_age_gender_manu,
   counts_onedose_age_gender_manu, 
   counts_partial_age_gender_manu, 
   counts_vax_age_gender_manu)



## cases
load(file.path(rda_path, "dat_cases_vax.rda"))
dat_cases <- dat_cases_vax[!manu_1 %in% c("ATZ","OTR") & 
                     !manu_2 %in% c("ATZ","OTR") &
                     !manu_2 %in% c("ATZ","OTR"), ] 
dat_cases$manu_1 <- droplevels(dat_cases$manu_1)
dat_cases$manu_2 <- droplevels(dat_cases$manu_2)
dat_cases$manu_3 <- droplevels(dat_cases$manu_3)

dat_cases[estado %in% c("PR", "No reportado") , c("vax_date", "booster_date", "booster_manu") := .(date_2, date_3, manu_3)]
dat_cases[manu_1 == "JSN", 
        c("vax_date", "booster_date", "manu_2", "booster_manu") := .(date_1, date_2, manu_1, manu_2)] 
dat_cases$manu <- factor(replace_na(as.character(dat_cases$manu_1), "UNV"),
                             levels = manu_levels)
dat_cases[!is.na(vax_date), vax_date := vax_date + days(14)]
dat_cases[, status := "UNV"]
dat_cases[date > date_1, status := "PAR"]
dat_cases[date > vax_date, status := "VAX"]
dat_cases[date <= date_1, manu := "UNV"]
dat_cases$status <- factor(dat_cases$status, levels = c("UNV", "PAR", "VAX"))
dat_cases$booster <- FALSE
dat_cases[!is.na(booster_date) & status == "VAX", booster := date > booster_date]

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
counts$manu <- factor(counts$manu, levels = manu_levels)
counts$status <- factor(counts$status,  levels = c("UNV", "PAR", "VAX"))
counts$ageRange <- factor(counts$ageRange, levels = age_levels)
###################
##counts by booster
##################
counts_cases <- dat_cases[booster==TRUE ,.(cases = .N), 
                          keyby = .(date, ageRange, gender, manu)]
counts_hosp <- dat_cases[booster==TRUE & hosp==TRUE, .(hosp = .N), 
                         keyby = .(date_hosp, ageRange, gender, manu)]
setnames(counts_hosp, "date_hosp", "date")
counts_death <- dat_cases[booster==TRUE & death==TRUE,.(death = .N), 
                           keyby = .(date_death, ageRange, gender, manu)]
setnames(counts_death, "date_death", "date")
all_combs <- CJ(date = all_dates$date, 
                ageRange = levels(dat_cases$ageRange),
                gender = levels(dat_cases$gender),
                manu = manu_levels[-1])
booster_counts <- merge(merge(counts_cases, counts_hosp, all = TRUE), 
                        counts_death, all = TRUE)
booster_counts <- merge(all_combs, booster_counts, all.x = TRUE)
booster_counts[is.na(booster_counts)] <- 0
booster_poblacion <- copy(daily_vax_counts)
booster_poblacion[, n := cumsum(booster), keyby = .(ageRange, gender, manu)]
booster_poblacion <- booster_poblacion[, !c("onedose","full","booster")]
booster_counts <- merge(booster_counts, booster_poblacion, by = c("date", "ageRange", "gender", "manu"))
booster_counts$manu <- factor(booster_counts$manu, levels=manu_levels[-1])

### Summary tab

primera <- sum(!is.na(dat_vax$date_1))
primera_prop <- primera/pr_pop

completa <- sum(!is.na(dat_vax$vax_date))
completa_prop <- completa/pr_pop

the_immune <- immune %>% filter(date == last_day) %>% summarize(n=sum(n)) %>% pull(n)
the_immune_prop <- the_immune/pr_pop

booster <- sum(!is.na(dat_vax$booster_date))
booster_prop <- booster/pr_pop

tasas <- daily_vax_counts %>% 
  filter(date %in% span_range) %>%
  summarize(onedose = sum(onedose)/length(span_range) * 7, 
            full = sum(full)/length(span_range)* 7 , 
            immune = (sum(full) - sum(lost))/length(span_range)*7,
            booster = sum(booster)/length(span_range)  * 7)
             
summary_tab <- data.frame(names = c("Vacunas administradas",
                            "Personas con por lo menos 1 dosis",
                            "Personas con dosis completa",
                            "Personas con dosis completa sin necesidad de booster",
                            "Personas con boosters"),
                  total = c(administradas, primera, completa, the_immune, booster),
                  porciento = c(NA, primera_prop, completa_prop, the_immune_prop, booster_prop),
                  tasas = tasas <- c(administradas_tasa, tasas$onedose, tasas$full, tasas$immune, tasas$booster))


outcome_tab <- counts %>% group_by(status, manu) %>%
  summarize(n = sum(n, na.rm = TRUE)/365,
            cases = sum(cases), hosp=sum(hosp), 
            death = sum(death),
            .groups = "drop") %>%
  mutate(rate_cases = cases/n, rate_hosp = hosp/n, rate_death = death/n) 

outcome_tab_booster <-  booster_counts %>% group_by(manu) %>%
  summarize(n = sum(n, na.rm = TRUE)/365,
            cases = sum(cases), hosp=sum(hosp), 
            death = sum(death),
            .groups = "drop") %>%
  mutate(rate_cases = cases/n, rate_hosp = hosp/n, rate_death = death/n,
         status = "Con booster")

outcome_tab <- bind_rows(outcome_tab, outcome_tab_booster)

totals <- poblacion %>% 
  filter(date == last_day) %>%
  group_by(manu, status) %>%
  summarize(total = sum(n, na.rm=TRUE), .groups = "drop")  

totals_booster <- daily_vax_counts %>% 
  group_by(manu) %>%
  summarize(total = sum(booster), .groups = "drop") %>%
  mutate(status = "Con booster")

totals <- bind_rows(totals, totals_booster)

outcome_tab <- left_join(outcome_tab, totals, by = c("manu", "status"))

save(counts, file=file.path(rda_path ,"counts.rda"))
save(summary_tab, outcome_tab, file=file.path(rda_path ,"tabs.rda"))
save(poblacion, file = file.path(rda_path ,"poblacion.rda"))
save(daily_vax_counts, file = file.path(rda_path, "daily_vax_counts.rda"))
save(daily_vax_counts_by_municipio, file = file.path(rda_path, "daily_vax_counts_by_municipio.rda"))
save(dat_cases, file =  file.path(rda_path ,"dat_cases.rda"))
save(immune, file = file.path(rda_path ,"immune.rda"))
