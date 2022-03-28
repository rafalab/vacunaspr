## this sciprt is meant to be sourced from another script where
## collapse_age and counts_age_starts are definded

counts_pop_by_age_gender <- collapse_age(raw_pop, counts_age_starts)
counts_age_levels <- levels(counts_pop_by_age_gender$ageRange)

load(file.path(rda_path, "dat_vax_SYA.rda"))

load(file.path(rda_path, "dat_cases_vax_SYA.rda"))
load(file.path(rda_path, "dates.rda"))
all_dates <- data.table(date = seq(first_day, last_day, "days"))
dat_cases_vax[is.na(date) & !is.na(date_death), date := date_death]

## change ageRange from 12-17 that were vaccinated as 5-11 year olds
dat_cases_vax[ageRange >= 12 & ageRange <= 17 & ageRange_1 <12, ageRange := ageRange_1]
  
## collapse ages
counts_collapse_func <- function(x)
  cut(x, c(counts_age_starts, Inf),  right = FALSE, include.lowest = TRUE, labels = counts_age_levels)

cols <- str_subset(names(dat_vax), "ageRange")
dat_vax[, (cols) := lapply(.SD, counts_collapse_func), .SDcols = cols]

cols <- str_subset(names(dat_cases_vax), "ageRange")
dat_cases_vax$original_ageRange <- counts_collapse_func(dat_cases_vax$ageRange)
dat_cases_vax[, (cols) := lapply(.SD, counts_collapse_func), .SDcols = cols]


## remove correctional cases to avoid bias introduced by outbreak
dat_vax <- dat_vax[(is.na(proveedor_1) | proveedor_1!="Correccional") &
                     (is.na(proveedor_2) | proveedor_2!="Correccional"),]
dat_cases_vax <- dat_cases_vax[(is.na(proveedor_1) | proveedor_1!="Correccional") &
                                 (is.na(proveedor_2) | proveedor_2!="Correccional"),]


#
## define status and manufacturers
dat_cases_vax[, manu := factor(fifelse(is.na(manu_1), "UNV", as.character(manu_1)), levels=manu_levels)]
dat_cases_vax[, status := "UNV"]
dat_cases_vax[date >= date_1, status := "PAR"]
dat_cases_vax[date >= vax_date, status := "VAX"]
dat_cases_vax[date >= booster_date, status := "BST"]
dat_cases_vax[date < date_1, manu := "UNV"]
dat_cases_vax$status <- factor(dat_cases_vax$status, levels = c("UNV", "PAR", "VAX", "BST"))

## Compute number of unvaccinated that were susceptible
all_dates <- data.table(date = seq(first_day, last_day, "days"))
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
                         counts_pop_by_age_gender,  by = c("ageRange", "gender"), all.x=TRUE)
pop_susceptible[, poblacion := poblacion - cases]
setnames(pop_susceptible, "se", "se_poblacion")
pop_susceptible[, cases := NULL]

### Compute number with and without booster for each day after vaccinated
message("Computing populations for vax date/date combinations.")

compute_date_comb_counts <- function(tab, other_date = "booster_date"){
  the_date <- unique(tab$date)
  all_dates <- data.table(tmp = seq(the_date, last_day, "days"))
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
                     gender %in% c("F", "M") & vax_ageRange != "0-4",
                   compute_date_comb_counts(tab = .SD), 
                   keyby = c("vax_date", "manu_1", "vax_ageRange", "gender")]

setnames(pop_vax, c("vax_date", "manu", "ageRange", "gender", "date", "poblacion"))
setcolorder(pop_vax, c("date", "vax_date", "manu", "ageRange", "gender", "poblacion"))
pop_vax$manu <- factor(pop_vax$manu, levels = manu_levels)
pop_vax$ageRange <- factor(pop_vax$ageRange, levels = counts_age_levels)
pop_vax <- pop_vax[order(manu, ageRange, gender, date, vax_date)]
pop_vax %>% group_by(date,ageRange, manu) %>% summarize(n=sum(poblacion)) %>% ggplot(aes(date,n,color=manu))+geom_line()+facet_wrap(~ageRange)


message("Computing populations for vax date/date combinations for partial vax.")

## population of partially vaccinated
dat_vax[, date := date_1] ##make date date_1 for compute_date_comb_counts
pop_par <- dat_vax[!is.na(date_1) & date_1 <= last_day &
                     gender %in% c("F", "M") & ageRange_1 != "0-4",
                   compute_date_comb_counts(tab = .SD, other_date = "vax_date"), 
                   keyby = c("date_1", "manu_1", "ageRange_1", "gender")]
setnames(pop_par, c("vax_date", "manu", "ageRange", "gender", "date", "poblacion"))
setcolorder(pop_par, c("date", "vax_date", "manu", "ageRange", "gender", "poblacion"))
pop_par$manu <- factor(pop_par$manu, levels = manu_levels)
pop_par$ageRange <- factor(pop_par$ageRange, levels = counts_age_levels)
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
pop_unvax <- pop_unvax[,c("date", "ageRange","gender", "poblacion", "se_poblacion")]

#pop_unvax %>% ggplot(aes(date,poblacion, color=gender))+geom_line()+facet_wrap(~ageRange)
#pop_unvax %>% ggplot(aes(date, poblacion_2010/poblacion, color = gender)) + geom_line() + facet_wrap(~ageRange, scales="free_y")
# Booster counts ----------------------------------------------------------

### Booster counts - a bit different because we dont have to remove boosters... it's monotically increasing
message("Computing populations for vax date/date combinations for boosters.")

daily_counts_booster_age_gender_manu <- dat_vax[!is.na(booster_date), .(poblacion = .N),
                                                keyby = .(booster_date, booster_ageRange, gender, manu_1, booster_manu)]
names(daily_counts_booster_age_gender_manu) <- c("booster_date", "booster_ageRange", "gender", "manu_1", "booster_manu", "poblacion")

the_booster_ndays <- as.numeric(last_day - first_booster_day)

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
vax[,`:=`(primary_manu = NA, status = "VAX", se_poblacion = NA)]

message("Computing daily counts.")

par <- dat_cases_vax[status == "PAR"  & gender %in% c("F", "M") & ageRange != "0-4" & date <= last_day]
setnames(par, "date_1", "vax_date")
par <- par[, .(cases = .N, hosp = sum(hosp), death = sum(death)), keyby = .(manu, ageRange, gender, date, vax_date)]
par <- merge(pop_par, par, by = c("manu", "ageRange", "gender", "date", "vax_date"), all.x = TRUE) 
par[is.na(par)] <- 0
par <- melt(par, measure.vars = c("cases", "hosp", "death"), variable.name = "outcome", value.name = "obs")
par[,`:=`(primary_manu=NA, status = "PAR", se_poblacion = NA)]


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
bst[, `:=`(status = "BST", se_poblacion = NA)]


unvax <- dat_cases_vax[status == "UNV" & gender %in% c("F", "M")]
unvax <- unvax[, .(cases = .N, hosp = sum(hosp), death = sum(death)), keyby = .(date, ageRange, gender)]
unvax <- merge(pop_unvax, unvax, by = c("date", "ageRange", "gender"), all.x = TRUE) 
unvax[is.na(unvax)] <- 0
unvax <- melt(unvax, measure.vars = c("cases", "hosp", "death"), variable.name = "outcome", value.name = "obs")

unvax[,`:=`(primary_manu=NA, manu="UNV", vax_date=make_date(NA), status = "UNV")]

## store counts for each vax date and case date combination
counts <- rbindlist(list(par, vax, bst, unvax), use.names = TRUE)
rm(par, vax, bst, unvax); gc(); gc()
counts[, outcome := factor(outcome, levels = c("cases", "hosp","death"))]
counts[, status := factor(status, levels = c("UNV","PAR","VAX","BST"))]
counts[, variant := fcase(date < make_date(2021, 6, 15), "alpha",
                          date >= make_date(2021, 6, 15) & date < make_date(2021,12,8), "delta",
                          date >= make_date(2021, 12, 8), "omicron")]
counts[, day :=fifelse(status!="UNV", as.numeric(date - vax_date), 0)]

