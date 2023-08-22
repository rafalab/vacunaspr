source("init.R")
library(readxl)
library(stringdist)
library(data.table)

gender_levels <- c("F", "M", "O", "U")
#ageRange_starts <- c(0, 5, 12, 18, 30, 40, 50, 60, 70, 80)
ageRange_starts <- c(0, 5, 10, 16, 18, 22, seq(30, 85, 5))
ageRange_ends <- c(ageRange_starts[-1]-1, Inf)
ageRange_levels <- paste(ageRange_starts, ageRange_ends, sep = "-")
ageRange_levels[length(ageRange_levels)] <- paste0(ageRange_starts[length(ageRange_levels)],"+")
manu_levels <- c("UNV", "MOD", "PFR", "JSN")
first_ped_day <- make_date(2021, 11, 04)
first_booster_day <- make_date(2021, 8, 13) 
first_jnj_booster_day <- make_date(2021, 10, 22) 
second_booster_day <- make_date(2022, 03, 29)
second_jnj_booster_day <- make_date(2022, 03, 29)
####
load("rdas/dates.rda")
load("rdas/data_tidy.rda")
## Before removing data from other states count how many administered
administradas <- nrow(dat_tidy)


administradas_tasa <- nrow(dat_tidy[date %in% span_range]) /
  length(span_range) * 7

## Remove data from people form other states
dat_tidy[, en_pr := any(estado %in% c("PR", "No reportado") | municipio %in% municipios), by = "id"]
dat_tidy[, three_manu := all(manu %in% setdiff(manu_levels, "UNV")), by = "id"]

## keep ids of people taken out because these should not be considered unvaccinated
id_out <- dat_tidy[en_pr==FALSE | three_manu == FALSE]$id
## remove individuals with problematic records
## Improve this by developing approach to determining if "OTR" is actually PFR or MOD
dat_tidy <- dat_tidy[!id %in% id_out]

dat_tidy[, `:=`(en_pr = NULL, three_manu = NULL)]


## remove vaccinated after today or before 
if(any(dat_tidy$date > today()) | any(dat_tidy$date < make_date(2020, 12, 15))) stop("There are vaccines outside the range of dates")

## order by dose if not already ordered
if(!identical(dat_tidy, dat_tidy[order(id, dosis)])) dat_tidy <- dat_tidy[order(id, dosis)]

### Remove dosis that are too close to be a second dose

tmp_1 <- dat_tidy[dosis==1, .(id, manu, date)]
tmp_2 <- dat_tidy[dosis==2, .(id, date)]
tmp_3 <- dat_tidy[dosis==3, .(id, date)]
setnames(tmp_3, "date", "date_3")
tmp <- merge(tmp_1, tmp_2, by = "id", suffixes = c("_1", "_2"), all.x = TRUE) |>
  merge(tmp_3, by = "id", all.x = TRUE)

tmp[, out_2 := (manu=="PFR" & abs(date_2-date_1 - 21) > abs(date_3-date_1 - 21)) |
      (manu=="MOD"  & abs(date_2-date_1 - 28) > abs(date_3-date_1 - 28))]
tmp[is.na(out_2), out_2 := FALSE]
tmp <- tmp[, .(id, out_2)]
dat_tidy <- merge(dat_tidy, tmp, by = "id", all.x = TRUE)
dat_tidy <- dat_tidy[!(out_2==TRUE & dosis==2)]
dat_tidy[out_2==TRUE, dosis := order(dosis), by = "id"]
dat_tidy[, out_2:=NULL]


## remove 3rd or later dose that aren't far enough or happened before boosters
tmp <- copy(dat_tidy)
tmp[, n_dosis := .N, by = "id"]
tmp[, manu_1 := first(manu), by = "id"]
tmp[manu_1 == "JSN", first_series_completed := first(date),  by = "id"]
tmp[manu_1 != "JSN" & n_dosis > 1, first_series_completed := date[2], by = "id"]

tmp[, date_diff:=as.numeric(date)]
tmp[, date_diff := c(NA, diff(date_diff)), by = "id"]
tmp[, first_manu := first(manu), by = "id"]



tmp[
  ((first_manu != "JSN" & dosis>=3 & (date_diff < 150 | date < first_booster_day)) |
      (first_manu == "JSN" & dosis>=2 & (date_diff < 60 | date < first_jnj_booster_day)))]
tmp[, dosis := order(dosis), by = "id"]
tmp[, date_diff := NULL]


dat_vax[manu_1 != "JSN" &
          (date_3 < first_booster_day | date_3  - date_2 < days(150)) &
          (is.na(date_4) | (date_4 - date_2 >= days(150) & date_4 >= first_booster_day)),
        c("date_3", "ageRange_3", "manu_3", "insert_date_3", "proveedor_3",
          "date_4", "ageRange_4", "manu_4", "insert_date_4", "proveedor_4",
          "date_5", "ageRange_5", "manu_5", "insert_date_5", "proveedor_5",
          "extra_dose") :=
          .(date_4, ageRange_4, manu_4, insert_date_4, proveedor_4,
            date_5, ageRange_5, manu_5, insert_date_5, proveedor_5,
            NA, NA, NA, NA, NA,
            TRUE)]

dat_vax[manu_1 != "JSN" &
          (date_3 < first_booster_day | date_3  - date_2 < days(150)) &
          (date_4 - date_2 < days(150) | date_4 < first_booster_day),
        c("date_3", "ageRange_3", "manu_3", "insert_date_3", "proveedor_3",
          "date_4", "ageRange_4", "manu_4", "insert_date_4", "proveedor_4",
          "date_5", "ageRange_5", "manu_5", "insert_date_5", "proveedor_5",
          "extra_dose") :=
          .(NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA,
            TRUE)]


dat_vax[manu_1 == "JSN" & (date_2 < first_jnj_booster_day | date_2 - date_1 < days(60)) &
          (is.na(date_3) | (date_3 - date_1 >= days(60) & date_3 >= first_jnj_booster_day)) &
          (is.na(date_4) | (date_4 - date_3 >= days(120) & date_4 >= second_jnj_booster_day)),
        c("date_2", "ageRange_2", "manu_2", "insert_date_2", "proveedor_2",
          "date_3", "ageRange_3", "manu_3", "insert_date_3", "proveedor_3",
          "date_4", "ageRange_4", "manu_4", "insert_date_4", "proveedor_4",
          "date_5", "ageRange_5", "manu_5", "insert_date_5", "proveedor_5",
          "extra_dose") :=
          .(date_3, ageRange_3, manu_3, insert_date_3, proveedor_3,
            date_4, ageRange_4, manu_4, insert_date_4, proveedor_4,
            date_5, ageRange_5, manu_5, insert_date_5, proveedor_5,
            NA, NA, NA, NA, NA,
            TRUE)]

dat_vax[manu_1 == "JSN" & (date_2 < first_jnj_booster_day | date_2 - date_1 < days(60)) &
          (date_3 - date_1 < days(60) | date_3 < first_jnj_booster_day) &
          (is.na(date_4) | (date_4 - date_1 >= days(60) & date_4 >= first_jnj_booster_day)) ,
        c("date_2", "ageRange_2", "manu_2", "insert_date_2", "proveedor_2",
          "date_3", "ageRange_3", "manu_3", "insert_date_3", "proveedor_3",
          "date_4", "ageRange_4", "manu_4", "insert_date_4", "proveedor_4",
          "extra_dose") :=
          .(date_4, ageRange_4, manu_4, insert_date_4, proveedor_4,
            NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA,
            TRUE)]

dat_vax[manu_1 == "JSN" &
          (date_2 < first_jnj_booster_day | date_2 - date_1 < days(60)) &
          (date_3 - date_1 < days(60) | date_3 < first_jnj_booster_day) &
          (date_4 - date_1 < days(60) | date_4 < first_jnj_booster_day),
        c("date_2", "ageRange_2", "manu_2", "insert_date_2", "proveedor_2",
          "date_3", "ageRange_3", "manu_3", "insert_date_3", "proveedor_3",
          "date_4", "ageRange_4", "manu_4", "insert_date_4", "proveedor_4",
          "date_5", "ageRange_5", "manu_5", "insert_date_5", "proveedor_5",
          "extra_dose") :=
          .(NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA,
            TRUE)]

## define the vax date and booster date
dat_vax[manu_1 != "JSN",
        c("vax_date", "vax_ageRange", "booster_date", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange", "booster2_date", "booster2_manu", "booster2_insert_date", "booster2_proveedor", "booster2_ageRange") :=
          .(date_2, ageRange_2, date_3, manu_3, insert_date_3, proveedor_3, ageRange_3,  date_4, manu_4, insert_date_4, proveedor_4, ageRange_4)]

dat_vax[manu_1 == "JSN",
        c("vax_date", "vax_ageRange", "booster_date", "ageRange_2", "manu_2", "booster_manu", "booster_insert_date", "booster_proveedor", "booster_ageRange", "booster2_date", "booster2_manu", "booster2_insert_date", "booster2_proveedor", "booster2_ageRange") :=
          .(date_1, ageRange_1, date_2, ageRange_1, manu_1, manu_2, date_2, proveedor_2, ageRange_2, date_3, manu_3, insert_date_3, proveedor_3, ageRange_3)]

dat_vax[!is.na(vax_date), vax_date := vax_date + days(14)]
dat_vax[!is.na(booster_date), booster_date := booster_date + days(14)]
dat_vax[!is.na(booster2_date), booster2_date := booster2_date + days(14)]
dat_vax[vax_date > today(), vax_date := NA] ## not fully vax yet
dat_vax[booster_date > today(), booster_date := NA]
dat_vax[booster2_date > today() | booster2_date<= second_booster_day, booster2_date := NA]

## last data without needing a booster
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 != "JSN", last_immune_date := date_2 + days(150)]
dat_vax[is.na(booster_date) & !is.na(vax_date) & manu_2 == "JSN", last_immune_date := date_1 + days(60)]


#dat_vax[is.na(booster2_date) & !is.na(booster_date) & !is.na(vax_date) & manu_2 != "JSN" & vax_ageRange>=50, last_immune_date := date_3 + days(120)]
#dat_vax[is.na(booster2_date) & !is.na(booster_date) & !is.na(vax_date) & manu_2 == "JSN" &  vax_ageRange>=50, last_immune_date := date_2 + days(120)]


## check column names ##

colnames(dat_vax)

class(dat_vax)

## RDS for dat_casos_vacunados
save(dat_vax, file="~/R/vacunaspr/private_rdas/dat_vax_id_SYA.rda")

dat_vax <- dat_vax %>% 
  select(-id)  

colnames(dat_vax)

class(dat_vax)

the_stamp <- now()
## rda for public sharing 

save(dat_vax, administradas, administradas_tasa, the_stamp,
     file = "~/R/vacunaspr/rdas/dat_vax_SYA.rda")

