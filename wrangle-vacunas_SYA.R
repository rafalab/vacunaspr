source("~/R/vacunaspr/init.R")
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
load("~/R/vacunaspr/rdas/dates.rda")
load("~/R/vacunaspr/private_rdas/preis_map.rda")

convert_date <- function(d) as_date(ymd_hms(d, tz = "America/Puerto_Rico"))

# Read in vaccine records -------------------------------------------------
# this file is not publicly shared so this wont run

load("~/R/vacunaspr/private_rdas/vax_data_BP.rda")

# RAFAs date correction 
download_date <- as_date(max(vax_data_BP$INSERT_STAMP)) + days(1)
first_day <- make_date(2020,12,15)
preis <- vax_data_BP %>% as.data.table()

preis[, date := as_date(ADMINISTRATION_DATE)]

preis[, insert_date := convert_date(INSERT_STAMP)]

preis[, vac_code := factor(VACC_CODE)]
preis[, manu := fct_collapse(vac_code, "MOD" = c("2080"), 
                     "PFR" = c("2089", "3015", "3016", "3017", "3012"),
                     "ATZ" = c("2091"), 
                     "JSN" = c("2092"),
                     "EVC" = c("3012"), 
                     "OTR" = c("2090"))]
preis[, manu := factor(manu, levels = unique(manu))]

preis[, original_date := date]

preis[, dob := as_date(DOB)]

preis[date < first_day | date > download_date, date := as_date(NA)]

preis[, gender := fcase(is.na(PATIENT_GENDER), "O",
                        str_starts(PATIENT_GENDER, "F|f"), "F",
                        str_starts(PATIENT_GENDER, "M|m"), "M",
                        str_starts(PATIENT_GENDER, "U|u"), "U")]
preis[, gender := factor(gender, levels = gender_levels)]

preis[, fixed_year := make_date(2021, month(original_date), day(original_date))]
preis[month(original_date) == 12, fixed_year := make_date(2020, month(original_date), day(original_date))]
                            
preis[, date := fcase((!is.na(date)), date, 
                      (is.na(date) & month(original_date) == 12 & year(original_date) == 2020),  make_date(2020, 12, 15), 
                      (is.na(date) & dob!=original_date &
                         as.numeric(insert_date - fixed_year) <= 30 & insert_date >= fixed_year), fixed_year)] 

preis[is.na(date), date := insert_date]
                            
preis[, ageRange := floor(as.numeric(difftime(date, dob, units=c("days")))/365)]
preis[, ageRange := cut(ageRange, c(ageRange_starts, Inf), labels = ageRange_levels, right=F)] 

preis[, municipio := str_to_title(PATIENT_CITY)]
preis[, municipio := fix_municipio(municipio)]
preis[ is.na(municipio), municipio := "No reportado"]
preis[, municipio := factor(municipio, levels = unique(municipio))]

preis[, estado := PATIENT_STATE]
preis[is.na(estado), estado := "No reportado"] 
preis[, estado := factor(estado, levels = unique(estado))]

preis[, proveedor:=FACILITY_NAME]
preis <- preis %>% simplify_proveedor()  %>% as.data.table()
preis[, proveedor := factor(proveedor, levels = unique(proveedor))]
preis[is.na(proveedor), proveedor := "No reportado"]

preis <- preis[!(!(vac_code %in% c("3016", "3015", "3017")) & ageRange %in% c("5-11")),]

preis[, id := PATIENT_ID]
preis <- preis[, c("id", "gender", "ageRange","municipio", 
                   "estado", "date", "manu", "insert_date", "proveedor")]
  

preis<-preis[date<=the_preis_map_stamp,]


## Remove dups
## remove rows with duplicated dates
na_false <- function(x){x[is.na(x)]<-FALSE;return(x)}
## start conservative
preis <- merge(preis, 
               preis_map[score >= 0.55|
                           (score >= 0.50 & genero_match)| 
                           (score >= 0.45 & lugar_match & genero_match) |
                           full_prop_match >= 0.7 |
                           (full_prop_match >= 0.65 & genero_match)  |
                           (full_prop_match >= 0.60 & lugar_match & genero_match),
                         c("id.x","id.y")],#, #"score", "full_prop_match", "genero_match", "lugar_match")],
               by.x ="id", by.y="id.y", all.x=TRUE) %>%
  as.data.table()
 
## define new id combining possible duplicates
preis[, new.id := ifelse(is.na(id.x), id, id.x)]
## remove duplicated dates, using new id... same date considered too much of a concidence
preis <- unique(preis, by=c("new.id", "date"))
## count how many dosiss with new ids
preis[, n := .N, by = new.id]
## if 3 or less assume its duplicate
preis[, id := ifelse(n<=3, new.id, id)]
## order and define dosiss
preis <- preis[order(id, date)]

preis <- preis[, dosis := 1:.N, keyby = "id"]
## remove extra rows
preis <- preis[, !c("new.id","n","id.x")]

save(preis , file = "data_tidy.rda")

### hasta aqui con ids ### -----------------------------------------------------

## Deal with Janssen 
## if different dosis manufacturers, we keep them. Use second as the main one
 
dat_vax <- full_join(filter(preis, dosis == 1), 
                     filter(preis, dosis == 2),
                     by = "id", suffix = c("", "_2")) %>%
  full_join(filter(preis, dosis == 3), by="id", suffix = c("" ,"_3")) %>%
  full_join(filter(preis, dosis == 4), by="id", suffix = c("" ,"_4")) %>%
  full_join(filter(preis, dosis == 5), by="id", suffix = c("_1" ,"_5")) %>%
#   mutate(ageRange_1 = if_else(!is.na(ageRange_2) & is.na(ageRange_1), 
#                               ageRange_2, 
#                               ageRange_1)) %>%
  mutate(gender_1 = if_else(!is.na(gender_2) & is.na(gender_1), 
                            gender_2, 
                            gender_1)) %>%
  mutate(gender_1 = if_else(gender_2 %in% c("F","M") & !gender_1 %in% c("F","M"), 
                            gender_2, 
                            gender_1)) %>%
  select( -dosis_1, -dosis_2, -dosis_3, - dosis_4, -dosis_5, -municipio_2, -municipio_3, -municipio_4, -municipio_5,
          -estado_2, -estado_3, -estado_4, -estado_5, -gender_2, -gender_3, -gender_4, -gender_5) %>%
  rename(municipio = "municipio_1", gender = "gender_1", estado = "estado_1") %>%
  data.table::as.data.table()


## Before removing data from other states count how many administered
administradas <- sum(!is.na(dat_vax$date_1)) +
  sum(!is.na(dat_vax$date_2)) +
  sum(!is.na(dat_vax$date_3)) + 
  sum(!is.na(dat_vax$date_4)) +
  sum(!is.na(dat_vax$date_5))

administradas_tasa <- 
  (filter(dat_vax, date_1 %in% span_range) %>% nrow +
     filter(dat_vax, date_2 %in% span_range) %>% nrow +
     filter(dat_vax, date_3 %in% span_range) %>% nrow  + 
     filter(dat_vax, date_4 %in% span_range) %>% nrow  + 
     filter(dat_vax, date_5 %in% span_range) %>% nrow)/length(span_range) * 7

## Remove data from people form other states
dat_vax <- dat_vax[estado %in% c("PR", "No reportado")] 

## remove vaccinated after today or before 
dat_vax <- dat_vax[date_1 <= today() & date_1 >= make_date(2020, 12, 15)] 

### hasta aqui se corrio para ultima version de dat_vax ### --------------------


# ## Remove anybody that had dose of anything other than Pfizer, Moderna or J&J
dat_vax <- dat_vax[manu_1 %in% setdiff(manu_levels, "UNV") &
                     (manu_2 %in% setdiff(manu_levels, "UNV") | is.na(manu_2)) &
                     (manu_3 %in% setdiff(manu_levels, "UNV") | is.na(manu_3)) &
                     (manu_4 %in% setdiff(manu_levels, "UNV") | is.na(manu_4)) &
                     (manu_5 %in% setdiff(manu_levels, "UNV") | is.na(manu_5))  ]
dat_vax$manu_1 <- droplevels(dat_vax$manu_1)
dat_vax$manu_2 <- droplevels(dat_vax$manu_2)
dat_vax$manu_3 <- droplevels(dat_vax$manu_3)
dat_vax$manu_4 <- droplevels(dat_vax$manu_4)
dat_vax$manu_5 <- droplevels(dat_vax$manu_5)

## fix date_2 too early. if date_3 is closer to the scheduled 2nd dose remove it
dat_vax[manu_1=="PFR" &
          abs(date_2-date_1 - 21) > abs(date_3-date_1 - 21),
        c("date_2", "ageRange_2", "manu_2", "insert_date_2", "proveedor_2",
          "date_3", "ageRange_3", "manu_3", "insert_date_3", "proveedor_3",
          "date_4", "ageRange_4", "manu_4", "insert_date_4", "proveedor_4",
          "date_5", "ageRange_5", "manu_5", "insert_date_5", "proveedor_5")  :=
          .(date_3, ageRange_3, manu_3, insert_date_3, proveedor_3,
            date_4, ageRange_4, manu_4, insert_date_4, proveedor_4,
            date_5, ageRange_5, manu_5, insert_date_5, proveedor_5,
            NA, NA, NA, NA, NA)]

dat_vax[manu_1=="MOD"  &
          abs(date_2-date_1 - 28) > abs(date_3-date_1 - 28),
        c("date_2", "ageRange_2", "manu_2", "insert_date_2", "proveedor_2",
          "date_3", "ageRange_3", "manu_3", "insert_date_3", "proveedor_3",
          "date_4", "ageRange_4", "manu_4", "insert_date_4", "proveedor_4",
          "date_5", "ageRange_5", "manu_5", "insert_date_5", "proveedor_5") :=
          .(date_3, ageRange_3, manu_3, insert_date_3, proveedor_3,
            date_4, ageRange_4, manu_4, insert_date_4, proveedor_4,
            date_5, ageRange_5, manu_5, insert_date_5, proveedor_5,
            NA, NA, NA, NA, NA)]

## remove 3rd dosis that aren't far enough
dat_vax[, extra_dose:=FALSE]

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

