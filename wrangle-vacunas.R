source("~/R/vacunaspr/init.R")
library(readxl)
library(stringdist)
library(data.table)

gender_levels <- c("F", "M", "O", "U")
#age_starts <- c(0, 5, 12, 18, seq(30, 80, 10))
age_starts <- c(0, 5, 12, 18, seq(25, 85, 5))
age_ends <- c(age_starts[-1]-1, Inf)
age_levels <- paste(age_starts, age_ends, sep = "-")
age_levels[length(age_levels)] <- paste0(age_starts[length(age_levels)],"+")

####

convert_date <- function(d) as_date(ymd_hms(d, tz = "America/Puerto_Rico"))

# Read in vaccine records -------------------------------------------------
# this file is not publicly shared so this wont run

load("~/R/vacunaspr/rdas/vax_data_BP.rda")

# RAFAs date correction 
download_date <- as_date(max(vax_data_BP$INSERT_STAMP)) + days(1)

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

preis[, gender := PATIENT_GENDER]
preis[is.na(gender), gender := "O"]
preis[, gender := factor(gender, levels = gender_levels)]

preis[, fixed_year := make_date(2021, month(original_date), day(original_date))]
preis[month(original_date) == 12, fixed_year := make_date(2020, month(original_date), day(original_date))]
                            
preis[, date := fcase((!is.na(date)), date, 
                      (is.na(date) & month(original_date) == 12 & year(original_date) == 2020),  make_date(2020, 12, 15), 
                      (is.na(date) & dob!=original_date &
                         as.numeric(insert_date - fixed_year) <= 30 & insert_date >= fixed_year), fixed_year)] 

preis[is.na(date), date := insert_date]
                            
preis[, age := floor(as.numeric(difftime(date, dob, units=c("days")))/365)]
preis[, ageRange := cut(age, c(age_starts, Inf), labels = age_levels, right=F)] 

preis[, municipio := str_to_title(PATIENT_CITY)]
preis[, municipio := fix_municipio(municipio)]
preis[ is.na(municipio), municipio := "No reportado"]
preis[, municipio := factor(municipio, levels = unique(municipio))]

preis[, estado := PATIENT_STATE]
preis[is.na(estado), estado := "No reportado"] 
preis[, estado := factor(estado, levels = unique(estado))]

preis[, proveedor := simplify_proveedor(FACILITY_NAME)]
preis[, proveedor := factor(proveedor, levels = unique(proveedor))]
preis[is.na(proveedor), proveedor := "No reportado"]

preis <- preis[!(!(vac_code %in% c("3016", "3015", "3017")) & ageRange %in% c("5-11")),]

preis[, id := PATIENT_ID]
preis <- preis[, c("id", "gender", "ageRange","municipio", 
                   "estado", "date", "manu", "insert_date", "proveedor")]
  
###
load("~/R/vacunaspr/rdas/preis_map.rda")
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
# 
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



## Deal with Janssen 
## if different dosis manufacturers, we keep them. Use second as the main one
 
dat_vax <- full_join(filter(preis, dosis == 1), 
                     filter(preis, dosis == 2),
                     by = "id", suffix = c("", "_2")) %>%
  full_join(filter(preis, dosis == 3), by="id", suffix = c("" ,"_3")) %>%
  full_join(filter(preis, dosis == 4), by="id", suffix = c("_1" ,"_4")) %>%
  mutate(ageRange_1 = if_else(!is.na(ageRange_2) & is.na(ageRange_1), 
                              ageRange_2, 
                              ageRange_1)) %>%
  mutate(gender_1 = if_else(!is.na(gender_2) & is.na(gender_1), 
                            gender_2, 
                            gender_1)) %>%
  mutate(gender_1 = if_else(gender_2 %in% c("F","M") & !gender_1 %in% c("F","M"), 
                            gender_2, 
                            gender_1)) %>%
  select( -dosis_1, -dosis_2, -dosis_3, - dosis_4, -municipio_2, -municipio_3, -municipio_4,
          -estado_2, -estado_3, -estado_4, -gender_2, -gender_3, -gender_4) %>%
  rename(municipio = "municipio_1", gender = "gender_1", estado = "estado_1") %>%
  data.table::as.data.table()


## check column names ##

colnames(dat_vax)

class(dat_vax)

## RDS for dat_casos_vacunados
saveRDS(dat_vax, file="~/R/vacunaspr/rdas/dat_vax.rds")

dat_vax <- dat_vax %>% 
  select(-id)  

colnames(dat_vax)

class(dat_vax)

the_stamp <- now()
## rda for public sharing 

save(dat_vax, the_stamp,
     file = "~/R/vacunaspr/rdas/dat_vax.rda")

save(dat_vax, the_stamp,
     file = "~/R/casos-vacunados-pr/boosters/rdas/dat_vax.rda")
