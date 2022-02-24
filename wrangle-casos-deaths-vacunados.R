source("~/R/vacunaspr/init.R")
library(data.table)

age_starts <- c(0, 5, 12, 18, seq(25, 85, 5))
age_ends <- c(age_starts[-1]-1, Inf)
age_levels <- paste(age_starts, age_ends, sep = "-")
age_levels[length(age_levels)] <- paste0(age_starts[length(age_levels)],"+")
manu_levels <- c("UNV", "MOD", "PFR", "JSN")
first_ped_day <- make_date(2021, 11, 04)
first_booster_day <- make_date(2021, 8, 13) 
first_jnj_booster_day <- make_date(2021, 10, 22)
first_day_cases <- make_date(2020,09,15)
gender_levels <- c( "F", "M", "O", "U")

# useful function

convert_date <- function(d) as_date(ymd_hms(d, tz = "America/Puerto_Rico"))
recode_manu <- function(x)  recode(x, 'Johnson & Johnson' = 'JSN', 
                                         Moderna = 'MOD', 
                                         Pfizer = "PFR", 
                                         Astrazeneca = "AZN")
### load case data

dat_cases <-  readRDS("~/R/vacunaspr/rdas/casos.RDS") %>%
  mutate(lastHospitalizedDate = na_if(lastHospitalizedDate, "0001-01-01T00:00:00Z")) %>%
  mutate(across(contains("Dat"), convert_date)) %>%
  filter(caseType %in% c("Confirmed", "Probable") &
           earliestPositiveDiagnosticSampleCollectedDate >= first_day_cases &
           earliestPositiveDiagnosticSampleCollectedDate  <= today()-days(2)) %>% 
  mutate(age = floor(as.numeric(difftime(earliestPositiveDiagnosticSampleCollectedDate, birthDate, units="days"))/365.25)) %>%
  mutate(ageRange = cut(age, c(age_starts, Inf), labels = age_levels,  right=F )) %>% 
  mutate(ageRange = factor(ageRange, levels = age_levels)) %>%
  as_tibble() %>%
  rename(gender = sex,
         date = earliestPositiveDiagnosticSampleCollectedDate,
         date_hosp = lastHospitalizedDate,
         date_death = deathDate) %>%
  select(-contains("Name"), -age, -birthDate) %>%
  mutate(across(contains("manu"), recode_manu)) %>% 
  mutate(gender = case_when(str_starts(gender, "F|F") ~ "F",
                            str_starts(gender, "M|m") ~ "M", 
                            str_starts(gender, "U|u") ~ "U",
                            str_starts(gender, "O|o") ~ "O", 
                            is.na(gender) ~ "O")) %>%
  mutate(gender = factor(gender, levels = gender_levels)) 

## include death data 

deaths_BP <- readRDS("~/R/vacunaspr/rdas/deaths_BP.RDS") %>%
  setNames(c("patientId", "birthDate", "gender", "physicalCity", "personDiedCovid", "date_death")) %>%
  filter(!(patientId %in% dat_cases$patientId)) %>%
  mutate(across(contains("date"), convert_date)) %>%
  mutate(gender = case_when(str_starts(gender, "F|F") ~ "F",
                            str_starts(gender, "M|m") ~ "M", 
                            str_starts(gender, "U|u") ~ "U",
                            str_starts(gender, "O|o") ~ "O", 
                            is.na(gender) ~ "O")) %>%
  mutate(gender = factor(gender, levels = gender_levels)) %>%
  mutate(age = floor(as.numeric(difftime(date_death, birthDate, units="days"))/365.25)) %>%
  mutate(ageRange = cut(age, c(age_starts, Inf), labels = age_levels,  right=F )) %>% 
  mutate(ageRange = factor(ageRange, levels = age_levels))  %>%
  select(-age, -birthDate)
  
### merge death data with case data

dat_cases <- rbindlist(list(deaths_BP, dat_cases), fill=T) %>% distinct(patientId, .keep_all = T)

### FOR NOW USE MY OWN MATCH
load("~/R/vacunaspr/rdas/bp_preis_maps.rda")
dat_vax <- readRDS("rdas/dat_vax.rds")
bp_preis_map <- final_map %>% select(id.x, id.y, score) %>%
  setNames(c("patientId", "PATIENT_ID", "score"))

dat_cases <- dat_cases %>%  
  left_join(bp_preis_map, by = "patientId") %>%
  left_join(dat_vax, by = c("PATIENT_ID" = "id")) %>%
  filter(!(is.na(date) & is.na(date_death)))

dat_cases_vax <- dat_cases %>% 
  select(date, ageRange, gender.x, date_hosp, 
          date_death, municipio, estado, date_1, manu_1, proveedor_1,
         date_2, manu_2, proveedor_2, date_3, manu_3, proveedor_3, 
         date_4, manu_4, proveedor_4, extra_dose, vax_date, booster_date, booster_manu, 
         booster_proveedor, last_immune_date, score) %>%
  mutate(hosp = if_else(is.na(date_hosp), FALSE, TRUE)) %>%
  mutate(death =if_else(is.na(date_death), FALSE, TRUE)) %>%
  rename(gender = "gender.x") %>%
  data.table::as.data.table()

class(dat_cases_vax)

the_stamp_cases <- now()

save(dat_cases_vax,
     the_stamp_cases,
     file = "~/R/vacunaspr/rdas/dat_cases_vax.rda")

save(dat_cases_vax,
     the_stamp_cases,
     file ="~/R/casos-vacunados-pr/boosters/rdas/dat_cases_vax.rda")

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

save(dat_cases, file="~/R/vacunaspr/rdas/dat_cases.rda")




