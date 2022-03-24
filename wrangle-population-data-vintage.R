library(tidyverse)
library(lubridate)
library(data.table)

rda_path <- "rdas"

## downlaoded data like this: curl https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/asrh/PRC-EST2020-SYASEX.csv >> PRC-EST2020-SYASEX.csv
raw_pop  <-
  read_csv("data/PRC-EST2020-SYASEX.csv") %>%
  filter(SEX!=0 & AGE != 999) %>%
  mutate(AGE2=AGE) %>%
  select(AGE, AGE2, SEX, POPESTIMATE2020) %>%
  setNames(c("start", "end", "gender", "poblacion")) %>%
  mutate(gender = ifelse(gender==1, "M", "F")) %>%
  mutate(gender = factor(gender)) %>%
  as.data.table() 



pr_pop <- sum(pop_by_age_gender$poblacion)
pr_adult_pop <- sum(pop_by_age_gender[!ageRange %in% c("0-4", "5-11", "12-15", "16-17")]$poblacion)


raw_pop_municipio <- 
  read_csv("data/PRM-EST2020-AGESEX.csv",  locale = locale(encoding = "ISO-8859-1"))  %>% filter(YEAR == 13) %>%
  select(NAME, matches("AGE.+_[FEM|MALE]")) %>%
  pivot_longer(-NAME) %>%
  mutate(name = str_remove(name, "AGE")) %>%
  separate(name, c("age","gender")) %>%
  mutate(age = recode(age, `04` = "0004", `59` = "0509", `85PLUS` = "85Inf")) %>% 
  pivot_wider(names_from = age, values_from = value) %>% 
  select(NAME, gender, "0004", "0509", "1014", "1519", "2024", "2529", "3034", "3539", "4044",
         "4549", "5054", "5559", "6064", "6569", "7074", "7579", "8084", "85Inf") %>%
  rename(municipio =  NAME) %>% 
  mutate(municipio = str_remove(municipio, " Municipio")) %>%
  mutate(gender = recode(gender, MALE = "M", FEM = "F")) %>%
  pivot_longer(-c("municipio","gender"),names_to = "age", values_to = "poblacion") %>%
  mutate(start = as.numeric(str_extract(age, "\\d{2}")),
         end = as.numeric(str_remove(age, "\\d{2}"))) %>%
  select(-age) %>%
  select(municipio, start, end, gender, poblacion) %>%
  as.data.table() 

save(raw_pop, raw_pop_municipio,
     file = file.path(rda_path, "population-tabs-vintage.rda"))

