library(tidyverse)
library(lubridate)
library(data.table)

pr_pop <- 3285874 ## population of puerto rico

if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/vacunaspr/rdas"
} else{
  rda_path <- "rdas"
}
load(file.path(rda_path, "dates.rda"))

age_starts <- as.numeric(str_extract(age_levels, "\\d+"))
breaks <- sort(age_starts)
labels <- c(paste(breaks[-length(breaks)], c(breaks[-1]-1), sep="-"),
            paste0(breaks[length(breaks)], "+"))
pop_by_age_gender  <-
    readxl::read_xlsx("data/prc-est2019-syasex.xlsx",  
                               skip = 4) %>%
  select("...1", "Male...36", "Female...37")  %>% 
  setNames(c("ageRange", "M", "F")) %>%
  mutate(ageRange= str_remove(ageRange, "."))  %>% 
  filter(str_detect(ageRange, "^\\d")) %>% 
  mutate(ageRange = str_remove(ageRange,"\\+")) %>%
  mutate(ageRange = as.numeric(ageRange)) %>% 
  pivot_longer(-ageRange, names_to="gender", values_to = "poblacion") 

pr_adult_pop <- pop_by_age_gender %>% filter(ageRange>= 18) %>%
  summarize(total = sum(poblacion)) %>%
  pull(total)
old_pop <- sum(pop_by_age_gender$poblacion)

pop_by_age_gender <- pop_by_age_gender %>%
  mutate(poblacion = poblacion*pr_pop/old_pop) %>% 
  mutate(ageRange = factor(as.character(cut(ageRange, c(breaks, Inf), right = FALSE,
                                            labels = labels)), levels = labels)) %>%
  filter(!is.na(ageRange)) %>%
  group_by(ageRange, gender) %>%
  summarize(poblacion = sum(poblacion), .groups = "drop") %>%
  as.data.table() 

pop_by_age_gender_municipio <- 
  read_csv("data/prm-est2019-agesex.csv",  locale = locale(encoding = "ISO-8859-1"))  %>% filter(YEAR == 12) %>%
  select(NAME, matches("AGE.+_[FEM|MALE]")) %>%
  pivot_longer(-NAME) %>%
  mutate(name = str_remove(name, "AGE")) %>%
  separate(name, c("age","gender")) %>%
  mutate(age = recode(age, `04` = "0004", `59` = "0509", `513` = "0513")) %>% 
  pivot_wider(names_from = age, values_from = value) %>% 
  mutate(`1013` = `0513`-`0509`,
         `1017` = `1013` + `1417`,
         `1829` = `1824` + `2529`,
         `80Inf` = `8084` + `85PLUS`) %>%
  mutate(`0511` = 0.5* `0509` * 7/5 + 0.5 * `0513`*7/9,
         `1217` = `1017` * 6/8) %>%
  select(NAME, gender, "0004", "0511", "1217", "1829", "3034", "3539", "4044",
         "4549", "5054", "5559", "6064", "6569", "7074", "7579", "80Inf") %>%
  rename(municipio =  NAME) %>% 
  mutate(municipio = str_remove(municipio, " Municipio")) %>%
  mutate(gender = recode(gender, MALE = "M", FEM = "F")) %>%
  pivot_longer(-c("municipio","gender"),names_to = "age", values_to = "poblacion") %>%
  mutate(age_start = as.numeric(str_extract(age, "\\d{2}")),
         age_end = as.numeric(str_remove(age, "\\d{2}"))) %>%
  mutate(ageRange = cut(age_start, c(age_starts, Inf), right = FALSE, labels = labels)) %>% 
  group_by(municipio, ageRange, gender) %>%
  summarize(poblacion = sum(poblacion), .groups = "drop") %>%
  mutate(ageRange = factor(ageRange, levels = labels))

correction <- pop_by_age_gender_municipio %>% group_by(ageRange, gender) %>% 
  summarize(poblacion=sum(poblacion), .groups = "drop") %>%
  left_join(pop_by_age_gender, tmp, by = c("ageRange","gender")) %>%
  mutate(correction = poblacion.y / poblacion.x) %>% 
  select(-contains("poblacion"))

pop_by_age_gender_municipio  %>% left_join(correction, by = c("ageRange", "gender")) %>%
  mutate(poblacion = poblacion*correction) %>%
  select(-correction)

save(pr_pop, pr_adult_pop, pop_by_age_gender, pop_by_age_gender_municipio, 
     file = file.path(rda_path, "population-tabs.rda"))


