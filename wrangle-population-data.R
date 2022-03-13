library(tidyverse)
library(lubridate)
library(data.table)

# pr_pop <- 3285874 ## population of puerto rico
# pr_adult_pop <- 2724903
# pr_child_pop <- pr_pop - pr_adult_pop


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
## downlaoded data like this: curl https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/asrh/PRC-EST2020-SYASEX.csv >> PRC-EST2020-SYASEX.csv
pop_by_age_gender  <-
  read_csv("data/PRC-EST2020-SYASEX.csv") %>%
  select(SEX, AGE, POPESTIMATE2020) %>%
  setNames(c("gender", "ageRange", "poblacion")) %>%
  filter(gender !=0 & ageRange != 999) %>%
  mutate(gender = ifelse(gender==1, "M", "F")) %>%
  #  mutate(g = ifelse(ageRange>= 18, "adult", "child")) %>%
  #  left_join(data.frame(g = c("adult", "child"), new = c(pr_adult_pop, pr_child_pop)), by = "g") %>%
  # group_by(g) %>%
  #  mutate(poblacion = poblacion/sum(poblacion)*new) %>%
  #  ungroup() %>%
  mutate(ageRange = factor(as.character(cut(ageRange, c(breaks, Inf), right = FALSE,
                                            labels = labels)), levels = labels)) %>%
  group_by(ageRange, gender) %>%
  summarize(poblacion = sum(poblacion), .groups = "drop")  %>%
  mutate(gender = factor(gender)) %>%
  as.data.table() 

pr_pop <- sum(pop_by_age_gender$poblacion)
pr_adult_pop <- sum(pop_by_age_gender[!ageRange %in% c("0-4", "5-11", "12-15", "16-17")]$poblacion)


pop_by_age_gender_municipio <- 
  read_csv("data/PRM-EST2020-AGESEX.csv",  locale = locale(encoding = "ISO-8859-1"))  %>% filter(YEAR == 13) %>%
  select(NAME, matches("AGE.+_[FEM|MALE]")) %>%
  pivot_longer(-NAME) %>%
  mutate(name = str_remove(name, "AGE")) %>%
  separate(name, c("age","gender")) %>%
  mutate(age = recode(age, `04` = "0004", `59` = "0509", `513` = "0513")) %>% 
  pivot_wider(names_from = age, values_from = value) %>% 
  mutate(`1013` = `0513`-`0509`,
         `1017` = `1013` + `1417`,
         `85Inf` = `85PLUS`) %>%
  mutate(`0511` = 0.5* `0509` * 7/5 + 0.5 * `0513`*7/9,
         `1215` = `1017` * 4/8, 
         `1617` = `1017` * 2/8) %>%
  select(NAME, gender, "0004", "0511", "1215", "1617", "1824", "2529", "3034", "3539", "4044",
         "4549", "5054", "5559", "6064", "6569", "7074", "7579", "8084", "85Inf") %>%
  rename(municipio =  NAME) %>% 
  mutate(municipio = str_remove(municipio, " Municipio")) %>%
  mutate(gender = recode(gender, MALE = "M", FEM = "F")) %>%
  pivot_longer(-c("municipio","gender"),names_to = "age", values_to = "poblacion") %>%
  mutate(age_start = as.numeric(str_extract(age, "\\d{2}")),
         age_end = as.numeric(str_remove(age, "\\d{2}"))) %>%
  mutate(ageRange = cut(age_start, c(age_starts, Inf), right = FALSE, labels = labels)) %>% 
  group_by(municipio, ageRange, gender) %>%
  summarize(poblacion = sum(poblacion), .groups = "drop") %>%
  mutate(ageRange = factor(ageRange, levels = labels))  %>%
  as.data.table() 


correction <- pop_by_age_gender_municipio %>% group_by(ageRange, gender) %>% 
  summarize(poblacion=sum(poblacion), .groups = "drop") %>%
  left_join(pop_by_age_gender, by = c("ageRange","gender")) %>%
  mutate(correction = poblacion.y / poblacion.x) %>% 
  select(-contains("poblacion"))

pop_by_age_gender_municipio <- pop_by_age_gender_municipio  %>%
  right_join(correction, by = c("ageRange", "gender")) %>%
  mutate(poblacion = poblacion*correction) %>%
  select(-correction) 

pop_by_age_gender_municipio[,municipio := reorder(municipio, -poblacion, sum)]
setcolorder(pop_by_age_gender_municipio, c("municipio", "ageRange", "gender", "poblacion"))

muni_order <- pop_by_age_gender_municipio %>%
  group_by(municipio) %>% summarize(n=sum(poblacion), .groups="drop") 
save(pr_pop, pr_adult_pop, pop_by_age_gender, pop_by_age_gender_municipio, 
     file = file.path(rda_path, "population-tabs.rda"))


