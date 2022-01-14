library(tidyverse)
library(lubridate)
library(data.table)
library(splines)

pr_pop <- 3285874 ## population of puerto rico
pr_adult_pop <- 2724903
pr_child_pop <- pr_pop - pr_adult_pop

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

pop  <- readxl::read_xlsx("data/prc-est2019-syasex.xlsx",  
                    skip = 4) %>%
  select("...1", contains(c("Male...", "Female..."))) %>% 
  rename(ageRange="...1") %>%
  pivot_longer(-ageRange, values_to = "poblacion") %>%
  filter(str_detect(ageRange, "^.\\d")) %>% 
  separate(name, c("gender", "year"), sep="\\.\\.\\.", convert = TRUE) %>%
  mutate(date = make_date(2007+floor(year/3), 7, 1)) %>%
  mutate(ageRange= str_remove(ageRange, "."))  %>% 
  mutate(ageRange = str_remove(ageRange,"\\+")) %>%
  mutate(ageRange = as.numeric(ageRange)) %>%
  mutate(ageRange = factor(as.character(cut(ageRange, c(breaks, Inf), right = FALSE,
                                            labels = labels)), levels = labels)) %>%
  filter(!is.na(ageRange)) %>%
  mutate(gender = recode(gender, Male = "M", Female="F")) %>%
  group_by(ageRange, gender, date) %>%
  summarize(poblacion = sum(poblacion), .groups = "drop")

dates <- seq(make_date(2008, 12, 15), last_day, by= "day")
extrapolate <- function(tab){
  fit <- lm(poblacion ~ ns(x,3), data = tab)
  data.frame(date = dates, 
             fit = predict(fit, newdata = data.frame(x=as.numeric(dates))))
}

pred_prop <- pop %>%  
  filter(year(date) != 2018) %>%
  mutate(x=as.numeric(date)) %>%
  group_by(ageRange, gender) %>%
  do(extrapolate(.)) %>%
  mutate(adult = !ageRange %in% c("0-4","5-11","12-17")) %>%
  group_by(date) %>%
  mutate(total = ifelse(adult, sum(fit[adult]), sum(fit[!adult]))) %>%
  ungroup() %>%
  mutate(prop = fit/total * ifelse(adult, pr_adult_pop, pr_child_pop)/pr_pop)
  
### check fit
if(FALSE){
  final_pop <- full_join(pop, pred_prop, by = c("date", "ageRange", "gender"))
  final_pop %>%  filter(year(date) != 2018) %>%
    ggplot(aes(date, color = gender))+
    geom_point(aes(y=poblacion))+
    geom_line(aes(y=fit))+
    facet_wrap(~ageRange, scale="free_y") +
    theme_bw()
  
  pred_prop %>%  filter(date >= make_date(2020, 12, 15)) %>%
    ggplot(aes(date, prop, color = gender))+
    geom_line()+
    facet_wrap(~ageRange, scale="free_y") +
    theme_bw()
  
}

pop_by_age_gender <- pred_prop %>%
  mutate(poblacion = prop*pr_pop) %>%
  select(date, ageRange, gender, poblacion) %>%
  as.data.table() 



# Municipios --------------------------------------------------------------

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
  mutate(ageRange = factor(ageRange, levels = labels))  %>%
  as.data.table() 


correction <- pop_by_age_gender_municipio %>% group_by(ageRange, gender) %>% 
  summarize(poblacion=sum(poblacion), .groups = "drop") %>%
  right_join(pop_by_age_gender, by = c("ageRange","gender")) %>%
  mutate(correction = poblacion.y / poblacion.x) %>% 
  select(-contains("poblacion"))

pop_by_age_gender_municipio <-pop_by_age_gender_municipio  %>%
  left_join(correction, by = c("ageRange", "gender")) %>%
  mutate(poblacion = poblacion*correction) %>%
  select(-correction) 

pop_by_age_gender_municipio[,municipio := reorder(municipio, -poblacion, sum)]

muni_order <- pop_by_age_gender_municipio %>%
  group_by(municipio) %>% summarize(n=sum(poblacion), .groups="drop") 
save(pr_pop, pr_adult_pop, pr_child_pop, pop_by_age_gender, pop_by_age_gender_municipio, 
     file = file.path(rda_path, "population-tabs.rda"))


rm(list=ls())

