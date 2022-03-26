library(tidyverse)
library(lubridate)
library(data.table)
library(tidycensus)
library(splines)

rda_path <- "rdas"


# Define variables --------------------------------------------------------


vars <- load_variables(2019, "acs1", cache = TRUE)
tmp <- vars %>% filter(concept == "SEX BY AGE" & str_detect(label,"years")) 
acs_labels <- tmp$name
names(acs_labels) <- tmp$label %>% 
  str_remove(" years") %>%
  str_replace("Under 5", "0-4") %>%
  str_replace("and over", "-Inf") %>%
  str_remove("Estimate\\!\\!Total:\\!\\!") %>%
  str_replace(":\\!\\!", "_") %>%
  str_remove_all("\\s+") %>%
  str_replace("and|to", "-") 


# Download acs data for each year -----------------------------------------


dat <- map_df(c(2017:2019), function(y){
  tmp <- get_acs(geography = "state", 
                 variables = acs_labels,
                 state = "PR",
                 year = y,
                 survey = "acs1")
  tmp$year <- y
  return(tmp)
})


# wrangle data ------------------------------------------------------------


raw_pop <- dat %>% 
  mutate(se = replace_na(moe, 0) / qnorm(0.95)) %>%  
  separate(variable, c("gender", "ageRange"), sep="_") %>%
  separate(ageRange, c("start", "end"), sep="-", fill = "right") %>%
  select(year, start, end, gender, estimate, se) %>%
  mutate(start=as.numeric(start), end=as.numeric(end)) %>%
  mutate(end = ifelse(is.na(end), start, end)) %>%
  rename(poblacion = estimate) %>% 
  mutate(gender = factor(recode(gender, Male="M", Female = "F"))) %>%
  mutate(date = make_date(year, 7, 1))


# extrapolate for 2020 and 2021 -------------------------------------------


dates <- make_date(c(unique(raw_pop$year), 2020,2021), 7, 1) # seq(min(raw_pop$date), make_date(2021, 7, 1), by= "year")
#dates <- make_date(c(2020,2021), 7, 1) # seq(min(raw_pop$date), make_date(2021, 7, 1), by= "year")
extrapolate <- function(tab){
  fit <- lm(poblacion ~ gender + x, data = tab)
  ret <-  expand.grid(gender=c("M","F"), date=dates) %>% mutate(x=as.numeric(date))
  ret$fit <- predict(fit, newdata =ret)
  return(ret)
}

pred_pop <- raw_pop %>%  
  mutate(x=as.numeric(date)) %>%
  group_by(start, end) %>%
  do(extrapolate(.)) 

### check fit
if(FALSE){
  final_pop <- full_join(raw_pop, pred_pop, by = c("date", "start", "end", "gender"))
  final_pop %>% #filter(start %in% c(10,15)) %>%
    mutate(ageRange=paste(start, end, sep="-")) %>%
    ggplot(aes(date, color = gender))+
    geom_point(aes(y=poblacion))+
    geom_line(aes(y=fit))+
    facet_wrap(~ageRange) +
    theme_bw()
}


## keep extrapolated population but original SEs

tmp1 <- raw_pop %>% filter(year == max(year)) %>% select(start, end, gender, poblacion, se) %>%
  rename(poblacion_2019 = poblacion) 

tmp2 <- pred_pop %>% 
  mutate(year = paste("poblacion", year(date), sep="_")) %>%
  select(year, start, end, gender, fit) %>% 
  pivot_wider(names_from = year, values_from = fit)


raw_pop <- left_join(tmp1, tmp2, by = c("start", "end", "gender")) %>% 
  rename(poblacion_2019 = poblacion_2019.y)

# Municipios --------------------------------------------------------------

## for municipios we get 2019 vintage and get age proportions from there

dat_muni <- get_estimates(geography = "county",
                          product = "characteristics",
                          breakdown = c("AGEGROUP","SEX"),
                          breakdown_labels = TRUE,
                          year= 2019,
                          state="Puerto Rico")

raw_pop_municipio <- dat_muni %>% 
  filter(!AGEGROUP %in% c("All ages", "Median age", "85 years and over") & SEX!="Both sexes") %>%
  mutate(NAME = str_remove(NAME, " Municipio, Puerto Rico")) %>%
  rename(municipio = NAME, gender= SEX, poblacion = value) %>%
  mutate(municipio = factor(municipio)) %>%
  mutate(AGEGROUP = str_remove_all(AGEGROUP, "Age\\s|\\syears")) %>%
  mutate(AGEGROUP = str_replace(AGEGROUP, " and over| and older", " to Inf")) %>%
  mutate(AGEGROUP = str_replace(AGEGROUP, "Under ", "0 to ")) %>%
  separate(AGEGROUP, c("start", "end"), sep=" to ", fill = "right") %>%
  select(municipio, start, end, gender, poblacion) %>%
  mutate(start=as.numeric(start), end=as.numeric(end)) %>%
  mutate(end = ifelse(is.na(end), start, end)) %>%
  mutate(gender = factor(recode(gender, Male="M", Female = "F"))) %>%
  filter(end-start==4 | (start==85 & is.infinite(end)))


age_starts <- unique(raw_pop_municipio$start)
breaks <- sort(age_starts)
labels <- c(paste(breaks[-length(breaks)], c(breaks[-1]-1), sep="-"),
            paste0(breaks[length(breaks)], "+"))

## correction to make totals match

total_pop <- raw_pop %>% 
  mutate(ageRange = cut(start, c(age_starts, Inf), right = FALSE, include.lowest = TRUE, labels = labels))%>%
  group_by(ageRange, gender) %>%
  summarize(across(starts_with("poblacion"), ~ sum(.x)), .groups = "drop")

raw_pop_municipio 
raw_pop_municipio <- raw_pop_municipio %>% 
  mutate(ageRange = paste(start, end, sep="-") %>% str_replace("-Inf", "+") %>% factor(levels=labels))%>%
  group_by(ageRange, gender) %>%
  mutate(total=sum(poblacion))%>%
  ungroup() %>%
  mutate(prop = poblacion/total) %>%
  select(-poblacion) %>%
  left_join(total_pop, by = c("ageRange","gender")) %>%
  mutate(across(starts_with("poblacion"), ~  .x * prop)) %>%
  select(municipio, start, end, gender, starts_with("poblacion"))

raw_pop <- setDT(raw_pop)
raw_pop_municipio <- setDT(raw_pop_municipio)

save(raw_pop, raw_pop_municipio, file = file.path(rda_path, "population-tabs-acs.rda"))






