library(tidyverse)
library(lubridate)
library(data.table)
library(tidycensus)

if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/vacunaspr/rdas"
} else{
  rda_path <- "rdas"
}

v20 <- load_variables(2020, "acs5", cache = TRUE)
tmp <- v20 %>% filter(concept == "SEX BY AGE" & str_detect(label,"years")) 
acs_labels <- tmp$name
names(acs_labels) <- tmp$label %>% 
  str_remove(" years") %>%
  str_replace("Under 5", "0-4") %>%
  str_replace("and over", "-Inf") %>%
  str_remove("Estimate\\!\\!Total:\\!\\!") %>%
  str_replace(":\\!\\!", "_") %>%
  str_remove_all("\\s+") %>%
  str_replace("and|to", "-") 

dat <- get_acs(geography = "state", 
               variables = acs_labels,
               state = "PR",
               year = 2020) 

raw_pop <- dat %>% 
  mutate(se = replace_na(moe, 0) / qnorm(0.95)) %>%  
  separate(variable, c("gender", "ageRange"), sep="_") %>%
  separate(ageRange, c("start", "end"), sep="-", fill = "right") %>%
  select(start, end, gender, estimate, se) %>%
  mutate(start=as.numeric(start), end=as.numeric(end)) %>%
  mutate(end = ifelse(is.na(end), start, end),
         ageRange = paste(start, end, sep="-")) %>%
  mutate(gender = factor(recode(gender, Male="M", Female = "F"))) 
  

# By municipio ------------------------------------------------------------


dat_muni <- get_acs(geography = "county", 
                    variables = acs_labels,
                    state = "PR",
                    year = 2020) 

raw_pop_municipio <- dat_muni %>% 
  mutate(NAME = str_remove(NAME, " Municipio, Puerto Rico")) %>%
  rename(municipio = NAME) %>%
  mutate(se = replace_na(moe, 0) / qnorm(0.95)) %>%  
  separate(variable, c("gender", "ageRange"), sep="_") %>%
  separate(ageRange, c("start", "end"), sep="-", fill = "right") %>%
  select(municipio, start, end, gender, estimate, se) %>%
  mutate(start=as.numeric(start), end=as.numeric(end)) %>%
  mutate(end = ifelse(is.na(end), start, end),
         ageRange = paste(start, end, sep="-")) %>%
  mutate(gender = factor(recode(gender, Male="M", Female = "F")))

## split 10-14

split_10_14 <- function(tab){
  return(data.frame(start=c(10,12), end=c(11,14), 
                    gender = tab$gender,
                    estimate=tab$estimate*c(2,3)/5,
                    se = tab$se*sqrt(c(2/3)/5),
                    ageRange=c("10-11", "12-14")))
}
  
tmp_10_14 <- filter(raw_pop, ageRange=="10-14") %>%
  group_by(gender) %>%
  do(split_10_14(.))

raw_pop <- raw_pop %>%
  filter(ageRange!="10-14") %>%
  bind_rows(tmp_10_14) 

age_levels <- paste(sort(unique(raw_pop$start)),
                    sort(unique(raw_pop$end)), sep="-")

raw_pop <- raw_pop %>%
  mutate(ageRange = factor(ageRange, levels=age_levels)) %>%
  arrange(ageRange, gender)


tmp_10_14 <- filter(raw_pop_municipio, ageRange=="10-14") %>%
  group_by(gender, municipio) %>%
  do(split_10_14(.))

raw_pop_municipio <- raw_pop_municipio %>%
  filter(ageRange!="10-14") %>%
  bind_rows(tmp_10_14) 

raw_pop_municipio <- raw_pop_municipio  %>%
  mutate(ageRange = factor(ageRange, levels=age_levels)) %>%
  arrange(municipio, ageRange, gender)
  

raw_pop <- setDT(raw_pop)
setnames(raw_pop, "estimate", "poblacion")
setcolorder(raw_pop, c("ageRange", "start", "end", "gender", "poblacion", "se"))
raw_pop_municipio <- setDT(raw_pop_municipio)
setnames(raw_pop_municipio, "estimate", "poblacion")
setcolorder(raw_pop_municipio, c("municipio", "ageRange", "start", "end", "gender", "poblacion", "se"))

## test: check if sum of municipios adds up to total
# tmp <- raw_pop_municipio[,.(poblacion=sum(poblacion), se=sqrt(sum(se^2))), 
#                          by = c("ageRange", "gender")]
# tmp <- merge(tmp, raw_pop, by = c("ageRange", "gender"))
# tmp%>%ggplot(aes(poblacion.x, poblacion.y, label=ageRange))+
#   geom_text()+geom_abline()
# tmp%>%ggplot(aes(se.x, se.y, label=ageRange))+
#   geom_text()+geom_abline()

pr_pop <- sum(raw_pop$poblacion)
pr_pop_se <- sqrt(sum(raw_pop$se^2))

pr_adult_pop <- sum(raw_pop[end<=17]$poblacion)
pr_adult_pop_se <- sqrt(sum(raw_pop[end<=17]$se))

save(pr_pop, pr_adult_pop, raw_pop, raw_pop_municipio, 
     file = file.path(rda_path, "population-tabs.rda"))





