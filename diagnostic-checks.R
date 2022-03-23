
##check if pop_vax and pop_par match totals

booster <- dat_vax[!is.na(booster_date) &
                     vax_date <= last_day &
                     manu_1 == "PFR" &
                     gender == "F" & vax_ageRange == "12-17", .(booster=.N), by = "booster_date"][order(booster_date)]
setnames(booster, "booster_date","date")

vax <- dat_vax[!is.na(vax_date) &
                     vax_date <= last_day &
                     manu_1 == "PFR" &
                     gender == "F" & vax_ageRange == "12-17", .(full=.N), by = "vax_date"][order(vax_date)]
setnames(vax, "vax_date","date")


tmp <- merge(vax, booster, by = "date", all = TRUE)
tmp[is.na(tmp)]<-0

left_join(pop_vax[ageRange=="12-17" & gender=="F" & manu=="PFR"] %>% 
            group_by(date) %>% 
            summarize(pop=sum(poblacion)),
          tmp[order(date)] %>% mutate(vax=cumsum(full)-cumsum(booster)),
          by = "date") %>%
  ggplot(aes(date))+
  geom_line(aes(y=vax - pop))
  #geom_line(aes(y=pop),lty=1)+
  #geom_line(aes(y=vax),lty=2)
  
  

onedose <- dat_vax[!is.na(date_1) &
                     date_1 <= last_day &
                     manu_1 == "PFR" &
                     gender == "F" & ageRange_1 == "12-17", .(onedose=.N), by = "date_1"]
setnames(onedose, "date_1","date")

vax <- dat_vax[!is.na(vax_date) &
                 vax_date <= last_day &
                 manu_1 == "PFR" &
                 gender == "F" & ageRange_1 == "12-17", .(full=.N), by = "vax_date"]
setnames(vax, "vax_date","date")


tmp <- merge(onedose, vax, by = "date", all = TRUE)
tmp[is.na(tmp)]<-0


left_join(pop_par[ageRange=="12-17" & gender=="F" & manu=="PFR"] %>% 
            group_by(date) %>% 
            summarize(pop=sum(poblacion)),
          tmp[order(date)] %>% mutate(vax=cumsum(onedose)-cumsum(full)),
          by = "date") %>%
  ggplot(aes(date))+
 geom_line(aes(y=vax - pop))
#geom_line(aes(y=pop),lty=1)+
#geom_line(aes(y=vax),lty=2)

