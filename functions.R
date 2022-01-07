make_pct <- function(x, digit = 1) ifelse(is.na(x), "", paste0(format(round(100*x, digit = digit), nsmall = digit), "%"))
make_pretty <- function(x) prettyNum(replace_na(x, " "), big.mark = ",")
get_ci_lower <- function(n, p, alpha = 0.05) qbinom(alpha/2, n, p) / n
get_ci_upper <- function(n, p, alpha = 0.05) qbinom(1-alpha/2, n, p) / n
make_pretty_ci <- function(p, lower, upper, nsmall = 1, bounds_nsmall = 1){
  floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
  ifelse(is.na(p), "", 
         paste0(format(round(100*p, nsmall), nsmall = nsmall), " ",
                "(", trimws(format(floor_dec(100*lower, bounds_nsmall), nsmall = bounds_nsmall)),"" , ", ",
                format(ceiling_dec(100*upper, bounds_nsmall), nsmall = bounds_nsmall),"", 
                ")"
         ))
}

dynamic_round <- function(x, min_rounded = 10, digits=1){
  ifelse(round(x) >= min_rounded, 
         prettyNum(round(x), big.mark = ","),
         prettyNum(round(x, digits = digits), nsmall = digits, big.mark = ",")) %>%
    replace_na("")
}

digits <- function(x, digits = 2){
  ifelse(x < 10^(-digits),
         paste("<",10^(-digits)),
         prettyNum(round(x, digits = digits), nsmall = digits, big.mark = ","))
}

ma7 <- function(d, y, k = 7) 
  tibble(date = d, moving_avg = as.numeric(stats::filter(y, rep(1/k, k), side = 1)))


make_datatable <- function(tab, col.names = colnames(tab),
                           align = rep("c", ncol(tab)),
                           nowrap = 1){
  
  tab <- tab %>% 
    mutate(dummy = date, date = format(date, "%B %d")) %>%
    arrange(desc(dummy)) %>%
    setNames(c(col.names, "dummy"))
  
  column_defs <- list(list(targets = 0, orderData = ncol(tab)-1),
                      list(targets = ncol(tab)-1, visible = FALSE))
  centers <- which(align == "c")-1
  if(length(centers)>0) 
    column_defs <- append(column_defs, list(list(className = 'dt-center', targets = centers)))
  
  lefts <- which(align == "l")-1
  if(length(lefts)>0) 
    column_defs <- append(column_defs, list(list(className = 'dt-left', targets = lefts)))
  
  rights <- which(align == "r")-1
  if(length(rights)>0) 
    column_defs <- append(column_defs, list(list(className = 'dt-right', targets = rights)))
  
  
  DT::datatable(tab, 
                rownames = FALSE,
                options = list(dom = 't', pageLength = -1,
                               columnDefs = column_defs)) %>%
    DT::formatStyle(nowrap, "white-space" = "nowrap")
}


to_english <-  function(x){
  replace_na(x, "") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ü", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_remove_all("\\s+") 
}

fix_municipio <- function(x, min.dist = 2, 
                          min.score = 1/3, 
                          min.score.barrio = 2/9,
                          first.letter.match = TRUE,
                          return.dist = FALSE){
  
  
  municipios <- c("Adjuntas", "Aguada", "Aguadilla", "Aguas Buenas", "Aibonito", "Añasco",
                  "Arecibo", "Arroyo", "Barceloneta", "Barranquitas", "Bayamón", "Cabo Rojo", 
                  "Caguas", "Camuy", "Canóvanas", "Carolina", "Cataño", "Cayey", "Ceiba",
                  "Ciales", "Cidra", "Coamo", "Comerío", "Corozal", "Culebra", "Dorado", 
                  "Fajardo", "Florida", "Guánica", "Guayama", "Guayanilla", "Guaynabo", 
                  "Gurabo", "Hatillo", "Hormigueros", "Humacao", "Isabela", "Jayuya", 
                  "Juana Díaz", "Juncos", "Lajas", "Lares", "Las Marías", "Las Piedras", 
                  "Loíza", "Luquillo", "Manatí", "Maricao", "Maunabo", "Mayagüez", "Moca", 
                  "Morovis", "Naguabo", "Naranjito", "Orocovis", "Patillas", "Peñuelas", 
                  "Ponce", "Quebradillas", "Rincón", "Río Grande", "Sabana Grande", "Salinas", 
                  "San Germán", "San Juan", "San Lorenzo", "San Sebastián", "Santa Isabel", 
                  "Toa Alta", "Toa Baja", "Trujillo Alto", "Utuado", "Vega Alta", "Vega Baja", 
                  "Vieques", "Villalba", "Yabucoa", "Yauco")
  
  x <- as.factor(x)
  query <- levels(x)
  ans <- as.character(rep(NA, length(query)))
  barrio <- as.character(rep(NA, length(query)))
  the_dist <- rep(NA, length(ans))
  the_score <- rep(NA, length(ans)) 
  
  if(all(query %in% municipios)){ # if all map, we are done
    ans <- query
    barrio <- as.character(rep(NA, length(query)))
  } else{ ## if not, we will convert to lower case, remvoe accents and non-characters
    map_mun <- data.frame(id = to_english(municipios), municipio = municipios)
    
    query <- query %>%
      to_english() %>%
      str_remove_all("[^a-zA-Z]+") 
    
    ## remove commone errors
    query[query=="puertorico" | query == "unknown" | query == "pr"] <- ""
    
    ## we will store answers in ans and if matching barrios, store that in barrio
    ind1 <- query %in% map_mun$id
    ans[ind1] <- map_mun$municipio[match(query[ind1], map_mun$id)]
    
    ## if not all matched keep going
    if(!all(ind1)){
      ## read-in barrios and keep only the unique ones
      ## if there is ambiguity we do not assing a municipio
      
      url <- "https://raw.githubusercontent.com/rafalab/pr-covid/master/coalicion/fix-municipio/data/pr-barrios.csv"
      barrios <- read_csv(url,
                          col_types = cols(municipio = col_character(),
                                           barrio = col_character())) %>%
        group_by(barrio) %>% ##remove duplicates
        mutate(n=n()) %>% 
        filter(n==1) %>%
        select(-n) %>% ## add commonly used names by hand
        bind_rows(data.frame(
          barrio = c("Río Piedras", "Hato Rey", "Barrio Obrero", "Bo Obrero", "Condado", "Miramar", 
                     "Caparra Heights","College Park","Villa Palmeras", 
                     "Fort Buchanan", "Caparra",
                     "Mercedita", 
                     "Saint Just", "Trujillo", "Tru Alto",
                     "Levittown", 
                     "Bajadero", 
                     "Ramey", "Aquadilla",
                     "Palmer",
                     "Isla Verde", "Urb Country Club",
                     "La Plata",
                     "Villa Rica", "Rio Hondo",
                     "Palmas del Mar",
                     "Cieba", "Roosvelt Roads",
                     "Campanilla",
                     "Caomo",
                     "Yuaco",
                     "Lioza",
                     "Tao Baja",
                     "Tao Alta",
                     "A Buenas"),
          municipio = c("sanjuan","sanjuan","sanjuan","sanjuan","sanjuan", "sanjuan",
                        "sanjuan","sanjuan","sanjuan", 
                        "guaynabo","guaynabo",
                        "ponce", 
                        "trujilloalto", "trujilloalto","trujilloalto",
                        "catano", 
                        "arecibo",
                        "aguadilla", "aguadilla",
                        "riogrande",
                        "carolina", "carolina",
                        "aibonito",
                        "bayamon", "bayamon",
                        "humacao",
                        "ceiba", "ceiba",
                        "toabaja",
                        "coamo",
                        "yauco",
                        "loiza",
                        "toabaja",
                        "toalta",
                        "aguasbuenas"))) %>%
        mutate(original_barrio = barrio) %>%
        mutate(barrio=to_english(barrio), municipio = to_english(municipio)) %>%
        rename(id = municipio)%>%
        left_join(map_mun, by = "id")
      
      ## fix one barrio name
      barrios$barrio[barrios$barrio == "barriopueblo(isabelii)"] <- "isabellii"
      
      ## use tmp to match barrios or mispellings in query to barrios in our table
      tmp <- data.frame(barrio = query[!ind1]) %>% left_join(barrios, by = "barrio") 
      ans[!ind1] <- tmp$municipio
      barrio[!ind1] <- tmp$original_barrio
      
      ## keep those not matched to search and see if municioio a subset like sanjuan in sanjuanpr 
      ind <- which(is.na(ans))
      if(length(ind)>0){
        
        for(i in 1:nrow(map_mun)){
          ind2 <- str_which(query[ind], map_mun$id[i])
          if(length(ind2)>0) ans[ind[ind2]] <- map_mun$municipio[i]
        }
        
        
        ##for those not matched and find fuzzy match with municipios
        ind <- which(is.na(ans))
        
        if(length(ind)>0){  
          
          ## distance to municipio
          d_mun <- stringdistmatrix(query[ind], map_mun$id, method = "lv")
          ind_mun <- apply(d_mun, 1, which.min)
          min_d_mun <- apply(d_mun, 1, min) 
          if(first.letter.match){ ##require first letter matches
            min_d_mun[str_sub(query[ind],1,1) != str_sub(map_mun$id[ind_mun],1,1)] <- Inf
          }
          score_mun <- min_d_mun / nchar(query[ind])
          
          ##if criteria met, keep it
          keep <- min_d_mun <= min.dist & score_mun <= min.score 
          
          if(length(keep)>0){
            ans[ind][keep] <-map_mun$municipio[ind_mun][keep]
            the_dist[ind][keep] <- min_d_mun[keep]
            the_score[ind][keep] <- score_mun[keep]
          }
          ##for those not matched check for fuzzy match with barrio
          ind <- which(is.na(ans))
          
          if(length(ind)>0){
            #distance to barrio
            d_bar <- stringdistmatrix(query[ind], barrios$barrio, method = "lv")
            ind_bar <- apply(d_bar, 1, which.min)
            min_d_bar <- apply(d_bar, 1, min) 
            if(first.letter.match){
              min_d_bar[str_sub(query[ind], 1, 1) != str_sub(barrios$barrio[ind_bar], 1, 1)] <- Inf
            }
            score_bar <- min_d_bar / nchar(query[ind])
            
            keep <- min_d_bar <= min.dist & score_bar <= min.score.barrio
            if(length(keep)>0){
              ans[ind][keep] <- barrios$municipio[ind_bar][keep]
              barrio[ind][keep] <- barrios$original_barrio[ind_bar][keep]
              the_dist[ind][keep] <- min_d_bar[keep]
              the_score[ind][keep] <- score_bar[keep]
            }
            
            ind <- which(is.na(ans))
            
            if(length(ind)>0){
              ##check if barrio included
              for(i in 1:nrow(barrios)){
                ind2 <- str_which(query[ind], barrios$barrio[i])
                if(length(ind2)>0){
                  ans[ind[ind2]] <- barrios$municipio[i]
                  barrio[ind[ind2]] <- barrios$original_barrio[i]
                }
              }
            }
          }
        }
      }
    }
  }  
  look_up <- data.frame(original = levels(x),
                        predicted = ans,
                        barrio.match = barrio,
                        dist = the_dist,
                        score = the_score)
  
  ret <- data.frame(original = as.character(x)) %>% 
    left_join(look_up, by = "original")
  
  if(return.dist){
    return(ret)
  } else{
    return(ret$predicted)
  }  
}
simplify_proveedor <- function(x, col_name = "proveedor") {
  x %>%
    mutate("{col_name}" := case_when(str_detect(proveedor, "WALGREENS") ~ "Walgreens",
                                     str_detect(proveedor, "WALMART|SAM'S|AMIGO") ~ "Walmart/Sams/Amigo",
                                     str_detect(proveedor, "COSTCO ") ~ "Costco",
                                     str_detect(proveedor, "VETERANS|VETERANOS") ~ "Veteranos",
                                     str_detect(proveedor, "CVS") ~ "CVS",
                                     str_detect(proveedor, "VOCES") ~ "Voces",
                                     str_detect(proveedor, "PONCE HEALTH SCIENCE UNIV|PONCE HLTH SCI UNIV HUB") ~ "Ponce Health",
                                     str_detect(proveedor, "PR NATIONAL GUARD") ~ "Guardia Nacional",
                                     str_detect(proveedor, "CDVA") ~ "CDVA",           
                                     str_detect(proveedor, "COLEGIO MEDICOS CIRUJANOS") ~ "Colegio de Médicos", #Victor y su pandilla",
                                     str_detect(proveedor, "DESARROLLO SALUDN") ~ "Desarrollo de la Salud",
                                     str_detect(proveedor, "CORREC.*CIONAL|CORRECTIONAL|CARCEL") ~ "Correccional",
                                     str_detect(proveedor, "FARM.*|PHARM.*") ~ "Otras farmacias",
                                     str_detect(proveedor, "HOS|HOSP|HOSPTIAL|HIMA") ~ "Hospitales",
                                     str_detect(proveedor, "MED.* CENTER") ~ "Hospitales",
                                     str_detect(proveedor, "MEDICAL SCIENCE CAMPUS UPR") ~ "Ciencias Médicas", 
                                     str_detect(proveedor, "UNIVERSIDAD|UPR|UNIV|COLEGIO|COLLEGE|SECUNDA.*|SCHOOL|ACADEM|COL|ESC|RECINTO|CCD") ~ "Otras Universidades/Escuelas/Cuidos",
                                     str_detect(proveedor, "CDT") ~ "CDTs",
                                     str_detect(proveedor, "DR|DRA|GRUPO|OFIC.*INA.*|.*CLINIC.*|CLINC") ~ "Doctores",
                                     str_detect(proveedor, ".*CENTRO.*|.*CENTER.*|.*CNTR|CTR|PRYMED|SALUS|CERVAC|QCDC|GROUP|INST|WIC|SERV|INC|MALL|LLC|CORP|COSSMA|PROMED|FMC|FRESENIUS") ~ "Centros de Salud",
                                     TRUE ~ "Otros")) }
  
make_outcome_tab <- function(tab, complete = TRUE, details = FALSE){
  if(details){
    tab <- tab %>% 
      filter(complete_week == complete) %>%
      mutate(status = recode(status, UNV = "No vacunados", 
                             PAR= "Parcial", 
                             VAX="Vacunados sin booster", 
                             BST = "Vacunados con booster")) %>%
      mutate(manu = recode(as.character(manu), UNV = "", MOD = "Moderna", 
                           PFR = "Pfizer", JSN = "J & J")) %>%
      mutate(n = make_pretty(round(n)),
             cases = make_pretty(cases), 
             rate_cases =  digits(rate_cases * 10^5, 1),
             hosp = make_pretty(hosp), 
             rate_hosp =  digits(rate_hosp * 10^5, 1),
             death = make_pretty(death), 
             rate_death =  digits(rate_death * 10^5, 2)) %>%
      select(status, manu, n, cases, rate_cases, hosp,rate_hosp, death, rate_death)
      kableExtra::kbl(tab, col.names = c("Vacunación", "Tipo de vacuna", "Número de personas", "Casos", "Casos por 100K por día", "Hosp", "Hosp por 100K por día", "Muertes","Muertes por 100K por día"),
                      align = c("c","c", rep("r", 8)))  %>%
      kableExtra::kable_styling() %>%
      kableExtra::column_spec(1, width = "12em") %>%
        row_spec(which(tab$status=="No vacunados"), bold = T, color = "black", background = status_colors[["UNV"]]) %>%
        row_spec(which(tab$status=="Parcial"), bold = T, color = "black", background = status_colors[["PAR"]]) %>%
        row_spec(which(tab$status=="Vacunados sin booster"), bold = T, color = "black", background = status_colors[["VAX"]]) %>%
        row_spec(which(tab$status=="Vacunados con booster"), bold = T, color = "black", background = status_colors[["BST"]])}
  else{
    tab <- tab %>% 
      filter(complete_week == complete) %>%
      mutate(status = recode(status, UNV = "No vacunados", 
                             PAR= "Parcial", 
                             VAX="Vacunados sin booster", 
                             BST = "Vacunados con booster")) %>%
      mutate(n = make_pretty(round(n)),
             cases = make_pretty(cases), 
             rate_cases =  digits(rate_cases * 10^5, 1),
             hosp = make_pretty(hosp), 
             rate_hosp =  digits(rate_hosp * 10^5, 1),
             death = make_pretty(death), 
             rate_death =  digits(rate_death * 10^5, 2)) %>%
      select(status, n, cases, rate_cases, hosp,rate_hosp, death, rate_death)
      kableExtra::kbl(tab,col.names = c("Vacunación", "Número de personas", "Casos", "Casos por 100K por día", "Hosp", "Hosp por 100K por día", "Muertes","Muertes por 100K por día"),
                      align = c("c","c", rep("r", 7)))  %>%
      kableExtra::kable_styling() %>%
      kableExtra::column_spec(1, width = "12em") %>%
      row_spec(which(tab$status=="No vacunados"), bold = T, color = "black", background = status_colors[["UNV"]]) %>%
      row_spec(which(tab$status=="Parcial"), bold = T, color = "black", background = status_colors[["PAR"]]) %>%
      row_spec(which(tab$status=="Vacunados sin booster"), bold = T, color = "black", background = status_colors[["VAX"]]) %>%
      row_spec(which(tab$status=="Vacunados con booster"), bold = T, color = "black", background = status_colors[["BST"]])
  }
}

