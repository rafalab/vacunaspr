source("init.R")

library(shiny)
library(shinythemes)

button_style <- "color: black; background-color: rgb(230, 220, 205); position: relative; 
                     text-align:center;border-radius: 6px; border-width: 2px"

ui <- fluidPage(
  theme = shinytheme("cosmo"),
                
  # -- Meta data
  tags$head(
    tags$meta(name="description", content="Puerto Rico COVID-19 Vaccine Dashboard"),
    tags$meta(name="keywords", content="Puerto Rico, Dashboard, COVID-19"),
    tags$meta(name="author", content="Mónica Robles-Fontán, José Zavala, Rafael A. Irizarry")
  ),
  
  #-- Google analytics add on
  tags$head(includeHTML(("google-analytics.html"))),
  
  # CSS
  tags$head(tags$style(HTML(".leaflet-container {
    background-color:rgba(255,255,255,1.0);
  }"))),
  
  # Application title
  titlePanel("Informe de vacunas COVID-19 en Puerto Rico (en construcción)"),
  
  tabsetPanel(id = "tabs", 
              tabPanel("Resumen",
                       htmlOutput("fecha"),
                       htmlOutput("summary_1"),
                       hr(),
                       htmlOutput("summary_2")
              ),
              tabPanel("Eventos",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("event_type",
                                       "",
                                       choice = c(Muertes = "death",
                                                  Hospitalizaciones = "hosp",
                                                  Casos = "cases"),
                                       selected = "death"),
                           dateRangeInput("event_range", "Periodo", 
                                          start = last_day - days(240),
                                          end = last_day,
                                          format = "M-dd-yyyy",
                                          language = "es",
                                          width = "100%",
                                          min = first_day,
                                          max = last_day),
                           selectInput("event_agerange",
                                       "Grupo de Edad",
                                       choice = c("Agregados" = "all",
                                                  "Todos" = "facet",
                                                  rev(age_levels[-1])),
                                       selected = "all"),
                           width = 3),
                         mainPanel(
                            plotOutput("muertes_plot"),
                            DT::dataTableOutput("muertes_tabla"))
                         )),
              
              tabPanel("Gráficas",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("time_type",
                                       "Tipo de gráfico",
                                       choice = c(Acumulado = "total",
                                                  Diario = "daily"),
                                       selected = "total"),
                           selectInput("status_type",
                                       "Estado de vacunación",
                                       choice = c(`Una dosis` = "onedose",
                                                  `Completa` = "full",
                                                  `Booster` = "booster", 
                                                  `Sin necesidad de booster` = "immunized"),
                                       selected = "full"),
                           selectInput("manu_type",
                                       "Tipo de vacuna",
                                       choice = c(`Pfizer` = "PFR",
                                                  `Moderna` = "MOD", 
                                                  `J&J` = "JSN",
                                                  `Todas` = "all", 
                                                  `Agregadas` = "together"),
                                       selected = "together"), 
                           dateRangeInput("dose_timerange", "Periodo", 
                                          start = last_day - days(240),
                                          end = last_day,
                                          format = "M-dd-yyyy",
                                          language = "es",
                                          width = "100%",
                                          min = first_day,
                                          max = last_day),
                           selectInput("dose_agerange",
                                       "Grupo de Edad",
                                       choice = c("Agregados" = "all",
                                                  "Todos" = "facet",
                                                  rev(age_levels[-1])),
                                       selected = "all"),
                           width = 3),
                         mainPanel(
                           plotOutput("people_plot"))
                       )),
              tabPanel("Municipios",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("municipio_agerange",
                                       "Grupo de Edad",
                                       choice = c("Agregados" = "all",
                                                  rev(age_levels[-1])),
                                       selected = "all"),
                           width = 3),
                         mainPanel(
                           plotOutput("mapa"),
                           leafletOutput("mapa_leaflet"),
                           DT::dataTableOutput("municipio_tabla")
                         )
                       )),
             
             
              tabPanel("Pirámide",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("piramide_municipio",
                                       "Municipio",
                                       choice = c(Todos = "Todos",
                                                  municipios),
                                       selected = "Todos"),
                           width = 3),
                         mainPanel(
                           plotOutput("piramide"),
                           htmlOutput("piramide_tabla")
                         )
                       )),
              tabPanel("Proveedores",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("proveedor_agerange",
                                       "Grupo de Edad",
                                       choice = c("Agregados" = "all",
                                                  age_levels[-1]),
                                       selected = "Todos"),
                           selectInput("proveedor_dose",
                                       "Dosis",
                                       choice = c("Agregadas" = "all",
                                                  "Primera" = "Primera",
                                                  "Completa" = "Segunda",
                                                  "Booster" = "Booster"),
                                       selected = "all"),
                           selectInput("proveedor_manu",
                                       "Tipo de vacuna",
                                       choice = c("Agregados" = "all",
                                                  "Pfizer" = "PFR",
                                                  "Moderna" = "MOD",
                                                  "J&J" = "JSN"),
                                       selected = "all"),
                           width = 3),
                         mainPanel(
                           htmlOutput("proveedores")
                         )
                       )),
              tabPanel("Datos Diarios", 
                       sidebarLayout(
                         sidebarPanel(
                           dateRangeInput("tabla_range", "Periodo", 
                                          start = last_day - days(60),
                                          end = last_day,
                                          format = "M-dd-yyyy",
                                          language = "es",
                                          width = "100%",
                                          min = first_day,
                                          max = last_day),
                           width = 3),
                         mainPanel(
                           DT::dataTableOutput("tabla")
                         )
                       )
              ) 
            ),
  htmlOutput("update"),
)
              




server <- function(input, output, session) {
    
  output$fecha <- renderText({
  load(file.path(rda_path, "dates.rda"))
    paste0("<h4>Datos para ", 
           format(last_day, "%B %d, %Y:"), "</h4>")})
  
  output$update <- renderText({
    paste0("<h4>Última actualización: ", format(the_stamp, "%B %d, %Y"), "</h4>")
  })
  
  output$summary_1 <- renderText({
    load(file.path(rda_path, "tabs.rda"))
    summary_tab %>% 
      mutate(total = make_pretty(total),
             porciento = make_pct(porciento),
             tasas = make_pretty(round(tasas))) %>%
      select(names, total, tasas, porciento) %>%
      kableExtra::kbl(col.names = c("", "Total", "Por semana", "Por ciento de la población"),
                    align = c("l","r","r","r")) %>%
    kableExtra::kable_styling()
  
  })
  
  output$summary_2 <- renderText({
    load(file.path(rda_path, "tabs.rda"))
  
    outcome_tab %>% 
      mutate(status = recode(status, UNV = "No vacunados", PAR= "Parcial", VAX="Vacunados")) %>%
      mutate(manu = recode(as.character(manu), UNV = "", MOD = "Moderna", 
                           PFR = "Pfizer", JSN = "J & J")) %>%
      mutate(n = make_pretty(round(n)),
             cases = make_pretty(cases), 
             rate_cases =  digits(rate_cases * 10^5/365, 1),
             hosp = make_pretty(hosp), 
             rate_hosp =  digits(rate_hosp * 10^5/365, 1),
             death = make_pretty(death), 
             rate_death =  digits(rate_death * 10^5/365, 2), 
             total = make_pretty(round(total))) %>%
      select(status, manu, total, n, cases, rate_cases, hosp,rate_hosp, death, rate_death) %>%
      kableExtra::kbl(col.names = c("Vacunación", "Tipo de vacuna", "Total", "Años-Persona", "Casos", "Casos por 100,000 por día", "Hosp", "Hosp por 100,000 por día", "Muertes","Muertes por 100,000 por día"),
                      align = c("c","c", rep("r", 8)))  %>%
      kableExtra::kable_styling() %>%
      kableExtra::column_spec(1, width = "12em")
  })
  
   output$muertes_plot <- renderPlot({
     load(file.path(rda_path, "counts.rda"))
     
     the_title <- case_when(
       input$event_type == "death" ~ "Tasa de mortalidad por estado de vacunación",
       input$event_type == "hosp" ~ "Tasa de hospitalización por estado de vacunación",
       input$event_type == "cases" ~ "Casos por 100,000 por día por estado de vacunación")
     
     if(input$event_agerange %in% c("all", "facet")){
       counts <- filter(counts, ageRange != "0-4" & ageRange !="5-11") %>%
         mutate(ageRange = droplevels(ageRange))} else{
           counts <- filter(counts, ageRange == input$event_agerange)
     }
     
     if(input$event_agerange == "all") counts$ageRange <- "all" 
     
     p <- counts %>% 
       filter(date >= input$event_range[1] & date <= input$event_range[2]) %>%
       filter(status %in% c("VAX", "UNV")) %>%
       mutate(outcome = !!sym(input$event_type)) %>%
       group_by(date, manu, ageRange) %>%
       summarize(outcome = sum(outcome), n=sum(n, na.rm=TRUE), .groups = "drop") %>%
       group_by(manu, ageRange) %>% 
       mutate(denom =  ma7(date, n, k = 14)$moving_avg) %>%
       mutate(rate = ma7(date, outcome, k = 14)$moving_avg/denom * 10^5) %>%
       ungroup() %>%
       filter(denom > 25) %>%
       ggplot(aes(date, rate, color = manu)) +
       geom_line(lwd = 1.5, alpha = 0.7) +
       labs(y="Tasa por día por 100,000", x="Fecha", title = the_title, 
            caption = "Basado en media móvil de 14 días")+
       scale_color_manual(
         labels = c(manu_labels[["UNV"]], manu_labels[["MOD"]], manu_labels[["PFR"]],
                    manu_labels[["JSN"]]),
         values = c(manu_colors[["UNV"]], manu_colors[["MOD"]], manu_colors[["PFR"]],
                    manu_colors[["JSN"]]), name="Vacuna:") +
       theme_bw()+
       theme(legend.position = "bottom", text = element_text(size = 15))
     
     if(input$event_agerange == "facet") p <- p + facet_wrap(~ageRange)
     
     return(p)

  })
  
  output$muertes_tabla <- DT::renderDataTable({
    load(file.path(rda_path, "dat_cases.rda"))
    
    date_name <-  paste0("date_", input$event_type)
    
    if(!input$event_agerange %in% c("all", "facet")){
      dat_cases <- filter(dat_cases, ageRange == input$event_agerange)
    } 
    
    dat_cases$cases <- TRUE
    dat_cases$date_cases <- dat_cases$date
    ret <- dat_cases %>% 
      filter(!!sym(input$event_type)) %>%
      mutate(date = !!sym(date_name)) %>%
      filter(date >= input$event_range[1] & date <= input$event_range[2]) %>%
      slice(1:1000) %>%
      mutate(days = pmax(0, ifelse(manu=="JSN", as.numeric(date-date_1), as.numeric(date - date_2)))) %>%
      mutate(days = na_if(days, 0)) %>%
      mutate(booster = case_when(booster ~ "Sí",
                                 status == "VAX" ~ "No",
                                 TRUE ~ "")) %>%
      select(date, ageRange, gender, status, manu, days, booster) %>%
      mutate(status = as.character(recode(status, UNV = "No vacunado", PAR= "Parcial", VAX="Vacunado"))) %>%
      mutate(status = ifelse(gender == "F" & status == "Vacunado", "Vacunada", status)) %>%
      mutate(status = ifelse(gender == "F" & status == "No vacunado", "No vacunada", status)) %>%
      mutate(manu = recode(as.character(manu), UNV = "", MOD = "Moderna", PFR = "Pfizer", JSN = "J & J"))
     


    make_datatable(ret, 
                   col.names = c("Fecha", "Grupo de edada", "Sexo", "Vacunación", 
                                 "Tipo de vacuna", "Días desde completar dosis", 
                                 "Booster"),
                   align = c("r", "c", "c", "c", "c", "r", "r"),
                   nowrap = 1:3)
    },
  server = FALSE)
  
  output$piramide <- renderPlot({
    
    load(file.path(rda_path, "piramide.rda"))
      
    labs <- seq(-.08, .08, 0.04)
    
    tab <- filter(piramide, municipio == input$piramide_municipio)
    
    mun <- recode(input$piramide_municipio, Todos = "Puerto Rico")
    tab %>%
      ggplot(aes(ageRange, n, fill = estatus)) +
      geom_bar(position = "stack", stat = "identity", color = I("black"), width = 1) +
      scale_y_continuous(labels= make_pct(abs(labs)), breaks = labs) + 
      ylab("Porciento de la poblacion") +
      xlab("Grupo de edad") + 
      annotate("text", x=Inf, y=Inf, label = "\nMujeres   ", vjust = 1, hjust = 1) +
      annotate("text", x=Inf, y=-Inf, label = "\n   Hombres", vjust = 1, hjust = 0) +
      coord_flip() + 
      theme_bw() +
      scale_fill_discrete(name = "Estado de vacunación") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            text = element_text(size = 15)) +
      ggtitle(paste("Pirámide poblacional de", mun))
  })
  
  output$piramide_tabla <- renderText({
    
    load(file.path(rda_path, "piramide.rda"))
    
    make_col <- function(x,n) paste0(make_pretty(x), " (", make_pct(x/n,0), ")")
    piramide_tab  %>% 
      filter(municipio == input$piramide_municipio) %>%
      mutate(onedose = make_col(onedose, poblacion),
             full = make_col(full, poblacion),
             immune = make_col(immune, poblacion),
             booster = make_col(booster, poblacion),
             poblacion = make_pretty(round(poblacion))) %>%
      select(ageRange, gender, poblacion, onedose, full, immune, booster) %>%
      kbl(col.names = c("Grupo de Edad", "Sexo", "Población", "Una dosis", "Dosis completa", "Dosis complete sin necesidad de Booster", "Con booster"),
                      align = c("c","c", rep("r", 9)))  %>%
      kable_styling()
  })
  
  output$mapa <- renderPlot({
    load(file.path(rda_path, "piramide.rda"))
    load(file.path(rda_path, "map.rda"))
    tab <- piramide_tab %>%
      filter(!municipio %in% c("Todos", "No reportados"))
    
    min_rate <- .40
    max_rate <- .70

    if(input$municipio_agerange != "all"){
      tab <- tab %>% filter(ageRange == input$municipio_agerange)
    } 
    
    tab %>%  group_by(municipio) %>%
        summarize(onedose = sum(onedose),
                  full = sum(full),
                  lost = sum(lost),
                  immune = sum(immune),
                  poblacion = sum(poblacion), .groups = "drop") %>%
     mutate(rate = full/ poblacion) %>%      
      mutate(rate = 100*pmin(pmax(rate, min_rate), max_rate)) %>%
      na.omit() %>%
      left_join(map, by = "municipio") %>%
      ggplot() + 
      geom_polygon(aes(x = X, y = Y, group = paste(municipio, part), fill = rate), color = "black", size = 0.15) + 
      geom_text(mapping = aes(x = X, y = Y, label = municipio), data = map_centers,
                size  = 2.0,
                color = "black") +
      scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(9, "Reds")),
                           name = "Por ciento con dosis completa:",
                           limits= c(100*min_rate, 100*max_rate)) +
      coord_map() +
      theme_void() +
      theme(legend.position = "bottom")
  })
  
  output$mapa_leaflet <- renderLeaflet({
    load(file.path(rda_path, "piramide.rda"))
    load(file.path(rda_path, "map.rda"))
    tab <- piramide_tab %>%
      filter(!municipio %in% c("Todos", "No reportados"))
    
    min_rate <- .40
    max_rate <- .70
    
    if(input$municipio_agerange != "all"){
      tab <- tab %>% filter(ageRange == input$municipio_agerange)
    } 
    
    pal <- colorNumeric(palette=rev(RColorBrewer::brewer.pal(9, "Reds")), 
                        domain=c(100*min_rate, 100*max_rate))
    
    map_obj <- tab %>%  group_by(municipio) %>%
      summarize(onedose = sum(onedose),
                full = sum(full),
                lost = sum(lost),
                immune = sum(immune),
                poblacion = sum(poblacion), .groups = "drop") %>%
      mutate(rate = full/ poblacion) %>%
      mutate(rate = 100*pmin(pmax(rate, min_rate), max_rate)) %>%
      na.omit() %>%
      # left_join(map, by = "municipio") %>%
      # rename(lng = X, lat = Y) %>%
      # as.data.frame()
    # data_obj %>%
    sp::merge(map_sp, ., by.x = "Municipio", by.y = "municipio")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%.0f%% con dosis completa",
      map_obj@data$Municipio, map_obj@data$rate
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(data=map_obj) %>%
    # setView(-66.25789, 18.22132, 8.1) %>%
    fitBounds(lng1=-67.27135, lat1=17.92687,
                 lng2=-65.24442, lat2=18.51576,
              options = list(padding = c(0,0))) %>%
    # addTiles(providers$CartoDB.Positron) %>%
    
    addPolygons(
      # lng = ~lng,
      # lat = ~lat,
      fillColor = ~pal(rate),
      weight = 1,
      opacity = 1,
      color = "black",
      dashArray = "",
      fillOpacity = 1.0,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 1.0,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
      addLabelOnlyMarkers(data=map_centers, lng = ~X, lat = ~Y, label = ~municipio,
                          labelOptions = labelOptions(
                            noHide = TRUE, direction = 'center',
                            textOnly = TRUE, textsize="7px")) %>%
      addLegendNumeric(pal = pal, values = map_obj$rate,
                title = "Población con dosis completa", bins=7,
                position = "bottomright", orientation='horizontal',
                numberFormat = function(x) {make_pct(x/100,0)},
                height=20, width=150)
    
      # + 
      # geom_polygon(aes(x = X, y = Y, group = paste(municipio, part), fill = rate), color = "black", size = 0.15) + 
      # geom_text(mapping = aes(x = X, y = Y, label = municipio), data = map_centers,
      #           size  = 2.0,
      #           color = "black") +
      # scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(9, "Reds")),
      #                      name = "Por ciento con dosis completa:",
      #                      limits= c(100*min_rate, 100*max_rate)) +
      # coord_map() +
      # theme_void() +
      # theme(legend.position = "bottom")
  })
  
  
  output$municipio_tabla <- DT::renderDataTable({
    load(file.path(rda_path, "piramide.rda"))
    
    tab <- piramide_tab %>%
      filter(!municipio %in% c("Todos","No reportados"))
    
    
    if(input$municipio_agerange != "all"){
      tab <- tab %>% filter(ageRange == input$municipio_agerange)
    } 
    
    make_col <- function(x,n) paste0(make_pretty(x), " (", make_pct(x/n,0), ")")
    
    tab  %>%  group_by(municipio) %>%
      summarize(onedose = sum(onedose),
                full = sum(full),
                booster = sum(booster),
                lost = sum(lost),
                immune = sum(immune),
                poblacion = sum(poblacion), .groups = "drop") %>%
      arrange(full/poblacion) %>% 
      mutate(oonedose = onedose/poblacion,
             onedose = make_col(onedose, poblacion),
             ofull = full/poblacion,
             full = make_col(full, poblacion),
             oimmune = immune/poblacion,
             immune = make_col(immune, poblacion),
             obooster = booster/poblacion,
             booster = make_col(booster, poblacion),
             opoblacion = poblacion,
             poblacion = make_pretty(round(poblacion))) %>%
      select(municipio, poblacion, onedose, full, immune, booster,
             opoblacion, oonedose, ofull, oimmune, obooster) %>%
      setNames(c("Municipio", "Población", "Una dosis", "Dosis completa", "Dosis complete sin necesidad de Booster", "Con booster",
                 "opoblacion", "oonedose", "ofull", "oimmune", "obooster"))%>%
      DT::datatable(rownames = FALSE,
                    options = list(dom = 't', pageLength = -1,
                                   columnDefs = list(list(targets = 1, orderData = 6),
                                                     list(targets = 2, orderData = 7),
                                                     list(targets = 3, orderData = 8),
                                                     list(targets = 4, orderData = 9),
                                                     list(targets = 5, orderData = 10),
                                                     list(targets = 6:10, visible = FALSE),
                                                     list(className = 'dt-left', targets = 0),
                                                     list(className = 'dt-right', targets = 1:5)))) %>%
      DT::formatStyle(1, "white-space" = "nowrap")
  }, 
  server = FALSE)
  
  output$proveedores <- renderText({
    load(file.path(rda_path,"proveedores.rda"))

    if(input$proveedor_agerange != "all") proveedores <- filter(proveedores, ageRange == input$proveedor_agerange)
    if(input$proveedor_manu != "all") proveedores <- filter(proveedores, manu == input$proveedor_manu)
    if(input$proveedor_dose != "all") proveedores <- filter(proveedores, dose == input$proveedor_dose)
    
    proveedores %>% 
      group_by(proveedor) %>%
      summarize(rezago = round(sum(total*rezago, na.rm=TRUE)/sum(total, na.rm=TRUE)),
                total = sum(total),
                rezago_esta_semana = round(sum(entradas_esta_semana*rezago_esta_semana, na.rm=TRUE)/sum(entradas_esta_semana,na.rm=TRUE)),
                entradas_esta_semana = sum(entradas_esta_semana)) %>%
      mutate(o = ifelse(proveedor == "Otros", -Inf, total)) %>%  
      arrange(desc(o)) %>%
      select(-o) %>%
      mutate(total = make_pretty(total), entradas_esta_semana = make_pretty(entradas_esta_semana)) %>%
      select(proveedor, total, rezago, entradas_esta_semana, rezago_esta_semana) %>%
      setNames(c("Proveedor", "Vacunas administradas", "Rezago medio (días)", "Entradas última semana","Rezago última semana")) %>% 
      kableExtra::kbl(align = c("l","r", "r","r", "r", "r"))  %>% 
      kableExtra::kable_styling()
  })
  output$tabla <- DT::renderDataTable({
    load(file.path(rda_path, "counts.rda"))
    
    ret <- counts %>% filter(date >= input$tabla_range[1] & date <= input$tabla_range[2]) %>%
      group_by(date, manu, status) %>%
      summarize(cases = sum(cases), hosp = sum(hosp), 
                death = sum(death), n=sum(n, na.rm=TRUE), .groups = "drop") %>%
      group_by(manu, status) %>%
      mutate(cases_rate = ma7(date, cases)$moving_avg/ma7(date, n)$moving_avg * 10^5,
             hosp_rate = ma7(date, hosp)$moving_avg/ma7(date, n)$moving_avg * 10^5,
             death_rate = ma7(date, death)$moving_avg/ma7(date, n)$moving_avg * 10^5) %>%
      ungroup() %>%
      arrange(desc(date), status, manu) %>%
      mutate(status = recode(status, UNV = "No vacunados", PAR= "Parcial", VAX="Vacunados")) %>%
      mutate(manu = recode(as.character(manu), UNV = "", MOD = "Moderna", PFR = "Pfizer", JSN = "J & J")) %>%
      mutate(n = make_pretty(round(n)),
             cases_rate = digits(cases_rate),
             hosp_rate =digits(hosp_rate),
             death_rate = digits(death_rate)) %>%
      select(date, status, manu, n, cases_rate, hosp_rate, death_rate, cases, hosp, death)
    
    make_datatable(ret, align = c("r","c","c", rep("r",7)), nowrap = 1:3,
                   col.names = c("Fecha", "Vacunación", "Tipo de vacuna", "Población",
                                 "Casos (tasa) ", "Hosp (tasa)", "Muertes (tasa)",
                                 "Casos", "Hosp", "Muertes"))
  },
  server = FALSE
)
  output$people_plot <- renderPlot({
    load(file.path(rda_path, "total_daily_vax_counts.rda"))
    
    the_title <- case_when(
      input$status_type == "full" ~ "Cantidad de personas que completan serie de vacunación por día",
      input$status_type == "onedose" ~ "Cantidad de personas con una dosis por día",
      input$status_type == "booster" ~ "Cantidad de personas con dosis de refuerzo por día", 
      input$status_type == "immunized" ~ "Cantidad de personas sin necesidad de dosis de refuerzo por día")
    
    if(input$manu_type=="all"){
      total_daily_vax_counts
      labels_manu <- c( manu_labels[["JSN"]], manu_labels[["MOD"]],
                        manu_labels[["PFR"]])
      values_manu <- c(manu_colors[["JSN"]], manu_colors[["MOD"]],
                       manu_colors[["PFR"]])
      fill_name = "Vacunas:"
      legend_pstn = "bottom"
    } else if (input$manu_type =="together"){
      total_daily_vax_counts$manu <- "together"
      labels_manu <- c("Vacunas agregadas: Pfizer, Moderna y J&J")
      values_manu <- c("darkgray")
      fill_name = NULL
      legend_pstn = "none"
    } else {
      total_daily_vax_counts <- total_daily_vax_counts %>% filter(manu %in% c(input$manu_type))
      labels_manu <- c( manu_labels[[input$manu_type]])
      values_manu <- c(manu_colors[[input$manu_type]])
      fill_name = "Vacunas:"
      legend_pstn = "bottom"
    }
    
    if(input$dose_agerange %in% c("all", "facet")){
      total_daily_vax_counts <- filter(total_daily_vax_counts, ageRange != "0-4") %>%
        mutate(ageRange = droplevels(ageRange))} else{
          total_daily_vax_counts <- filter(total_daily_vax_counts, ageRange == input$dose_agerange)
        }
    
    if(input$dose_agerange == "all") { total_daily_vax_counts$ageRange<-"all"
    
    if(input$time_type == "daily") {
      
      tmp <- total_daily_vax_counts %>%
        filter(date >=input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
        filter(status_type == input$status_type) %>%
        mutate(outcome = !!sym(input$time_type)) %>%
        group_by(date) %>%
        summarize(outcome2 = sum(outcome),.groups = "drop") %>%
        mutate(rate =  ma7(date, outcome2, k = 14)$moving_avg) 
      
      g <- total_daily_vax_counts %>%
        filter(date >= input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
        filter(status_type == input$status_type) %>%
        mutate(outcome = !!sym(input$time_type)) %>%
        full_join(tmp, by = "date") %>%
        ggplot(aes(x=date, y=outcome, fill=manu))+ geom_col()+
        geom_line(aes(y=rate), size = 1)+
        scale_y_continuous(labels = scales::comma)+
        labs(x="Fecha", y="Cantidad de personas", title = the_title, 
             caption = "Curva (en negro) es la media móvil de 14 días")+
        scale_fill_manual(
          labels = labels_manu,
          values = values_manu, 
          name=fill_name) +
        theme_bw()+
        theme(legend.position = legend_pstn, text = element_text(size = 15))
    } else {
      g <- total_daily_vax_counts %>%
        filter(date >= input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
        filter(status_type == input$status_type) %>%
        mutate(outcome = !!sym(input$time_type)) %>%
        ggplot(aes(x=date, y=outcome, fill=manu))+ 
        geom_col()+
        scale_y_continuous(labels = scales::comma)+
        labs(x="Fecha", y="Cantidad de personas", title = the_title)+
        scale_fill_manual(
          labels = labels_manu,
          values = values_manu, 
          name=fill_name) +
        theme_bw()+
        theme(legend.position = legend_pstn, text = element_text(size = 15))
    } 
    return(g)
    } 
    if(input$dose_agerange == "facet") 
    {      if(input$time_type == "daily") {
      
      tmp <- total_daily_vax_counts %>%
        filter(date >=input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
        filter(status_type == input$status_type) %>%
        group_by(date, ageRange) %>%
        mutate(outcome = !!sym(input$time_type)) %>%
        summarize(outcome2 = sum(outcome),.groups = "drop") %>%
        group_by(ageRange) %>%
        mutate(rate =  ma7(date, outcome2, k = 14)$moving_avg) %>% 
        ungroup()
      
      g <- total_daily_vax_counts %>%
        filter(date >= input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
        filter(status_type == input$status_type) %>%
        mutate(outcome = !!sym(input$time_type)) %>%
        full_join(tmp) %>%
        ggplot(aes(x=date, y=outcome, fill=manu))+ geom_col()+
        geom_line(aes(y=rate), size = 1)+ facet_wrap(~ageRange)+
        scale_y_continuous(labels = scales::comma)+
        labs(x="Fecha", y="Cantidad de personas", title = the_title, 
             caption = "Curva (en negro) es la media móvil de 14 días")+
        scale_fill_manual(
          labels = labels_manu,
          values = values_manu, 
          name=fill_name) +
        theme_bw()+
        theme(legend.position = legend_pstn, text = element_text(size = 15))
    } else {
      g <- total_daily_vax_counts %>%
        filter(date >= input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
        filter(status_type == input$status_type) %>%
        mutate(outcome = !!sym(input$time_type)) %>%
        ggplot(aes(x=date, y=outcome, fill=manu))+ geom_col()+ facet_wrap(~ageRange)+
        scale_y_continuous(labels = scales::comma)+
        labs(x="Fecha", y="Cantidad de personas", title = the_title)+
        scale_fill_manual(
          labels = labels_manu,
          values = values_manu, 
          name=fill_name) +
        theme_bw()+
        theme(legend.position = legend_pstn, text = element_text(size = 15))
    }
      return(g)
    }
    
    else {
      if(input$time_type == "daily") {
        
        tmp <- total_daily_vax_counts %>%
          filter(date >=input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
          filter(status_type == input$status_type) %>%
          mutate(outcome = !!sym(input$time_type)) %>%
          group_by(date) %>%
          summarize(outcome2 = sum(outcome),.groups = "drop") %>%
          mutate(rate =  ma7(date, outcome2, k = 14)$moving_avg) 
        
        g <- total_daily_vax_counts %>%
          filter(date >= input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
          filter(status_type == input$status_type) %>%
          mutate(outcome = !!sym(input$time_type)) %>%
          full_join(tmp) %>%
          ggplot(aes(x=date, y=outcome, fill=manu))+ geom_col()+
          geom_line(aes(y=rate), size = 1)+
          scale_y_continuous(labels = scales::comma)+
          labs(x="Fecha", y="Cantidad de personas", title = the_title, 
               caption = "Curva (en negro) es la media móvil de 14 días")+
          scale_fill_manual(
            labels = labels_manu,
            values = values_manu, 
            name=fill_name) +
          theme_bw()+
          theme(legend.position = legend_pstn, text = element_text(size = 15))
      } else {
        g <- total_daily_vax_counts %>%
          filter(date >= input$dose_timerange[1] & date <= input$dose_timerange[2]) %>%
          filter(status_type == input$status_type) %>%
          mutate(outcome = !!sym(input$time_type)) %>%
          ggplot(aes(x=date, y=outcome, fill=manu))+ geom_col()+
          scale_y_continuous(labels = scales::comma)+
          labs(x="Fecha", y="Cantidad de personas", title = the_title)+
          scale_fill_manual(
            labels = labels_manu,
            values = values_manu, 
            name=fill_name) +
          theme_bw()+
          theme(legend.position = legend_pstn, text = element_text(size = 15))
      } 
      return(g)
    }
  })
  
}

shinyApp(ui = ui, server = server)

                                       
                                


                