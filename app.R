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
  titlePanel("Informe de vacunas COVID-19 en Puerto Rico"),
  
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
                           selectInput("event_scale",
                                       "Escala",choice = c("Lineal" = "linear",
                                                           "Logarítimica" = "log"),
                                       selected = "linear"),
                           width = 3),
                         mainPanel(
                            plotOutput("muertes_plot"),
                            DT::dataTableOutput("muertes_tabla"))
                         )),
              
              tabPanel("Gráficas",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("graficas_type",
                                       "Tipo de gráfico",
                                       choice = c(Acumulado = "total",
                                                  Diario = "daily"),
                                       selected = "total"),
                           selectInput("graficas_dose",
                                       "Estado de vacunación",
                                       choice = c(`Una dosis` = "onedose",
                                                  `Completa` = "full",
                                                  `Booster` = "booster", 
                                                  `Sin necesidad de booster` = "immune"),
                                       selected = "full"),
                           selectInput("graficas_manu",
                                       "Tipo de vacuna",
                                       choice = c(`Agregadas` = "all",
                                                  `Todas` = "facet", 
                                                  `Pfizer` = "PFR",
                                                  `Moderna` = "MOD", 
                                                  `J&J` = "JSN"),
                                       selected = "all"), 
                           selectInput("graficas_agerange",
                                       "Grupo de Edad",
                                       choice = c("Agregados" = "all",
                                                  "Todos" = "facet",
                                                  rev(age_levels[-1])),
                                       selected = "all"),
                           dateRangeInput("graficas_range", 
                                          "Periodo", 
                                          start = last_day - days(240),
                                          end = last_day,
                                          format = "M-dd-yyyy",
                                          language = "es",
                                          width = "100%",
                                          min = first_day,
                                          max = last_day),
                           selectInput("graficas_tasa",
                                       "Tasa o total",
                                       choice = c("Tasa" = "tasa",
                                                  "Total" = "total"),
                                       selected = "tasa"),
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
                           leafletOutput("mapa_leaflet_quantiles"),
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
                       )),
                       tabPanel("FAQ",
                                htmlOutput("importantInfo")
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
      mutate(status = recode(status, UNV = "No vacunados", PAR= "Parcial", VAX="Vacunados sin booster", BST = "Vacunados con booster")) %>%
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
     
     the_k <- case_when(input$event_type == "death" ~ 60,
                        input$event_type == "hosp" ~ 30,
                        input$event_type == "cases" ~ 14)
     p <- counts %>% 
       filter(status %in% c("VAX", "UNV", "BST")) %>%
       filter(!(status == "BST" & manu == "JSN")) %>%
       mutate(outcome = !!sym(input$event_type)) %>%
       group_by(date, status, manu, ageRange) %>%
       summarize(outcome = sum(outcome), n=sum(n, na.rm=TRUE), .groups = "drop") %>%
       group_by(status, manu, ageRange) %>% 
       mutate(denom =  ma7(date, n, k = the_k)$moving_avg) %>%
       mutate(rate = ma7(date, outcome, k = the_k)$moving_avg/denom * 10^5) %>%
       ungroup() %>%
       filter(date >= input$event_range[1] & date <= input$event_range[2]) %>%
       filter(denom > 10000) %>%
       mutate(Booster = factor(ifelse(status != "BST",  "Sin", "Con"), levels = c("Sin", "Con"))) %>%
       ggplot(aes(date, rate, color = manu, lty = Booster)) +
       geom_line(lwd = 1.25, alpha = 0.7) +
       labs(y="Tasa por día por 100,000", x="Fecha", title = the_title, 
            caption = paste("Basado en media móvil de", the_k,"días")) +
       scale_color_manual(
         labels = c(manu_labels[["UNV"]], manu_labels[["MOD"]], manu_labels[["PFR"]],
                    manu_labels[["JSN"]]),
         values = c(manu_colors[["UNV"]], manu_colors[["MOD"]], manu_colors[["PFR"]],
                    manu_colors[["JSN"]]), name="Vacuna:") +
       theme_bw()+
       theme(legend.position = "bottom", text = element_text(size = 15)) +
       scale_x_date(date_labels = "%b", breaks = scales::breaks_width("1 month"))
     
     if(input$event_agerange == "facet") p <- p + facet_wrap(~ageRange)
     if(input$event_scale == "log") p <- p + scale_y_continuous(trans = "log2")
     
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
      mutate(booster = case_when(status == "BST" ~ "Sí",
                                 status == "VAX" ~ "No",
                                 TRUE ~ "")) %>%
      select(date, ageRange, gender, status, manu, days, booster) %>%
      mutate(status = as.character(recode(status, UNV = "No vacunado", PAR= "Parcial", VAX="Vacunado", BST = "Vacunado"))) %>%
      mutate(status = ifelse(gender == "F" & status == "Vacunado", "Vacunada", status)) %>%
      mutate(status = ifelse(gender == "F" & status == "No vacunado", "No vacunada", status)) %>%
      mutate(manu = recode(as.character(manu), UNV = "", MOD = "Moderna", PFR = "Pfizer", JSN = "J & J"))
     


    make_datatable(ret, 
                   col.names = c("Fecha", "Grupo de edad", "Sexo", "Vacunación", 
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
      mutate(faltan = make_pretty(round(pmax(0, poblacion - full))),
             onedose = make_col(onedose, poblacion),
             full = make_col(full, poblacion),
             immune = make_col(immune, poblacion),
             booster = make_col(booster, poblacion),
             poblacion = make_pretty(round(poblacion))) %>%
      select(ageRange, gender, poblacion, onedose, full, faltan, immune, booster) %>%
      kbl(col.names = c("Grupo de Edad", "Sexo", "Población", "Una dosis", "Dosis completa", "Faltan", 
                        "Dosis completa sin necesidad de Booster", "Con booster"),
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
    
    if(input$municipio_agerange != "all"){
      tab <- tab %>% filter(ageRange == input$municipio_agerange)
    } 
    
    
    
    map_obj <- tab %>%  group_by(municipio) %>%
      summarize(onedose = sum(onedose),
                full = sum(full),
                lost = sum(lost),
                immune = sum(immune),
                poblacion = sum(poblacion), .groups = "drop") %>%
      mutate(rate = full/ poblacion) %>%
      mutate(rate = 100*rate) %>%
      # mutate(rate = 100*pmin(pmax(rate, min_rate), max_rate)) %>%
      na.omit() %>%
      # left_join(map, by = "municipio") %>%
      # rename(lng = X, lat = Y) %>%
      # as.data.frame()
    # data_obj %>%
    sp::merge(map_sp, ., by.x = "Municipio", by.y = "municipio")
    
    # min_rate <- .60
    # max_rate <- .80
    
    # min_data_rate <- min(map_obj@data$rate)
    # max_data_rate <- max(map_obj@data$rate)
    
    sorted_by_rate <- map_obj@data[order(map_obj@data$rate),]
    # ggplot(sorted_by_rate, aes(1:length(rate), rate)) +
    #   geom_point()
    min_rate <- sorted_by_rate[5,"rate"] / 100
    max_rate <- sorted_by_rate[78-10, "rate"] / 100
    
    pal <- colorNumeric(palette=rev(RColorBrewer::brewer.pal(9, "Reds")),
                        domain=c(100*min_rate, 100*max_rate))
    # pal <- colorNumeric(palette=rev(RColorBrewer::brewer.pal(9, "Reds")), 
    #                     domain=c(min_data_rate, max_data_rate))
    
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
      fillColor = ~pal(pmin(pmax(rate, 100*min_rate), 100*max_rate)),
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
      addLegendNumeric(pal = pal, values = c(min_rate*100,max_rate*100),
                title = "Población con dosis completa", bins=3.0,
                position = "bottomright", orientation='horizontal',
                numberFormat = function(x) {
                  paste(
                    ifelse(x == 100*min_rate,"<",">"),
                    make_pct(x/100,0))
                },
                height=20, width=150)
    
  })
  
  output$mapa_leaflet_quantiles <- renderLeaflet({
    load(file.path(rda_path, "piramide.rda"))
    load(file.path(rda_path, "map.rda"))
    tab <- piramide_tab %>%
      filter(!municipio %in% c("Todos", "No reportados"))
    
    if(input$municipio_agerange != "all"){
      tab <- tab %>% filter(ageRange == input$municipio_agerange)
    } 
    
    
    
    map_obj <- tab %>%  group_by(municipio) %>%
      summarize(onedose = sum(onedose),
                full = sum(full),
                lost = sum(lost),
                immune = sum(immune),
                poblacion = sum(poblacion), .groups = "drop") %>%
      mutate(rate = full/ poblacion) %>%
      mutate(rate = 100*rate) %>%
      # mutate(rate = 100*pmin(pmax(rate, min_rate), max_rate)) %>%
      na.omit() %>%
      # left_join(map, by = "municipio") %>%
      # rename(lng = X, lat = Y) %>%
      # as.data.frame()
      # data_obj %>%
      sp::merge(map_sp, ., by.x = "Municipio", by.y = "municipio")
    
    # min_rate <- .60
    # max_rate <- .80
    
    # min_data_rate <- min(map_obj@data$rate)
    # max_data_rate <- max(map_obj@data$rate)
    
    sorted_by_rate <- map_obj@data[order(map_obj@data$rate),]
    # ggplot(sorted_by_rate, aes(1:length(rate), rate)) +
    #   geom_point()
    
    nBins <- 3
    bins <- quantile(sorted_by_rate$rate, probs=seq(0,1, 1.0/nBins)) %>% round() %>% unname()
    pal <- colorBin(rev(RColorBrewer::brewer.pal(nBins, "Reds")), domain = sorted_by_rate$rate, bins = bins)
    # min_rate <- sorted_by_rate[5,"rate"] / 100
    # max_rate <- sorted_by_rate[78-10, "rate"] / 100
    
    # pal <- colorNumeric(palette=rev(RColorBrewer::brewer.pal(9, "Reds")),
    #                     domain=c(100*min_rate, 100*max_rate))
    # pal <- colorNumeric(palette=rev(RColorBrewer::brewer.pal(9, "Reds")), 
    #                     domain=c(min_data_rate, max_data_rate))
    
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
      addLegend(pal = pal, values = ~rate, opacity = 1.0, title = "Población con dosis completa",
                    position = "bottomright", labFormat = labelFormat(suffix='%'))
      # addLegendNumeric(pal = pal, values = c(min_rate*100,max_rate*100),
      #                  title = "Población con dosis completa", bins=3.0,
      #                  position = "bottomright", orientation='horizontal',
      #                  numberFormat = function(x) {
      #                    paste(
      #                      ifelse(x == 100*min_rate,"<",">"),
      #                      make_pct(x/100,0))
      #                  },
      #                  height=20, width=150)
    
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
      mutate(ofaltan = poblacion - full,
             faltan = make_pretty(round(pmax(0, poblacion - full))),
             oonedose = onedose/poblacion,
             onedose = make_col(onedose, poblacion),
             ofull = full/poblacion,
             full = make_col(full, poblacion),
             oimmune = immune/poblacion,
             immune = make_col(immune, poblacion),
             obooster = booster/poblacion,
             booster = make_col(booster, poblacion),
             opoblacion = poblacion,
             poblacion = make_pretty(round(poblacion))) %>%
      select(municipio, poblacion, onedose, full, faltan, immune, booster,
             opoblacion, oonedose, ofull, ofaltan, oimmune, obooster) %>%
      setNames(c("Municipio", "Población", "Una dosis", "Dosis completa", "Faltan", "Dosis complete sin necesidad de Booster", "Con booster",
                 "opoblacion", "oonedose", "ofull", "ofaltan", "oimmune", "obooster"))%>%
      DT::datatable(rownames = FALSE,
                    options = list(dom = 't', pageLength = -1,
                                   columnDefs = list(list(targets = 1, orderData = 7),
                                                     list(targets = 2, orderData = 8),
                                                     list(targets = 3, orderData = 9),
                                                     list(targets = 4, orderData = 10),
                                                     list(targets = 5, orderData = 11),
                                                     list(targets = 6, orderData = 12),
                                                     list(targets = 7:12, visible = FALSE),
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
      mutate(status = recode(status, UNV = "No vacunados", PAR= "Parcial", VAX="Vacunados sin booster", BST = "Vacunado con booster")) %>%
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
    
    load(file.path(rda_path, "daily_vax_counts.rda"))
    load(file.path(rda_path, "population-tabs.rda"))
    
    the_title <- case_when(
      input$graficas_dose == "full" ~ "Cantidad de personas que completan serie de vacunación por día",
      input$graficas_dose == "onedose" ~ "Cantidad de personas con una dosis por día",
      input$graficas_dose == "booster" ~ "Cantidad de personas con dosis de refuerzo por día", 
      input$graficas_dose == "immune" ~ "Cantidad de personas sin necesidad de dosis de refuerzo por día")
    
    show_legend <- TRUE
    if(input$graficas_manu=="facet"){
      labels_manu <- c(manu_labels[["JSN"]], manu_labels[["MOD"]],
                        manu_labels[["PFR"]])
      values_manu <- c(manu_colors[["JSN"]], manu_colors[["MOD"]],
                       manu_colors[["PFR"]])
      fill_name = "Vacunas:"
      legend_pstn = "bottom"
    } else{
      if (input$graficas_manu =="all"){
        labels_manu <- c("Vacunas agregadas: Pfizer, Moderna y J&J")
        values_manu <- c("darkgray")
        fill_name = NULL
        legend_pstn = "none"
        show_legend <- FALSE
      } else {
        labels_manu <- c(manu_labels[[input$graficas_manu]])
        values_manu <- c(manu_colors[[input$graficas_manu]])
        fill_name = "Vacunas:"
        legend_pstn = "bottom"
      }
    }
    
    daily_vax_counts <- daily_vax_counts %>%
      filter(ageRange != "0-4") %>%
      mutate(ageRange = droplevels(ageRange)) %>%
      mutate(immune = full - lost) %>%
      group_by(date, ageRange, manu) %>% 
      summarize(across(where(is.numeric), sum), .groups = "drop")
    
    pop_by_age_gender <- filter(pop_by_age_gender, ageRange != "0-4") %>%
      mutate(ageRange = droplevels(ageRange)) %>%
      group_by(ageRange) %>% 
      summarize(across(where(is.numeric), sum), .groups = "drop")
    
    if(input$graficas_agerange == "all"){
      daily_vax_counts <- daily_vax_counts %>% group_by(date, manu) %>% summarize(across(where(is.numeric), sum), .groups = "drop") %>% mutate(ageRange = "Todos")
      pop_by_age_gender <- pop_by_age_gender %>%  summarize(poblacion = sum(poblacion),  .groups ="drop")  %>%
        mutate(ageRange = "Todos")
    } else{
      if(input$graficas_agerange != "facet"){
        daily_vax_counts <- daily_vax_counts %>% filter(ageRange == input$graficas_agerange)
      }
    }
    
    if(input$graficas_manu == "all"){
      daily_vax_counts <- daily_vax_counts %>% group_by(date, ageRange) %>% summarize(across(where(is.numeric), sum), .groups = "drop") %>% mutate(manu = "Todos")
    } else{
      if(input$graficas_manu != "facet"){
        daily_vax_counts <- daily_vax_counts %>% filter(manu == input$graficas_manu)
      }
    }
    
   if(input$graficas_type == "total")  daily_vax_counts <- daily_vax_counts %>% group_by(ageRange, manu) %>% mutate(across(where(is.numeric), cumsum))
    
    if(input$graficas_tasa == "tasa"){
      daily_vax_counts <- daily_vax_counts %>% 
        left_join(pop_by_age_gender, by = "ageRange") %>% 
        mutate(across(where(is.numeric), ~ .x / poblacion))
    }

    if(input$graficas_type=="daily"){
      daily_vax_counts  <- daily_vax_counts %>%
        mutate(outcome = !!sym(input$graficas_dose)) %>%
        group_by(ageRange, manu) %>%
        mutate(moving_avg = ma7(date, outcome)$moving_avg) %>%
        ungroup()
    }
    
    if (input$graficas_agerange =="12-17") {
    daily_vax_counts <- daily_vax_counts %>%
      filter(date >=  ifelse(input$graficas_range[1]< make_date(2021,05,12), make_date(2021,05,12), input$graficas_range[1])  & date <= input$graficas_range[2]) 
    } else if (input$graficas_agerange =="5-11"){
        daily_vax_counts <- daily_vax_counts %>%
          filter(date >= ifelse(input$graficas_range[1]< make_date(2021,11,04), make_date(2021,11,04), input$graficas_range[1]) & date <= input$graficas_range[2]) 
      }
     else {
      daily_vax_counts <- daily_vax_counts %>%
        filter(date >= input$graficas_range[1] & date <= input$graficas_range[2]) 
    }
      
    
    p <- daily_vax_counts %>%
      mutate(outcome = !!sym(input$graficas_dose)) %>%
      ggplot(aes(x=date, y=outcome, fill=manu)) + 
      geom_col(position = "dodge", show.legend = show_legend, color = NA) +  
      xlab("Fecha") +
      scale_fill_manual(
        labels = labels_manu,
        values = values_manu, 
        name = fill_name) +
      theme_bw() +
      ggtitle(the_title)
    
    if(input$graficas_type=="daily"){
      p <- p + geom_line(aes(y=moving_avg, color = manu), size = 1, show.legend = FALSE) +
        scale_color_manual(values = values_manu, guide = "none")
    }
    
    if(input$graficas_agerange == "facet") p <- p + facet_wrap(~ageRange)
    
    if(input$graficas_tasa=="tasa"){
      p <- p + scale_y_continuous(labels = scales::percent) + 
        ylab("Por ciento de la población") 
    }else{
      p <- p + scale_y_continuous(labels = scales::comma) + 
        ylab("Número de individuos") 
    } 
    p    
  })
  
  output$importantInfo <- renderText({
    " <h3> Información importante sobre el manejo de datos en este dashboard </h3>
  <br/> 

<b> Nota: </b> Los datos presentados en este dashboard pueden cambiar según pasa el tiempo pues provienen de una base de datos viva sujeta a ediciones según nueva información se hace disponible. Además, como parte del proceso de organización y limpieza de datos, los analistas encargados de este dashboard pueden hacer modificaciones que entiendan pertinentes para mejorar la calidad de la información que aquí se presenta. 

<br/> <br/>
  
<h4> Definiciones importantes </h4>
<ul><li> Vacunas administradas: suma de todas las primeras dosis, segundas dosis, y dosis de refuerzo administradas en Puerto Rico. 
<br/> 
</li><li> Personas con por lo menos una dosis: suma de todas las personas que se han puesto al menos una dosis. Note que todas las personas con más de una dosis están contempladas en esta suma. 
<br/> 
</li><li> Personas con dosis completa: suma de todas las personas que se han puesto dos dosis de Pfizer o Moderna o una de Janssen y han transcurrido 14 días desde su última dosis. 
<br/> 
</li><li> Personas con dosis completa sin necesidad de booster: suma de personas que con dosis completa que aún no necesitan dosis de refuerzo porque no han transcurrido 6 meses desde su segunda dosis de Moderna o Pfizer o 2 meses desde su primera dosis de Janssen. 
<br/> 
</li><li> Personas con boosters: suma de personas que completaron sus dosis de vacunación y se administraron su dosis de refuerzo. 
<br/> 
</li><li> Personas con dosis completa con necesidad de booster: suma de personas que ya completaron sus dosis de vacunación pero han transcurrido 6 meses (Pfizer o Moderna) o 2 meses (Janssen) y no se han puesto su dosis de refuerzo. 
<br/> 
</li><li> Menores (5-11 años) con por lo menos 1 dosis: suma de personas entre 5-11 años que cuentan con al menos una dosis. 
<br/> 
</li><li> Menores (5-11 años) con dosis completa:  suma de personas entre 5-11 años que cuentan con sus dos dosis y han transcurrido 14 días desde su última dosis. 
<br/> 
</li><li> Vacunación <i>parcial</i> se refiere a aquellas personas que han iniciado su serie de vacunación pero aún no se consideran como vacunados por no haber transcurrido 14 días luego de su última dosis requerida. 
<br/>
</li><li> <i>Vacunados</i> son aquellas personas que han completado su serie de vacunación: han transcurrido 14 días luego de su última dosis requerida.
<br/>
</li><li> Las personas con booster son aquellas personas que se han puesto sus dosis de refuerzo y han transcurrido 14 días desde la dosis de refuerzo. 
</li></ul>

<h4> Sobre el cálculo de tasas </h4>

Para calcular las tasas de cada evento por estado de vacunación y grupo de edad es necesario realizar un pareo entre la base de datos del BioPortal y la base de datos del PREIS. Este proceso carece de un identificador único común entre las bases, por lo que se ha desarrollado un algoritmo que realiza el pareo utilizando variables disponibles. Al momento, hemos descubierto que el algoritmo es 99% efectivo en identificar correctamente a las personas. Naturalmente, existen rezagos de entrada de datos en ambas bases, que convergen en el pareo y resulta en que las tasa de días recientes cambien mientras entran los datos. 
  
<h4> Estimados poblacionales </h4>

Los estimados poblacionales que se usan en este dashboard provienen de un ajuste que se realiza utilizando el estimado poblacional del CENSO Decenal 2020 y los estimados poblacionales (por grupo edad, municipio y sexo) de ACS 2019. La razón para realizar el ajuste es porque se entiende que los estimados del ACS 2019 no capturan la realidad de la población puertorriqueña del 2020-2021 y se ha anunciado que el <i> U.S. Census Bureau </i> no estará publicando los estimados del ACS 2020 para Puerto Rico. Para estudiar el código de este proceso puede accesar: <a> https://github.com/rafalab/vacunaspr/blob/main/wrangle-population-data.R </a>. 
    
    
    "})
}

shinyApp(ui = ui, server = server)

                                       
                                


                