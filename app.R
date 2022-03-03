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
  
  #titlePanel("Informe de vacunas COVID-19 en Puerto Rico"))
  # Application title
  titlePanel("Informe de vacunas COVID-19 en Puerto Rico"),

  tabsetPanel(id = "tabs",
              tabPanel("Resumen",
                       htmlOutput("fecha"),
                       p("Los datos presentados en este dashboard han sido revisados. Por favor visite la ventana de FAQ para más detalles."), 
                       htmlOutput("summary_1"),
                       h4("Resumen de casos, hospitalizaciones y muertes"),
                       p("Los tamaños de los grupos son diferentes por lo cual no es informativo comparar totales sino las tasas (por 100K por día). Los totales no incluyen personas que han dado positivo a una prueba diagnóstica en los últimos 90 días. También tome en cuenta que los datos de vacunas tienen mayor rezago que los de la pruebas."),
                       radioButtons("summary_type",
                                    label = "",
                                    choices = list("Sencillo" = "simple",
                                                   "Detallado" = "detail"),
                                    selected = "simple",
                                    inline = TRUE),
                       htmlOutput("titulo_3"),
                       htmlOutput("summary_3"),
                       htmlOutput("titulo_2"),
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
                                          end = last_day_counts,
                                          format = "M-dd-yyyy",
                                          language = "es",
                                          width = "100%",
                                          min = first_day,
                                          max = last_day_counts),
                           selectInput("event_agerange",
                                       "Grupo de Edad",
                                       choice = c("Agregados" = "all",
                                                  "Todos" = "facet",
                                                  rev(collapsed_age_levels[-1])),
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
                                                  "Segunda" = "Segunda",
                                                  "Tercera" = "Tercera"),
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
                                          start = last_day - days(30),
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
  
  
  output$titulo_2 <- renderText({
    load(file.path(rda_path, "dates.rda"))
    paste("<p>Mes acabando", 
          format(last_day_counts, "%B %d, %Y:"), "</p>")
    })

  # output$titulo_3 <- renderText({
  #   load(file.path(rda_path, "dates.rda"))
  #   paste("<p>Semana acabando", 
  #         format(last_day_counts-weeks(1), "%B %d, %Y"), " (datos parciales):</p>")})
  
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
    if(input$summary_type == "detail"){
      make_outcome_tab(outcome_tab_details, details = TRUE)
    } else{
      make_outcome_tab(outcome_tab, details = FALSE)
    }
  })
  # output$summary_3 <- renderText({
  #   load(file.path(rda_path, "tabs.rda"))
  #   
  #   if(input$summary_type == "detail"){
  #     make_outcome_tab(outcome_tab_details, complete = FALSE, details = TRUE)
  #   } else{
  #     make_outcome_tab(outcome_tab, complete = FALSE, details = FALSE)
  #   }
  # })
  
  
   output$muertes_plot <- renderPlot({
     load(file.path(rda_path, "daily_counts.rda"))
     load(file.path(rda_path, "dates.rda"))
     
     daily_counts <- daily_counts %>% filter(outcome==input$event_type)
     
     the_title <- case_when(
       input$event_type == "death" ~ "Tasa de mortalidad por estado de vacunación",
       input$event_type == "hosp" ~ "Tasa de hospitalización por estado de vacunación",
       input$event_type == "cases" ~ "Casos por 100,000 por día por estado de vacunación")
     
     if(!input$event_agerange %in% c("all", "facet")){
       daily_counts <- filter(daily_counts, ageRange == input$event_agerange)
       the_title <- paste(the_title, "para el grupo de edad", input$event_agerange, "años")
     }
     
     if(input$event_agerange == "all") daily_counts$ageRange <- "all" 
     
     the_k <- case_when(input$event_type == "death" ~ 30,
                        input$event_type == "hosp" ~ 30,
                        input$event_type == "cases" ~ 14)
     tmp <- daily_counts %>% 
       filter(status != "Vacunación parcial") %>%
       group_by(date, status, manu, ageRange) %>%
       summarize(obs = sum(obs), n=sum(poblacion, na.rm=TRUE), .groups = "drop") %>%
       group_by(status, manu, ageRange) %>% 
       mutate(denom =  ma7(date, n, k = the_k)$moving_avg) %>%
       mutate(rate = ma7(date, obs, k = the_k)$moving_avg/denom * 10^5) %>%
       ungroup() %>%
       filter(date >= input$event_range[1] & date <= input$event_range[2]) %>%
       filter(denom > 1000) %>%
       mutate(Vacunación = ifelse(status=="Vacunación expirada", "expirada", "al día"))
     
       p <- tmp %>% ggplot(aes(date, rate, color = manu, lty = Vacunación)) +
         geom_line(lwd = 1.25, alpha = 0.7) +
         labs(y="Tasa por día por 100,000", x="Fecha", title = the_title, 
              caption = paste("Basado en media móvil de", the_k,"días.\nArea gris contiene datos incompletos.")) +
         scale_color_manual(
           labels = c(UNV=manu_labels[["UNV"]], MOD=manu_labels[["MOD"]], PFR=manu_labels[["PFR"]],
                      JSN=manu_labels[["JSN"]]),
           values = c(UNV=manu_colors[["UNV"]], MOD=manu_colors[["MOD"]], PFR=manu_colors[["PFR"]],
                      JSN=manu_colors[["JSN"]]), name="Vacuna:") +
         theme_bw()+
         theme(legend.position = "bottom", text = element_text(size = 15)) +
         scale_x_date(date_labels = "%b", breaks = scales::breaks_width("1 month"))
     
     if(input$event_agerange == "facet") p <- p + facet_wrap(~ageRange)
     if(input$event_scale == "log") p <- p + scale_y_continuous(trans = "log2")
     
     if(input$event_range[2]>last_day-days(14)){
       p <- p + 
          annotate("rect", 
                   xmin = pmax(last_day-days(14), input$event_range[1]),
                   xmax = pmin(last_day, input$event_range[2]),
                   ymin = min(tmp$rate, na.rm = TRUE),
                   ymax = max(tmp$rate, na.rm = TRUE), alpha =0.2)
     }
     
     return(p)

  })
  
  output$muertes_tabla <- DT::renderDataTable({
    load(file.path(rda_path, "dat_cases_vax.rda"))
    
    dat_cases_vax <- dat_cases_vax %>%
      mutate(date = if_else(is.na(date) & !is.na(date_death), date_death, date)) %>%
      filter(date>= first_day & date <= last_day) %>%
      mutate(manu = factor(ifelse(is.na(manu_1), "UNV", as.character(manu_1)), levels= c("UNV", "MOD", "PFR", "JSN"))) %>%
      mutate(status = case_when(
        date > booster_date ~"BST",
        date > vax_date ~ "VAX",
        date > date_1 ~"PAR",
        TRUE ~ "UNV")) 
    
    dat_cases_vax$status <- factor(dat_cases_vax$status, levels = c("UNV", "PAR", "VAX", "BST"))
    
    dat_cases_vax$ageRange_2 <- fct_collapse(dat_cases_vax$ageRange, 
                                         "18-44" = c("18-24", "25-29", "30-34", "35-39", "40-44"),
                                         "45-64" = c("45-49", "50-54", "55-59","60-64"),
                                         "65+" = c("65-69", "70-74", "75-79","80-84", "85+"))
    date_name <-  paste0("date_", input$event_type)
    
    if(!input$event_agerange %in% c("all", "facet")){
      dat_cases_vax <- filter(dat_cases_vax, ageRange_2 == input$event_agerange)
    } 
    
    dat_cases_vax$cases <- TRUE
    dat_cases_vax$date_cases <- dat_cases_vax$date
    ret <- dat_cases_vax %>% 
      filter(!!sym(input$event_type)) %>%
      mutate(date = !!sym(date_name)) %>%
    #  filter(date >= input$event_range[1] & date <= input$event_range[2]) %>%
      slice(1:1000) %>%
      mutate(days = pmax(0, as.numeric(date-vax_date))) %>%
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
                        "Vacunción al día", "Con booster"),
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
      setNames(c("Municipio", "Población", "Una dosis", "Dosis completa", "Faltan", "Vacunación al día", "Con booster",
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
      mutate(total = make_pretty(total), entradas_esta_semana = make_pretty(entradas_esta_semana),
             rezago_esta_semana = ifelse(is.nan(rezago_esta_semana), 
                                                  "No se reportaron entradas", rezago_esta_semana)) %>%
      select(proveedor, total, rezago, entradas_esta_semana, rezago_esta_semana) %>%
      setNames(c("Proveedor", "Vacunas administradas", "Rezago medio (días)", "Entradas última semana","Rezago última semana")) %>% 
      kableExtra::kbl(align = c("l","r", "r","r", "r", "r"))  %>% 
      kableExtra::kable_styling()
  })
  output$tabla <- DT::renderDataTable({
    load(file.path(rda_path, "daily_counts.rda"))
    
    ret <- daily_counts %>% 
      filter(date >= input$tabla_range[1] & date <= input$tabla_range[2]) %>%
      pivot_wider(names_from = outcome, values_from = obs) %>%
      mutate(poblacion = make_pretty(round(poblacion))) %>%
      mutate(manu = recode(manu, "UNV"="", "MOD"="Moderna", "PFR" = "Pfizer", "JSN"="J&J")) %>%
      select(date, ageRange, gender, status, manu, poblacion, cases, hosp, death) %>%
      arrange(date, desc(ageRange), gender, status)
    
    make_datatable(ret, align = c("r", "r", "c", "c", "c", rep("r",4)), nowrap = 1:4,
                   col.names = c("Fecha", "Grupo de edad", "Género", "Vacunación", "Tipo de vacuna", "Población",
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
      labels_manu <- c(JSN=manu_labels[["JSN"]], MOD=manu_labels[["MOD"]],
                        PFR=manu_labels[["PFR"]])
      values_manu <- c(JSN=manu_colors[["JSN"]], MOD=manu_colors[["MOD"]],
                       PFR=manu_colors[["PFR"]])
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

Los datos presentados en este dashboard pueden cambiar según pasa el tiempo pues provienen de una base de datos 
viva sujeta a ediciones según nueva información se hace disponible. Además, como parte del proceso de organización y 
limpieza de datos, los analistas encargados de este dashboard pueden hacer modificaciones que entiendan pertinentes 
para mejorar la calidad de la información que aquí se presenta. 

<h4> ¿Por qué hay diferencias entre los resúmenes de vacunas con el dashboard del Departamento de Salud? </h4>
Como parte de una colaboración con el Programa de Vacunación del Departamento de Salud, logramos realizar un análisis detallado que llevamos a cabo 
durante febrero 2022 en el cual confirmamos que sobre 150,000 récords de vacunas contra COVID-19 en la base de datos de vacunas de Puerto Rico, 
el Sistema de Registro de Inmunizaciones (PREIS), que tiene cerca de 7 millones de récords de vacunas contra COVID-19, tenían errores de entrada
que resultaban en subestimación de terceras dosis y de personas no vacunadas. Desarrollamos un programa informático (<a> https://github.com/rafalab/fuzzypareo </a>) 
que detecta la gran mayoría de estos errores y los corrige de manera local (no en el PREIS) antes de calcular los resúmenes presentados aquí. 
No obstante, las correcciones al PREIS se tienen que realizar en coordinación con los requisitos de los CDC, que toma más tiempo, 
y los datos que presenta el Departamento de Salud se obtienen directamente del PREIS.
<br/> <br/>
  
<h4> Definiciones importantes </h4>
<ul><li> Vacunas administradas: suma de todas las primeras dosis, segundas dosis, y dosis de refuerzo administradas en Puerto Rico. 
Esto incluye personas que no son residentes de Puerto Rico. El resto de las definiciones solo incluye residentes de Puerto Rico.
<br/> 
</li><li> Personas con por lo menos una dosis: suma de todos los personas que se han puesto al menos una dosis. 
Note que todas las personas con más de una dosis están contempladas en esta suma. 
<br/> 
</li><li> Personas con serie primaria completa: suma de todos los personas que se han puesto dos dosis de Pfizer o Moderna o una de Janssen 
y han transcurrido 14 días desde su última dosis. 
<br/> 
</li><li> Personas con vacunación al día: suma de personas que se han puesto dosis de resfuerzo o 
tienen dosis completa que aún no necesitan dosis de refuerzo porque 
no han transcurrido 5 meses desde su segunda dosis de Moderna o Pfizer o 2 meses desde su primera dosis de Janssen. 
<br/> 
</li><li> Personas con boosters: suma de personas que completaron sus dosis de vacunación y se administraron su dosis de refuerzo. 
<br/> 
</li><li> Personas con serie primaria completa con necesidad de booster (vacunación expirada): suma de personas 
que ya completaron sus dosis de vacunación pero han transcurrido 5 meses (Pfizer o Moderna) o 2 meses (Janssen) y no se han puesto su dosis de refuerzo. 
<br/> 
</li><li> Menores (5-11 años) con por lo menos 1 dosis: suma de personas entre 5-11 años que cuentan con al menos una dosis. 
<br/> 
</li><li> Menores (5-11 años) con serie primaria completa:  suma de personas entre 5-11 años que cuentan con sus dos dosis y han transcurrido 14 días desde su última dosis. 
<br/> 
</li><li> Vacunación <i>parcial</i> se refiere a aquellas personas que han iniciado su serie de vacunación pero aún no se consideran como vacunados por no haber transcurrido 14 días luego de su última dosis requerida. 
<br/>
</li><li> <i>Vacunados</i> son aquellas personas que han completado su serie de vacunación: han transcurrido 14 días luego de su última dosis requerida.
<br/>
</li><li> Las personas con booster son aquellas personas que se han puesto sus dosis de refuerzo y han transcurrido 14 días desde la dosis de refuerzo. 
</li></ul>

<h4> Sobre el cálculo de tasas </h4>

Para calcular las tasas de cada evento por estado de vacunación y grupo de edad es necesario realizar un pareo entre la base de datos del BioPortal y 
la base de datos del PREIS. 
Este proceso carece de un identificador único común entre las bases, por lo que se ha desarrollado un algoritmo que realiza el pareo utilizando variables disponibles. Al momento, hemos descubierto que el algoritmo es 99% efectivo en identificar correctamente a las personas. Naturalmente, existen rezagos de entrada de datos en ambas bases, que convergen en el pareo y resulta en que las tasa de días recientes cambien mientras entran los datos. 
  
<h4> Estimados poblacionales </h4>

Los estimados poblacionales que se usan en este dashboard viene de <i> Vintage 2020 Population Estimates </i> para Puerto Rico publicados 
por el <i> U.S. Census Bureau</i> 
(<a> https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-detail-puerto-rico.html </a>). Si
el número de vacunados para un grupo demográfico supera el estimado poblacional de ese grupo demográfico, usamos el numero de vacunados como el tamaño de la
población. Esto ocurre para algunos grupos de edad en algunos municipios.
    
    "})
}

shinyApp(ui = ui, server = server)

                                       
                                


                