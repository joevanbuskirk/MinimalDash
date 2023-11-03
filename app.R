library(sf)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(htmlwidgets)
library(plotly)
library(DT)

if(!require(renv)){install.packages('renv')}

depends <- renv::dependencies()

for (p in unique(depends$Package)){
  if(!require(p, character.only = TRUE)){install.packages(p)}
  library(p, character.only = TRUE)
}
       

## For testing only
source('Functions.R')

SA2_Tables <- readRDS('SA2_Tables.rds')
LHD_Tables <- readRDS('LHD_Tables.rds')
LHD_Cropped <- sf::st_read('LHD_Cropped/')
SA2_Cropped <- sf::st_read('SA2_2021_Cropped/')

names(SA2_Cropped) <- gsub('_2', '', names(SA2_Cropped))
names(SA2_Cropped) <- gsub('\\..*', '', names(SA2_Cropped))

AllVars <- setNames(c("Usual Resident Population",
                      "Born Overseas",
                      "Other languages than English mainly spoken",
                      "Median weekly personal income",
                      "Needs assistance with core activities",
                      "Mental health condition (including depression or anxiety)",
                      "Diabetes (excluding gestational diabetes)",
                      "Asthma",
                      "Arthritis",
                      "Stroke",
                      "Cancer (including remission)",
                      "Dementia (including Alzheimer's)",
                      "Heart disease (including heart attack or angina)",
                      "Kidney disease",
                      "Lung condition (including COPD or emphysema)",
                      'Index of Relative Socio-Economic Disadvantage (IRSD)',
                      'Index of Relative Socio-Economic Advantage and Disadvantage (IRSAD)',
                      'Index of Education and Occupation (IEO)',
                      'Index of Economic Resources (IER)'),
                    c('URP',
                      'BornOS',
                      'LOTE',
                      'MedIncome',
                      'Assist',
                      'MH',
                      'Diabetes',
                      "Asthma",
                      "Arthritis",
                      "Stroke",
                      "Cancer",
                      "Dementia",
                      "Heart",
                      "Kidney",
                      "Lung",
                      "IRSD", 
                      "IRSAD", 
                      "IEO", 
                      "IER"))

SexLookup <- setNames(c('Males', 'Females', 'Persons'),
                      as.character(1:3))

Variable_Types <- setNames(c('URP',
                             'BornOS',
                             'LOTE',
                             'MedIncome',
                             'Assist',
                             'MH',
                             'Diabetes',
                             "Asthma",
                             "Arthritis",
                             "Stroke",
                             "Cancer",
                             "Dementia",
                             "Heart",
                             "Kidney",
                             "Lung",
                             "IRSD", 
                             "IRSAD", 
                             "IEO", 
                             "IER"),
                           c('num',
                             'pct',
                             'pct',
                             'money',
                             rep('asr', 11),
                             rep('seifa', 4)))

LHD_Cropped_Labels <- cbind(
  sf::st_drop_geometry(LHD_Cropped),
  sf::st_coordinates(sf::st_centroid(LHD_Cropped)))

### Classify each of the Values by year and sex
### Could make it consistent across year for better comparison? 
### Easy to change if need be
SA2_Tables <- purrr::imap(
  SA2_Tables,
  function(df, variable){
    type_in <- names(Variable_Types[Variable_Types == variable])
    dplyr::mutate(df,
                  Value_Cat = Classify_Quantiles(Value, type_in),
                  .by = dplyr::matches('TIME_PERIOD|SEXP'))
  })

header <- dashboardHeader(title = "Census Local Health District Visualisation - PHRAME", 
                          titleWidth = '1000')

body <- dashboardBody(
  ### These boxes are for troubleshooting only
  # box(
  #   title = "Selected Type", 
  #   solidHeader = TRUE, 
  #   status = "primary", 
  #   width = 4, 
  #   textOutput("Type_Sel")
  # ),
  # box(
  #   title = "Selected Sex", 
  #   solidHeader = TRUE, 
  #   status = "primary", 
  #   width = 4, 
  #   textOutput("Type_Sex")
  # ),  
  # box(
  #   title = "Selected Year", 
  #   solidHeader = TRUE, 
  #   status = "primary", 
  #   width = 4, 
  #   textOutput("Type_Year")
  # ),
  
  tags$style(HTML(paste(
    '.dataTables_wrapper',
    '.dataTables_filter {float: left; padding-left: 50px;}',
    '.dataTables_wrapper',
    '.dataTables_filter input{width: 500px;}'))),
  
  fluidRow(
    tabBox(
      title = NULL, width = 12, height = 0.2,  
      tabPanel(
        'Dashboard',
        fluidRow(
          column(
            width = 10,
            tabItem(tabName = 'dashboard',
                    box(width = NULL, 
                        solidHeader = TRUE,
                        leafletOutput(outputId = "map", 
                                      height = 500))
                    # ,
                    # box(width = NULL,
                    #     plotlyOutput("barchart", 
                    #                  height = 250)))
          )
          # , 
          # column(width = 6, 
          #        height = 814, 
          #        tabItem(tabName = 'dashboard',
          #                box(width = NULL, 
          #                    height = '100%',
          #                    downloadButton('download',
          #                                   "Download SA2 data"),
          #                    DTOutput("datatable")))
          )
        )
      ),
      tabPanel('Data source and methods',
               box(width = 700,
                   (div(style='height:700px;overflow-y: scroll;',
                        htmlOutput('markdown', height = 700)))
                   
                   # style = "overflow-y: scroll",
                   # htmlOutput('markdown')
               )
      )
    ),
  )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", 
             selected = TRUE, 
             startExpanded = TRUE, 
             tabName = "dashboard",
             icon = icon("dashboard"),
             selectInput(inputId = "Type",
                         label = h4("Variable"),
                         choices = unname(AllVars)),
             selectInput(inputId = "Year",
                         label =  h4("Census year"),
                         choices = '2021'),
             selectInput(inputId = "Sex",
                         label =  h4("Gender"),
                         choices = 'Persons')
    )
  )
)

# input <- data.frame(Type = AllVars[18],
#                     Sex = SexLookup[1],
#                     Year = '2011')
# input <- data.frame(Type = AllVars[2],
#                     Sex = SexLookup[3],
#                     Year = '2021')
# input <- data.frame(Type = AllVars[6],
#                     Sex = SexLookup[3],
#                     Year = '2011')
# input <- data.frame(Type = AllVars[2],
#                     Sex = SexLookup[3],
#                     Year = '2011')

server <- function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet(data = SA2_Cropped) %>%
      setView(lng = 151.1258,
              lat = -33.89312,
              zoom = 11) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMapPane("Maps", zIndex = 410) %>%
      addMapPane("SLHD", zIndex = 420) %>%
      addPolygons(data = LHD_Cropped,
                  group = 'Base',
                  weight = 3,
                  options = pathOptions(pane = "SLHD"),
                  color = 'black',
                  fill = NA) %>%
      addLabelOnlyMarkers(data = LHD_Cropped_Labels,
                          lng = ~X,
                          lat = ~Y,
                          label = ~paste(Name, "LHD"),
                          options = pathOptions(pane = "SLHD"),
                          labelOptions = labelOptions(noHide = TRUE,
                                                      textsize = 10, 
                                                      direction = 'top',
                                                      textOnly = TRUE))
  })
  # 
  
  ## Observe the events on the sidebar to update content
  observe({
    Input_Type <- names(AllVars[AllVars == input$Type])
    SA2data <- SA2_Tables[[Input_Type]]
    
    SexChoices <- unname(SexLookup[sort(unique(SA2data$SEXP))])
    SexChoice <- ifelse(input$Sex %in% SexChoices, 
                        input$Sex,
                        'Persons')
    
    updateSelectInput(session, "Sex",
                      selected = SexChoice,
                      choices = SexChoices)
    
    YearChoices <- sort(unique(SA2data$TIME_PERIOD))
    YearChoice <- ifelse(input$Year %in% YearChoices,
                         input$Year,
                         YearChoices[1])
    
    updateSelectInput(session, "Year",
                      selected = YearChoice,
                      choices = YearChoices)
    
    Input_Sex <- names(SexLookup[SexLookup == SexChoice])
    
    output$YearChoice <- renderText(YearChoice)
    # output$Type_Sel <- renderText(Input_Type) 
    # output$Type_Sex <- renderText(Input_Sex) 
    # output$Type_Year <- renderText(input$Year) 
    
    SA2data <- dplyr::filter(SA2data,
                             TIME_PERIOD == YearChoice) %>% 
      dplyr::mutate(Name = gsub(" \\(.*", "", Name))
    # 
    LHDdata <- LHD_Tables[[Input_Type]]
    
    ## Assign type of variable
    vartype <- names(Variable_Types[Variable_Types == Input_Type])
    
    ## Keep all years if it's SEIFA
    if(vartype != 'seifa'){
      LHDdata <- dplyr::filter(LHDdata,
                               TIME_PERIOD == YearChoice)
    }
    
    LHDdata$LHD <- forcats::fct_relevel(LHDdata$Name, 
                                        'Sydney',
                                        after = 0L)
    
    ### Choose variable type for the markdown info
    info_type <- ifelse(vartype %in% c('num', 'money', 'pct'),
                        'num', vartype)
    
    output$markdown <- renderUI({
      includeHTML(paste0(info_type, '.html'))
    })
    
    SA2data <- tidyr::drop_na(SA2data)
    
    if(vartype == "asr"){
      SA2data$Value_Label <-  asr_ci(SA2data$Value,
                                     SA2data$lci,
                                     SA2data$uci)
      LHDdata$Value_Label <- asr_ci(LHDdata$Value,
                                    LHDdata$lci,
                                    LHDdata$uci)
      
    } else if(vartype == 'pct'){
      SA2data$Value_Label <-  scales::percent(SA2data$Value, 
                                              accuracy = 0.1)
      
      LHDdata$Value_Label <-  scales::percent(LHDdata$Value, 
                                              accuracy = 0.1)
      
    } else if(vartype == 'num'){
      SA2data$Value_Label <-  scales::number(SA2data$Value, 
                                             accuracy = 1, 
                                             big.mark = ',')
      LHDdata$Value_Label <-  scales::number(LHDdata$Value, 
                                             accuracy = 1,
                                             big.mark = ',')
      
    } else if(vartype == 'money'){
      SA2data$Value_Label <-  scales::dollar(SA2data$Value, 
                                             accuracy = 1, 
                                             big.mark = ',')
      LHDdata$Value_Label <-  scales::dollar(LHDdata$Value, 
                                             accuracy = 1,
                                             big.mark = ',')
      
    } else if(vartype == 'seifa'){
      SA2data$Value_Label <-  SA2data$Value
      LHDdata$Value_Label <-  scales::percent(LHDdata$Value, 
                                              accuracy = 0.1)
      
    }      
    
    SA2Map <- dplyr::left_join(SA2_Cropped,
                               SA2data[SA2data$SEXP == Input_Sex,],
                               by = c('sa2_code' = 'REGION'))
    
    SA2Map <- dplyr::mutate(SA2Map,
                            Value_Cat = forcats::fct_reorder(Value_Cat,
                                                             Value, 
                                                             .na_rm = TRUE))
    SA2Map <- sf::st_as_sf(SA2Map)
    
    pal <- colorFactor(
      pals::brewer.blues(n = length(levels(SA2Map$Value_Cat))),
      domain = SA2Map$Value_Cat,
      na.color = NA)
    
    
    ### Format data for download
    # SA2_Download <- SA2data %>% 
    #   dplyr::rename(SA2_Code21 = REGION,
    #                 SA2_Name21 = sa2_name_2021,
    #                 LHD = Name,
    #                 Year = TIME_PERIOD,
    #                 Sex = SEXP)
    # 
    # ### UP TO HERE -----------
    # ## Select variables based on indicator type
    # if(vartype == 'asr'){
    #   SA2_Download <- SA2_Download %>% 
    #     dplyr::mutate(Sex_lab = SexLookup[Sex]) %>% 
    #     dplyr::select(Year, SA2_Code21, SA2_Name21, 
    #                   LHD, 
    #                   dplyr::matches('_lab$'),
    #                   ASR = Value,
    #                   ASR_LCI = lci,
    #                   ASR_UCI = uci)
    #   
    # } else if(vartype == 'seifa'){
    #   SA2_Download <- SA2_Download %>% 
    #     dplyr::mutate(Index = AllVars[INDEX_TYPE]) %>% 
    #     dplyr::select(Index, Year, SA2_Code21, SA2_Name21, LHD, 
    #                   Decile = Value)  
    #   
    # } else if(vartype == 'num'){
    #   SA2_Download <- SA2_Download %>% 
    #     dplyr::select(Year, SA2_Code21, SA2_Name21, LHD, 
    #                   dplyr::matches('_lab$'), 
    #                   Count = Value)
    # } else if(vartype == 'pct'){
    #   SA2_Download <- SA2_Download %>% 
    #     dplyr::select(Year, SA2_Code21, SA2_Name21, LHD, 
    #                   dplyr::matches('_lab$'), 
    #                   Proportion = Value)
    # }
    # 
    # output$download <- downloadHandler(
    #   filename = function(){paste0(Input_Type, "_", input$Sex, "_", 
    #                                input$Year, ".csv")}, 
    #   content = function(fname){
    #     write.csv(SA2_Download, fname)
    #   }
    # )
    # 
    # 
    # sketch <- if(vartype == 'seifa'){
    #   htmltools::withTags(table(
    #     class = 'display',
    #     thead(
    #       tr(
    #         th(rowspan = 2, 'SA2 Name'),
    #         th(rowspan = 2, 'Local Health District'),
    #         th(colspan = 1, 'SEIFA Decile'),
    #       ),
    #       tr(
    #         lapply(SexChoices, th)
    #       )
    #     )
    #   ))
    # } else {
    #   htmltools::withTags(table(
    #     class = 'display',
    #     thead(
    #       tr(
    #         th(rowspan = 2, 'SA2 Name'),
    #         th(rowspan = 2, 'Local Health District'),
    #         th(colspan = length(SexChoices), 'Gender'),
    #       ),
    #       tr(
    #         lapply(SexChoices, th)
    #       )
    #     )
    #   ))
    # }
    # 
    # ##### Render the Data Table #####
    # Table_Variable <- ifelse(vartype == 'asr', 'Value_Label', 'Value')
    # 
    # datatab <- DT::datatable(
    #   tidyr::spread(
    #     dplyr::mutate(SA2data[, c('sa2_name_2021', 'Name', 'SEXP', Table_Variable)],
    #                   Name = factor(Name),
    #                   SEXP = SexLookup[SEXP]),
    #     SEXP, 
    #     get(Table_Variable)),
    #   rownames = FALSE,
    #   filter = 'top',
    #   options = list(
    #     pageLength = 13,
    #     columnDefs = list(list(targets = c(0,2:(1 + length(SexChoices))),
    #                            searchable = FALSE)),
    #     dom = 'tip'),
    #   style = 'bootstrap4',
    #   container = sketch) %>% 
    #   DT::formatStyle(columns = c(0:(1 + length(SexChoices))),
    #                   fontSize = '10pt')
    # 
    # if(vartype == 'asr'){
    #   output$datatable <- renderDT({datatab})
    # } else if(vartype == 'pct') {
    #   output$datatable <- renderDT({DT::formatPercentage(
    #     datatab,
    #     columns = 3:(2 + length(SexChoices)), 1)
    #   })
    # } else if(vartype == 'num') {
    #   output$datatable <- renderDT({DT::formatRound(
    #     datatab,
    #     columns = 3:(2 + length(SexChoices)), 
    #     digits = 0)
    #   })
    # } else if(vartype == 'money') {
    #   output$datatable <- renderDT({DT::formatCurrency(
    #     datatab,
    #     columns = 3:(2 + length(SexChoices)), 
    #     digits = 0)
    #   })
    # } else if(vartype == 'seifa') {
    #   output$datatable <- renderDT({datatab})
    # }
    # 
    # textsize <- 8
    # textangle <- 45
    # 
    # 
    # ##### Render the Bar Chart #####
    # output$barchart <-
    #   renderPlotly({
    #     if(vartype == 'asr'){
    #       baseplot <- ggplot(dplyr::mutate(LHDdata,
    #                                        SEXP = factor(SEXP,
    #                                                      levels = 1:3,
    #                                                      labels = SexLookup),
    #                                        SEXP = forcats::fct_rev(SEXP)),
    #                          aes(y = Value,
    #                              ymin = lci,
    #                              ymax = uci,
    #                              x = LHD,
    #                              fill = SEXP,
    #                              text = paste(
    #                                'Name:', LHD,
    #                                "<br>Sex:", SEXP,
    #                                "<br>ASR [95% CI]:", Value_Label))) +
    #         scale_y_continuous(paste0(gsub("(\\(.*)|( with core.*)", "", 
    #                                        input$Type),
    #                                   "\n(ASR by 10,000 population)")) 
    #       
    #     }
    #     else if(vartype == 'pct')
    #     {
    #       baseplot <- ggplot(dplyr::mutate(LHDdata,
    #                                        SEXP = factor(SEXP,
    #                                                      levels = 1:3,
    #                                                      labels = SexLookup),
    #                                        SEXP = forcats::fct_rev(SEXP)),
    #                          aes(x = LHD,
    #                              y = Value,
    #                              fill = SEXP,
    #                              text = paste(
    #                                "Name:", LHD,
    #                                "<br>Sex:", SEXP,
    #                                "<br>Proportion:", scales::percent(Value, 
    #                                                                   accuracy = 0.1)))) +
    #         scale_y_continuous(paste0(input$Type, "\n(%)"),
    #                            labels = scales::percent) 
    #     }
    #     else if(vartype == 'num')
    #     {
    #       baseplot <- ggplot(dplyr::mutate(LHDdata,
    #                                        SEXP = factor(SEXP,
    #                                                      levels = 1:3,
    #                                                      labels = SexLookup),
    #                                        SEXP = forcats::fct_rev(SEXP)),
    #                          aes(y = Value,
    #                              x = LHD,
    #                              fill = SEXP,
    #                              text = paste(
    #                                'Name:', Name,
    #                                "<br>Sex:", SEXP,
    #                                "<br>Count:", scales::comma(Value, 
    #                                                            accuracy = 1)))) +
    #         scale_y_continuous(paste(input$Type, "\n(N)"),
    #                            labels = scales::comma) 
    #     }
    #     else if(vartype == 'money')
    #     {
    #       baseplot <- ggplot(dplyr::mutate(LHDdata,
    #                                        SEXP = factor(SEXP,
    #                                                      levels = 1:3,
    #                                                      labels = SexLookup),
    #                                        SEXP = forcats::fct_rev(SEXP)),
    #                          aes(y = Value,
    #                              x = LHD,
    #                              fill = SEXP,
    #                              text = paste(
    #                                'Name:', Name,
    #                                "<br>Sex:", SEXP,
    #                                "<br>Count:", scales::comma(Value, 
    #                                                            accuracy = 1)))) +
    #         scale_y_continuous(paste(input$Type, "\n($)"),
    #                            labels = scales::comma) 
    #     }
    #     else if(vartype == 'seifa')
    #     {
    #       baseplot <- ggplot(dplyr::mutate(LHDdata,
    #                                        SEXP = factor(SEXP,
    #                                                      levels = 1:3,
    #                                                      labels = SexLookup),
    #                                        SEXP = forcats::fct_rev(SEXP)),
    #                          aes(y = Value,
    #                              x = LHD,
    #                              fill = TIME_PERIOD,
    #                              text = paste('Name:', Name,
    #                                           '<br>Year:', TIME_PERIOD,
    #                                           "<br>Proportion:", scales::percent(Value)))) +
    #         scale_y_continuous(paste(Input_Type, 
    #                                  "\n(% in highest two quintiles)"),
    #                            labels = scales::percent) 
    #       
    #     }
    #     
    #     baseplot <- baseplot +
    #       geom_col(position = position_dodge(width = 0.8),
    #                width = 0.8,
    #                colour = 'grey60') +
    #       scale_fill_brewer(NULL,
    #                         palette = 'Blues',
    #                         direction = -1,
    #                         breaks = rev) +
    #       scale_x_discrete("Local Health District") +
    #       theme_minimal() +
    #       theme(axis.text.x = element_text(angle = textangle,
    #                                        hjust = 1),
    #             text = element_text(size = textsize))
    #     
    #     plotly::ggplotly(baseplot, 
    #                      tooltip = c('text'))  
    #   })
    # 
    
    ##### Render the Leaflet
    if(vartype == 'asr'){
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name_2021}</b>",
                      "Gender: {input$Sex}",
                      "{Input_Type}: {Value_Label}"),
                      htmltools::HTML)) %>%
        addLegend(title = gsub("(\\(.*)|( with core.*)", "", 
                               input$Type),
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    } else if(vartype == "pct") {
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name_2021}</b>",
                      "Gender: {input$Sex}",
                      "{Input_Type}: {scales::percent(Value, a = 1)}"),
                      htmltools::HTML)) %>%
        addLegend(title = paste(input$Type, "(%)"),
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    } else if(vartype == "num") {
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name_2021}</b>",
                      "Gender: {input$Sex}",
                      "{Input_Type}: {scales::comma(Value)}"),
                      htmltools::HTML)) %>%
        addLegend(title = input$Type,
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    } else if(vartype == "money") {
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name_2021}</b>",
                      "Gender: {input$Sex}",
                      "{Input_Type}: {scales::dollar(Value)}"),
                      htmltools::HTML)) %>%
        addLegend(title = paste(input$Type, "($)"),
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    } else if(vartype == "seifa") {
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name_2021}</b>",
                      "Year: {input$Year}",
                      "{Input_Type} Decile: {Value_Label}"),
                      htmltools::HTML)) %>%
        addLegend(title = Input_Type,
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    }
    
  })
  
  
}


shinyApp(ui = dashboardPage(header, sidebar, body), server = server)
