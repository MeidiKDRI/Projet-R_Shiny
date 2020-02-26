# Projet R Shiny 
# Meidi KADRI - Benoit BARBEREAU - Ronan PELE
# Simplon - 2020

library(shinyWidgets)
library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(hrbrthemes)
library(tidyr)
library(plotly)
library(tmap)

# Import Tables

conso <- read.csv("./data/global-fossil-fuel-consumption.csv", header = TRUE)
colnames(conso) <- c("entity","code", "year", "charbon","petrol","gaz")

prod <- read.csv('data/fossil-fuel-production-over-the-long-term.csv', sep = ',')
colnames(prod) <- c("Entity","Code", "Year", "charbon","petrol","gaz")

wconso <- read.csv('data/global-fossil-fuel-consumption.csv', header = TRUE)
cprod <- read.csv('data/fossil-fuel-production-over-the-long-term.csv', header = TRUE)
cconso <- read.csv('data/fossil-fuel-consumption-by-fuel-type.csv', header = TRUE)

colnames(wconso) <- c('Pays','Code','Année', 'Charbon','Pétrole','Gaz')
colnames(cprod) <- c('Pays','Code','Année', 'Charbon','Pétrole','Gaz')
colnames(cconso) <- c('Pays','Code','Année', 'Charbon','Pétrole','Gaz')

# Define UI for application that draws a histogram
ui <- navbarPage(
  'Analyse des énergies fossiles',
  theme = shinytheme('cerulean'),
  
  ##########################################
  ### Onglet BDD
  
  tabPanel('Base de Données',
           sidebarLayout(
             sidebarPanel(
               
               h3('Analyses des données de la production'),
               helpText('Filtrez les données en sélectionnant les critères qui vous intéressent.'),
               
               selectInput('sel_prod_country',
                           'Sélectionner un pays :',
                           c('Tout',
                             unique(as.character(prod$Entity)))),
               
               selectInput('sel_prod_year',
                           'Sélectionner une année :',
                           c('Tout',
                             unique(as.character(prod$Year)))),
               tags$hr()
               
             ),
             
             mainPanel(
               h2('Table de la Production Mondiale'),
               h3('Evolution de la production des energies fossiles dans le monde'),
               DT::dataTableOutput('producTable')
             )
           )
  ),

  ##########################################
  ### Onglet Conso Energie
  
  tabPanel("Consommation Energies",
           
           mainPanel(
             plotlyOutput("distPlot")
           )
  ),
             
  tabPanel('Production Energie',
           sidebarLayout(
             sidebarPanel(
               h4('Paramètres'),
               selectInput('slct_country',
                           'Pays',
                           c('Tout',
                             unique(as.character(cprod$Pays)))),
               # selectInput('slct_energy',
               #             'Enérgie :',
               #             c('Tout','Charbon','Pétrole','Gaz'))
               #                    
               
             ),
             
             mainPanel(
               h2('Production d\'énergie fossile par Pays'),
               plotlyOutput('cprodPlot'),
             )
           ),
  ),
  
  
  ##########################################
  ### Onglet Map Monde
  
  tabPanel("Map Monde",
          
          mainPanel(
            h2("Carte Mondiale de la Production d'énergie du Pétrole"),
            leafletOutput("my_tmap")),
            actionButton('saveBtn', "Enregistrer la carte", class = "btn-success")
          ),
  
  ##########################################
  ### Onglet Summary
  
  navbarMenu("Analyse des données",
             
             tabPanel("Analyses des données de la production",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          h3('Analyses des données de la production'),
                          helpText('Filtrez les données en sélectionnant les critères qui vous intéressent.'),
                          
                          selectInput('sel_prod_sum_country',
                                      'Sélectionner un pays :',
                                      c('Tout',
                                        unique(as.character(prod$Entity)))),
                          
                          selectInput('sel_prod_sum_year',
                                      'Sélectionner une année :',
                                      c('Tout',
                                        unique(as.character(prod$Year)))),
                          
                          tags$hr()
                          
                        ),
                        
                        mainPanel(
                          h3('Analyses des données de la production'),
                          textOutput('txtCtryProd'),
                          textOutput('txtYearProd'),
                          verbatimTextOutput("summ_prod")
                        )
                      )
             ),
             
             tabPanel("Analyses des données de la consommation",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          h3('Analyses des données de la production'),
                          helpText('Filtrez les données en sélectionnant les critères qui vous intéressent.'),
                          
                          selectInput('sel_conso_sum_year',
                                      'Sélectionner une année :',
                                      c('Tout',
                                        unique(as.character(conso$year))))
                        ),
                        
                        mainPanel(

                          h3('Analyses des données de la consommation'),
                          verbatimTextOutput("summ_conso")
                          
                        )
                      )
             )
  ),
  
  ##########################################
  ### Onglet FUSION TABLEAUX
  
  tabPanel('Fusion de tableaux',
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               tags$hr(),
               
               # Input: Select a file ----
               fileInput("csv1", "Fichier 1 : ",
                         buttonLabel = 'Importer un fichier',
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               fileInput("csv2", "Fichier 2 : ",
                         buttonLabel = 'Importer un fichier',
                         multiple = FALSE,
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ",",
                           ".csv")
               ),
               
               fileInput("csv3", "Fichier 3 : ",
                         buttonLabel = 'Importer un fichier',
                         multiple = FALSE,
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ",",
                           ".csv")
               ),
               
               fileInput("csv4", "Fichier 4 : ",
                         buttonLabel = 'Importer un fichier',
                         multiple = FALSE,
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ",",
                           ".csv")
               ),
               tags$hr(),
               
               actionButton("fusionBtn", "Fusionner les tableaux", class = "btn-primary"),
               
               tags$hr(),
               
               downloadButton('downloadBtn', "Télécharger sous Excel", class = "btn-success")
               
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               
               h2('Fusion des données'),
               h4('Sélectionnez les tableaux des opérateurs à fusionner.'),
               textOutput("csvname1"),
               textOutput("csvname2"),
               textOutput("csvname3"),
               textOutput("csvname4"),
               textOutput("fus_ok"),
               DT::dataTableOutput('final')
               
             )
           )
  ),

  ### Footer
  tags$footer("Projet R- Shiny SIMPLON © -
              Dashboard By Meidi KADRI - Benoit BARBEREAU - Ronan PELE - 2020",
              align = "center",
              style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: #3A9AD1;
              z-index: 1000;")
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ########################################
  ##### Graphes Consommation Energie
  
  output$distPlot <- renderPlotly({
    
    plot_ly(conso, x = ~year, y = ~charbon, name = 'charbon', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = '#F5FF8D') %>%
      add_trace(y = ~petrol, name = 'Pétrole', fillcolor = '#50CB86') %>%
      add_trace(y = ~gaz, name = 'gaz', fillcolor = '#4C74C9') %>%
      layout(title = 'Consommation mondiale d énergie fossile en terawatt par heure',
             xaxis = list(title = "année",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "Consommation en terawatt par heure",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
    
  })

  ########################################
  ##### World Map
  
  map <- reactiveValues(dat = 0)
  
  output$my_tmap = renderLeaflet({
    tm <- tm_shape(World) + tm_polygons("economy", id = 'name', popup.vars = TRUE)
    tmap_leaflet(tm)
  })
  

  output$saveBtn <- downloadHandler(
    filename = function() { 
      paste("Map.png")
    },
    content = function(file) {
      mapshot(map$dat, file)
    })
  
  
  
  ########################################
  ##### Summaries
  
  ### Production Summary
  
  output$txtCtryProd <- renderText(paste("Vous avez sélectionné : ", input$sel_prod_sum_country, "."))
  output$txtYearProd <- renderText(paste("Pour l'année : ", input$sel_prod_sum_year))
  

  output$summ_prod <- renderPrint({
    
    prod_sum <- prod
    if (input$sel_prod_sum_country != 'Tout'){
      prod_sum <- prod_sum[prod_sum$Entity == input$sel_prod_sum_country, ]
    }
    if (input$sel_prod_sum_year != 'Tout'){
      prod_sum <- prod_sum[prod_sum$Year == input$sel_prod_sum_year, ]
    }
    summary(prod_sum)
  })
  
  ### Consommation Summary
  output$summ_conso <- renderPrint({
    
    conso_sum <- conso
    if (input$sel_prod_sum_year != 'Tout'){
      conso_sum <- conso_sum[conso_sum$year == input$sel_prod_sum_year, ]
    }
    summary(conso_sum)
  })
  
  ########################################
  ##### Base de données

  output$producTable <- DT::renderDataTable(DT::datatable({
    data <- prod
    if (input$sel_prod_country != 'Tout'){
      data <- data[data$Entity == input$sel_prod_country, ]
    }
    if (input$sel_prod_year != 'Tout'){
      data <- data[data$Year == input$sel_prod_year, ]
    }
    data
  }))
  
  
  ########################################
  ##### Upload des datas
  
  csvOne <- eventReactive(input$csv1,{
    inFile <- input$csv1
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, sep = ';')
  })
  
  output$csvname1 <- renderText(paste("Tableaux n°1 : ", input$csv1$name))
  
  csvTwo <- eventReactive(input$csv2,{
    inFile <- input$csv2
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, sep = ';')
  })
  
  output$csvname2 <- renderText(paste("Tableaux n°2 : ", input$csv2$name))
  
  csvThree <- eventReactive(input$csv3,{
    inFile <- input$csv3
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, sep = ';')
  })
  
  output$csvname3 <- renderText(paste("Tableaux n°3 : ", input$csv3$name))
  
  csvFour <- eventReactive(input$csv4,{
    inFile <- input$csv4
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, sep = ';')
  })
  
  output$csvname4 <- renderText(paste("Tableaux n°4 : ", input$csv4$name))
  
  #######################################
  
  ########################################
  ##### Fusions des données 
  # do.call permet de faire un merge sur une liste de dataframes
  final_react <- eventReactive(input$fusionBtn,
                               do.call("rbind", list(csvOne(), csvTwo(), csvThree(), csvFour())))
  
  output$final <- DT::renderDataTable({
    final_react()
  })
  
  output$downloadBtn <- downloadHandler(
    filename = function() { 
      paste("finalTable.csv", sep=";")
    },
    content = function(file) {
      write.csv(final_react(), file)
    })
  
  output$cprodPlot <- renderPlotly({ 
    
    dfcprod <- cprod
    if (input$slct_country != 'Tout'){
      dfcprod <- cprod %>% filter(Pays == input$slct_country)
    }
    
    plot_ly(dfcprod, x = ~Année, y = ~Charbon, name = 'Charbon', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = '#4fbb48') %>%
      add_trace(y = ~Pétrole, name = 'Pétrole', fillcolor = '#7c4439') %>%
      add_trace(y = ~Gaz, name = 'Gaz', fillcolor = '#844399') %>%
      layout(title = "Production du pays d'énergies fossiles (TWh)",
             xaxis = list(title = 'Année'),
             yaxis = list(title = 'Production (TWh)'))
    
  })
  
  output$cprod <- renderTable(cprod)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
