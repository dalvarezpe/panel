library(shinydashboard)
library(shiny)
library(DT)
library(plotly)
library(colourpicker)
library(tools)
library(stringr)
# title <- tags$a(href='https://www.google.com',
#                icon("diamond"),
#               'Diamonds Explorer')
title <- tags$a(href='https://www.google.com',
                tags$img(src="logo.png", height='40', widht='40'),
                'DATA WORLD BOLIVIA')

header <- dashboardHeader(
  title = title, titleWidth = 600,
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "DATA WORLD BOLIVIA",
      message = "Invitación a cursos",
      icon =  icon("graduation"),
      href = "https://drive.google.com/open?id=19OeeZMSIreqyoBLNg9LUZjPqHlGXQT3H"
    ),
    # Add a second messageItem() 
    messageItem(
      from = "DATA WORLD BOLIVIA",
      message = "New Course",
      href = "https://spotthestation.nasa.gov/faq.cfm"
    )
  ))
####END DASHBOARDHEADER

sidebar<- dashboardSidebar(
  tags$head(
    tags$link(rel="stylesheet",type="text/css", href="custom.css")
  ),
  div(img(src = "logo.png", height = "100px"), style="text-align: center;"),
  selectInput("var","Elegir Variable",
              choices=levels(DATAB$Indicador),
              multiple = FALSE,
              selected = "buscar"),#Prima de riesgo por préstamo(tasa de la prima menos tasa de los bonos del tesoro,%)
  sliderInput("years", "Años",
              min(DATAB$Año), max(DATAB$Año),
              value = c(1990, 2019)),
  colourInput("colour", "Elegir color", value = "#3F8596"),
  numericInput("size", "Ajustar Tamaño", 1, 1),
  textInput("title", "Título", "Escribe el título"),
  textInput("eje","Valor de eje","Escribe los valores del eje")
)

body <- dashboardBody(tabsetPanel(type = "tabs",
                                  tabPanel("GRAFICO", plotlyOutput("plot", width = "700",height = "400"),
                                           tableOutput(outputId = "table1")),
                                  tabPanel("DESCARGAR DATOS",downloadButton(outputId = "download_data", label = "Descargar datos"),
                                           DT::dataTableOutput("table"))  
))

server <- function(input, output) {
  output$plot<-renderPlotly({
    ggplotly({
      DAT<-subset(DATAB, 
                  Indicador %in% input$var&
                    Año >= input$years[1] & Año <= input$years[2]) 
      
      p<-ggplot(DAT, aes(Año, Valor))+
        geom_line(size = input$size, col = input$colour)+
        ggtitle(input$title)+
        xlab("Año")  + ylab(input$eje)
      p
    })
  })
  
  output$table<- DT::renderDataTable({DATA1<-select(subset(DATAB, 
                                                           Indicador %in% input$var&
                                                             Año >= input$years[1] & Año <= input$years[2]),Año,Indicador,Valor) 
  })
  output$table1<- renderTable({head(select(subset(DATAB, 
                                                  Indicador %in% input$var&
                                                    Año >= input$years[1] & Año <= input$years[2]),NOTA,FUENTE),1)
  })
  
  output$download_data <- downloadHandler(
    filename = "data.csv",
    content = function(file) {DAT
      
      write.csv(DAT,file,row.names = FALSE)})
}

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)
shinyApp(ui, server)

