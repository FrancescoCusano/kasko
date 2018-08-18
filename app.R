
library(googlesheets)
library(shiny)
library(shinythemes)
library(rsconnect)
library(reshape)
library(xts)
#library(zoo)
library(googleVis)

# rsconnect::setAccountInfo(name='francesco-cusano',
#                           token='C29BD6F3990BCA4D75112A052B0A70E8',
#                           secret='9zxnsDpxQbq/YRDygJPX+MhniNhcxGfZ066IbZqd')

kasko <- gs_title("kasko")
kasko <- gs_read(kasko, ws = 1)


fn1 <- function(num_giocatori, gioco){
  if(num_giocatori=="-" & gioco=="-"){
    tmp <- recast(kasko, vincitore~., measure.var = "valore", sum)
  } else if (gioco=="-"){
    tmp <- kasko[kasko$num_giocatori==as.numeric(num_giocatori),]
    tmp <- recast(tmp, vincitore~., measure.var = "valore", sum)
  } else if(num_giocatori=="-"){
    tmp <- kasko[kasko$gioco==gioco,]
    tmp <- recast(tmp, gioco~vincitore, measure.var = "valore", sum)
  } else{
    tmp <- kasko[kasko$num_giocatori==as.numeric(num_giocatori) & kasko$gioco==gioco,]
    tmp <- recast(tmp, vincitore~., measure.var = "valore", sum)
#    return(tmp)
  }
}

general.options <- list(
  fontSize="12",
  backgroundColor="#E0F1FF",
  height="450",
  chartArea="{backgroundColor:{fill:'#fff'}}",
  legend="{ position: 'top', alignment: 'start', textStyle: {fontSize: 12}}",
  series="[{color:'blue'}, {color: 'green'}, {color: 'red'}]",
  vAxes="[{minValue:0, maxValue:100}]"
)


server <- (function(input, output) {
  dataset <- reactive({ fn1(as.character(input$num_giocatori), as.character(input$gioco))
  })

  output$out    <- renderGvis({
    gvisColumnChart(dataset(), options=as.list(general.options))
  })
})



ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("kasko"),

  sidebarLayout(
    sidebarPanel(width=3,
      selectInput("num_giocatori", label = "Numero di giocatori", choices = c("-",unique(kasko$num_giocatori)), selected = "-"),
      selectInput("gioco", label = "Gioco", choices = c("-",unique(kasko$gioco)), selected = "-"),
      actionButton("go", "Go!", style="color:red")
    ),


    mainPanel(width=9,
      htmlOutput("out")
    )
  )
)

shinyApp(ui = ui, server = server)








