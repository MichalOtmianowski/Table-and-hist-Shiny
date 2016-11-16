library(plotly)
library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  sidebarPanel(
    
    fileInput('file', 'Za³aduj plik'),
    conditionalPanel(condition="output.table!=null", selectInput('variable', 'Wybierz kolumnê do histogramu', choices=NULL))
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Table", tableOutput('table')),
      tabPanel(value='hst', "Histogram", conditionalPanel(condition='output.table!=null',plotlyOutput('histogram')))
      
    
    )
  )
)
server <- function(input, output, session) {
  
  data<-reactive({
    
    uploaded_file<-input$file
    if(is.null(uploaded_file)){
      return(NULL)
    }
    read.csv(uploaded_file$datapath)
  })
  
  output$table<-renderTable({
    data()
  })
  
  
  output$histogram<-renderPlotly({
      p<-qplot(isolate(data())[,input$variable], geom="histogram",  fill=I("grey"), col=I("black"))+
      ylab('Liczba wyst¹pieñ')+xlab(paste('Wartoœæ kolumny', input$variable))+
      ggtitle(paste('Histogram kolumny', input$variable, 'z pliku', isolate(input$file$name)))
      ggplotly(p)
  })
  
  observe({
    
    numerics<-sapply(data(), is.numeric)
    col_names<-names(data()[,numerics])
    updateSelectInput(session, "variable",
                      choices = col_names,
                      selected = col_names[1]
    )
  })
}


shinyApp(ui = ui, server = server)