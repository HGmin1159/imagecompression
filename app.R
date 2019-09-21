library(shiny)
library(imager)
source("helper.R")

ui <- fluidPage(
  sidebarLayout(
  sidebarPanel(width = 3,
  fileInput("file1", label = h3("Image input"),),
  sliderInput("slider1", label = h3("Percent Range"), min = 1, 
              max = 100, value = c(1,100)),
  selectInput("select", label = h3("Number of plot"), 
              choices = list("3", "6", "9"), 
              selected = "3"),
  fluidRow(
  column(3,
  actionButton("action", label = "plotting")
  ))
  ),
  mainPanel(width = 9,
  plotOutput('image',height = "800px")
  )
)
)


server <- function(input,output){
  myjpg = eventReactive(input$file1,load.image(input$file1$datapath))
  ploting = eventReactive(input$action,
    reduction(myjpg(),as.numeric(input$select),input$slider1)
  )
  output$image = renderPlot(ploting())
}

shinyApp(ui=ui,server=server)
