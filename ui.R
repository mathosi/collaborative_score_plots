
library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot", width = 1200, height = 800),
  textInput('caption', 'Selection name', 'Group_1'),
  actionButton('button', label = 'Save selection'),
  actionButton('button2', label = 'Save all selections to Dropbox'),
  actionButton('button3', label = 'Clear all selections'),
  #verbatimTextOutput('value'),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush"),
  verbatimTextOutput('responseList'), 
  textOutput("messages"),
  DT::dataTableOutput("responses", width = 300), 
  tags$hr()
)