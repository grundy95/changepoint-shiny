library(shiny)
library(changepoint)
library(ggplot2)

ui <- navbarPage(title = "changepoint",
    tabPanel('Home',"Home Page"),
    tabPanel('AMOC Simulated Data',
             fluidRow(
                 sidebarLayout(
                     sidebarPanel(
                         tags$h2("Data Parameters"),
                         sliderInput(inputId = "n",
                                     label = "Length of data set",
                                     value = 200,
                                     min = 20,
                                     max = 2000,
                                     step = 1),
                         uiOutput("tauControls"),
                         selectInput(inputId = 'changeType',
                                      label = 'Type of change',
                                      choices = list('Mean', 'Variance', 'Mean & Variance')),
                         uiOutput("deltaControls")
                     ),
                     mainPanel(
                        plotOutput("ts")
                     )
                 )
             ),
             fluidRow(
                 sidebarLayout(
                     sidebarPanel(
                         tags$h2("Changepoint Parameters"),
                         uiOutput('cptTypeControls'),
                         selectInput(inputId = 'penalty',
                                     label = 'Penalty Choice',
                                     choices = list('SIC'='SIC', 'BIC'='BIC', 'MBIC'='MBIC', 'AIC'='AIC', 'Hannan-Quinn'='Hannan-Quinn', 'Manual'='Manual'),
                                     selected='MBIC'
                         ),
                         uiOutput('manPen')
                     ),
                     mainPanel(
                         verbatimTextOutput("cptSummary")
                     )
                 )
             )
    ),
    tabPanel('Load Data',
             'Tab for loading data and then doing changepoint analysis'
    ),
    tabPanel('Example Data Sets',
             'A few examples from changpeoint package'
    )
)

server <- function(input, output){
  data = reactive({
    if(input$changeType == 'Mean'){
      c(rnorm(input$tau), rnorm(input$n-input$tau, input$deltaMean))
    }else if(input$changeType == 'Variance'){
      c(rnorm(input$tau), rnorm(input$n-input$tau, 0, sqrt(input$deltaVar)))
    }else{
      c(rnorm(input$tau), rnorm(input$n-input$tau, input$deltaMean, sqrt(input$deltaVar)))
    }
  })
  output$ts = renderPlot({
    ggplot(mapping = aes(x=1:length(data()),y = data()))+
      geom_line()+
      labs(x='Time', y='Value')
  })
  output$cptSummary = renderPrint({
    if(input$cptType == 'Mean'){
      summary(cpt.mean(data(),penalty=input$penalty, pen.value = input$manPen))
    }else if(input$cptType == 'Variance'){
      summary(cpt.var(data(),penalty=input$penalty, pen.value = input$manPen))
    }else{
      summary(cpt.meanvar(data(),penalty=input$penalty, pen.value = input$manPen))
    }
  }) 
  output$cptTypeControls = renderUI({
    selectInput(inputId = 'cptType',
                label = 'Type of change to detect',
                choices = list('Mean', 'Variance', 'Mean & Variance'),
                selected = input$changeType
    )
  })
    
  output$tauControls = renderUI({
    sliderInput(inputId = 'tau',
                label = 'Change Location',
                value = input$n/2,
                min = 2,
                max = input$n-2,
                step = 1)
  })
  output$manPen = renderUI({
    if(input$penalty == 'Manual'){
      numericInput(inputId = 'manPen',
                   label = 'Manual Penalty',
                   value = 1,
                   min = 0, 
                   max = 10000,
                   step =0.001
      )
    }
  })
  output$deltaControls = renderUI({
    if(input$changeType == 'Mean'){
      sliderInput(inputId = 'deltaMean',
                  label = 'Mean change size',
                  value = 2,
                  min=-5,
                  max = 5,
                  step = 0.1)
    }else if(input$changeType == 'Variance'){
      sliderInput(inputId = 'deltaVar',
                  label = 'Variance change size',
                  value = 3,
                  min = 0.1,
                  max = 5,
                  step = 0.1)
    }else{
      tagList(
        sliderInput(inputId = 'deltaMean',
                  label = 'Mean change size',
                  value = 2,
                  min=-5,
                  max = 5,
                  step = 0.1),
        sliderInput(inputId = 'deltaVar',
                  label = 'Variance change size',
                  value = 3,
                  min = 0.1,
                  max = 5,
                  step = 0.1)
      )
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

