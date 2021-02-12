library(shiny)
library(changepoint)

ui <- navbarPage(title = "changepoint",
    tabPanel('Home',"Home Page"),
    tabPanel('Simulated Data',
             fluidRow(
                 sidebarLayout(
                     sidebarPanel(
                         tags$h1("Data Parameters"),
                         sliderInput(inputId = "n",
                                     label = "Length of data set",
                                     value = 200,
                                     min = 20,
                                     max = 2000,
                                     step = 1),
                         uiOutput("tauControls"),
                         selectInput(inputId = 'changeType',
                                      label = 'Type of change',
                                      choices = list('Mean' = 'mean', 'Variance' = 'var', 'Mean & Variance' = 'meanVar')),
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
                         tags$h1("Changepoint Parameters"),
                         selectInput(inputId = 'penalty',
                                     label = 'Penalty Choice',
                                     choices = list('SIC'='SIC', 'BIC'='BIC', 'MBIC'='MBIC', 'AIC'='AIC', 'Hannan-Quinn'='Hannan-Quinn'),
                                     selected='MBIC'
                         ),
                         actionButton(inputId = 'cptEval',
                                      label = 'Evaluate Changepoints')
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
    if(input$changeType == 'mean'){
      c(rnorm(1:input$tau), rnorm((input$tau+1):input$n, input$deltaMean))
    }else if(input$changeType == 'var'){
      c(rnorm(1:input$tau), rnorm((input$tau+1):input$n, 0, input$deltaVar^2))
    }else{
      c(rnorm(1:input$tau), rnorm((input$tau+1):input$n, input$deltaMean, input$deltaVar^2))
    }
  })
  output$ts = renderPlot({
    ts.plot(data())
  })
  cptSum = eventReactive(input$cptEval,{
    if(input$changeType == 'mean'){
      summary(cpt.mean(data(),penalty=input$penalty))
    }else if(input$changeType == 'var'){
      summary(cpt.var(data(),penalty=input$penalty))
    }else{
      summary(cpt.meanvar(data(),penalty=input$penalty))
    }
  })
  output$cptSummary = renderPrint({
    cptSum()
  }) 
  output$tauControls = renderUI({
    sliderInput(inputId = 'tau',
                label = 'Change Location',
                value = input$n/2,
                min = 2,
                max = input$n-2,
                step = 1)
  })
  output$deltaControls = renderUI({
    if(input$changeType == 'mean'){
      sliderInput(inputId = 'deltaMean',
                  label = 'Mean change size',
                  value = 2,
                  min=-5,
                  max = 5,
                  step = 0.1)
    }else if(input$changeType == 'var'){
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

