library(shiny)
library(shinythemes)
library(changepoint)
library(ggplot2)

ui <- navbarPage(
  title = "changepoint",
  theme = shinytheme("spacelab"),
  tabPanel('Home',
    h1("Welcome to the interactive shiny app for the", tags$a(href="https://CRAN.R-project.org/package=changepoint", target="_blank", "changepoint"),"package in R"),
    p("This dashboard allows the user to explore changepoint analysis on some simple simulated data, as well as uploading data sets to perfrom changepoint analysis on without the need for any coding."),
    p("Analysis is performed using the", tags$a(href="https://CRAN.R-project.org/package=changepoint", target="_blank", "changepoint"), 'package. See ', tags$a(href='http://www.jstatsoft.org/v58/i03/', target='blank','`Killick R, Eckley IA (2014). "changepoint: An R package for Changepoint Analysis." Journal of Statistical Software, 58(3), 1-19`'), " for more details" ),
    h3("Simulated Data"),
    p("Here the user can create a simple data set with a single changepoint and see the output of changepoint analysis on the data."),
    p("The user can control the following:", tags$ul(tags$li("Length of data"), tags$li("Changepoint Location"), tags$li("Type of Change"), tags$li("Size of Change"))),
    p("Additionally, the user can control the changepoint analysis by changing:", tags$ul(tags$li("Type of change to detect"), tags$li("Penalty Choice"))),
    h3("Load Data"),
    p("Here the user can upload their own .csv file containg a data set they wish to do changepoint analysis upon."),
    p("The user can alter the changepoint analysis to look for:", tags$ul(tags$li("Mean Changes"), tags$li("Variance Changes"), tags$li("Mean and Variance Changes"))),
    p("Additionally, the user can choose between allowing for a single or multiple changepoints along with other parameters for the changepoint analysis")
  ),
             
  tabPanel('Simulated Data',
           plotOutput('ts'),
           hr(),
           column(4,
                  tags$h3("Data Parameters"),
                  sliderInput(inputId = "n",
                              label = "Length of data set",
                              value = 200,
                              min = 20,
                              max = 1000,
                              step = 1),
                  uiOutput("tauControls"),
                  selectInput(inputId = 'changeType',
                              label = 'Type of change',
                              choices = list('Mean', 'Variance', 'Mean & Variance')),
                  uiOutput("deltaControls")
           ),
           column(4,
                  tags$h3("Changepoint Parameters"),
                  uiOutput('cptTypeControls'),
                  selectInput(inputId = 'penalty',
                              label = 'Penalty Choice',
                              choices = list('SIC'='SIC', 'BIC'='BIC', 'MBIC'='MBIC', 'AIC'='AIC', 'Hannan-Quinn'='Hannan-Quinn', 'Manual'='Manual'),
                              selected='MBIC'
                  ),
                  uiOutput('manPen'),
                  tags$h3("Changepoint Results"),
                  verbatimTextOutput("cptSummary")
           ),
           column(4,
                  tags$h3("Plot Options"),
                  checkboxInput("showCpts",
                                label='Show changepoints'
                  ),
                  checkboxInput("showMean",
                                label='Show segment means'
                  )
           )
  ),
  tabPanel('Load Data',
           sidebarLayout(
             sidebarPanel(width=2,
               h3("Upload Files"),
               p("Files should be of type '.csv' and should contain only one column containing the univariate data."),
               fileInput("file1", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ",")
             ),
             mainPanel(width=10,
               plotOutput("contents"),
               column(4,
                      tags$h3("Changepoint Parameters"),
                      uiOutput('cptTypeControlsLoad'),
                      selectInput(inputId = 'methodLoad',
                                  label = 'Changepoint Method',
                                  choices = list('PELT', 'BinSeg', 'AMOC')
                      ),
                      selectInput(inputId = 'penaltyLoad',
                                  label = 'Penalty Choice',
                                  choices = list('SIC'='SIC', 'BIC'='BIC', 'MBIC'='MBIC', 'AIC'='AIC', 'Hannan-Quinn'='Hannan-Quinn', 'Manual'='Manual'),
                                  selected='MBIC'
                      ),
                      uiOutput('binSegMaxLoad'),
                      uiOutput('manPenLoad'),
                      uiOutput('changeTypeWarning')
               ),
               column(4,
                      tags$h3("Changepoint Results"),
                      verbatimTextOutput("cptSummaryLoad")
               ),
               column(4,
                      tags$h3("Plot Options"),
                      checkboxInput("showCptsLoad",
                                    label='Show changepoints'
                      ),
                      checkboxInput("showMeanLoad",
                                    label='Show segment means'
                      )
               )
             )
           )
  )
)

server <- function(input, output){
  data = reactive({
    if(input$changeType == 'Mean'){
      c(rnorm(as.numeric(input$tau)), rnorm(as.numeric(input$n)-as.numeric(input$tau), as.numeric(input$deltaMean)))
    }else if(input$changeType == 'Variance'){
      c(rnorm(as.numeric(input$tau)), rnorm(as.numeric(input$n)-as.numeric(input$tau), 0, sqrt(as.numeric(input$deltaVar))))
    }else{
      c(rnorm(as.numeric(input$tau)), rnorm(as.numeric(input$n)-as.numeric(input$tau), input$deltaMean, sqrt(as.numeric(input$deltaVar))))
    }
  })
  cptAnalysis = reactive({
    if(input$cptType == 'Mean'){
      cptAns = cpt.mean(data(),penalty=input$penalty, pen.value = input$manPen)
    }else if(input$cptType == 'Variance'){
      cptAns = cpt.var(data(),penalty=input$penalty, pen.value = input$manPen)
    }else{
      cptAns = cpt.meanvar(data(),penalty=input$penalty, pen.value = input$manPen)
    }
  }) 
  output$ts = renderPlot({
    p = ggplot(mapping = aes(x=1:length(data()),y = data()))+
      geom_line()+
      labs(x='Time', y='Value')
    if(input$showCpts){
      Cpts = cpts(cptAnalysis())
      p = p + geom_vline(xintercept=Cpts, col='red', size=1.3)
    }
    if(input$showMean){
      Cpts = c(0,cpts(cptAnalysis()),length(data()))
      means = c()
      for(i in 1:(length(Cpts)-1)){
        mean = mean(data()[(Cpts[i]+1):Cpts[i+1]])
        means = c(means, rep(mean,Cpts[i+1]-Cpts[i]))
      }
      p = p + geom_line(aes(x=1:length(data()), y=means), col='blue',size=1.3)
    }
    p
  })
  output$cptSummary = renderPrint({
    summary(cptAnalysis())
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
                  max = 10,
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
                    max = 10,
                    step = 0.1)
      )
    }
  })
  dataLoad = reactive({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    X = df[,1]
  })
    
  output$contents <- renderPlot({
    req(input$file1)
    p = ggplot(mapping=aes(x=1:length(dataLoad()), y=dataLoad()))+
      geom_line()+
      labs(x='Time', y='Value')
    if(input$showCptsLoad){
      Cpts = cpts(cptAnalysisLoad())
      p = p + geom_vline(xintercept=Cpts, col='red', size=1.3)
    }
    if(input$showMeanLoad){
      Cpts = c(0,cpts(cptAnalysisLoad()),length(dataLoad()))
      means = c()
      for(i in 1:(length(Cpts)-1)){
        mean = mean(dataLoad()[(Cpts[i]+1):Cpts[i+1]])
        means = c(means, rep(mean,Cpts[i+1]-Cpts[i]))
      }
      p = p + geom_line(aes(x=1:length(dataLoad()), y=means), col='blue',size=1.3)
    }
    p
  })
  cptAnalysisLoad = reactive({
    req(input$file1)
    if(input$cptTypeLoad == 'Mean'){
      cptAns = cpt.mean(dataLoad(), method=input$methodLoad, penalty=input$penaltyLoad, pen.value = input$manPenLoad, Q=input$maxCptLoad)
    }else if(input$cptTypeLoad == 'Variance'){
      cptAns = cpt.var(dataLoad(), method=input$methodLoad, penalty=input$penaltyLoad, pen.value = input$manPenLoad, Q=input$maxCptLoad)
    }else{
      cptAns = cpt.meanvar(dataLoad(), method=input$methodLoad, penalty=input$penaltyLoad, pen.value = input$manPenLoad, Q=input$maxCptLoad)
    }
  }) 
  output$cptSummaryLoad = renderPrint({
    summary(cptAnalysisLoad())
  }) 
  output$cptTypeControlsLoad = renderUI({
    selectInput(inputId = 'cptTypeLoad',
                label = 'Type of change to detect',
                choices = list('Mean', 'Variance', 'Mean & Variance'),
                selected='Mean & Variance'
    )
  })
  output$changeTypeWarning = renderUI({
    if(input$cptTypeLoad=='Mean'){
      p("In the changepoint methods the variance is assumed to be 1. Use Mean & Variance change if this is not the case")   
    }else if(input$cptTypeLoad=='Variance'){
      p("In the changepoint methods the mean is assumed to be 0. Use Mean & Variance change if this is not the case")   
    }
  })
  output$manPenLoad = renderUI({
    if(input$penaltyLoad == 'Manual'){
      numericInput(inputId = 'manPenLoad',
                   label = 'Manual Penalty',
                   value = 1,
                   min = 0, 
                   max = 10000,
                   step =0.001
      )
    }
  })  
  output$binSegMaxLoad = renderUI({
    if(input$methodLoad == 'BinSeg'){
      numericInput(inputId = 'maxCptLoad',
                   label = 'Max number of changepoints',
                   value = 20,
                   min = 1, 
                   max = 10000,
                   step = 1
      )
    }
  })
}             

# Run the application 
shinyApp(ui = ui, server = server)

