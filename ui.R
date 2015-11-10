
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(fluidPage(
  titlePanel("Upload do Arquivo"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Escolha um arquivo CSV',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Cabeçalho', TRUE),
      radioButtons('sep', 'Separador',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      
      numericInput("tempo", 
                   label = h4("Tempo de vida para cálculo da confiabilidade:"), 
                   value = 100),
      
      sliderInput("probabilidade", label = h4("Probabilidade para cálculo do quantil:"),
                  min = 0, max = 1, value = 0.50)
    ),
    mainPanel(
      
        ## Define conjunto de abas
        tabsetPanel(
          
          ## Primeira aba
          tabPanel("Dados",
            h4(textOutput("caption1")),
            checkboxInput(inputId = "pageable", label = "Paginável"),
            conditionalPanel("input.pageable==true",
                           numericInput(inputId = "pagesize",
                                        label = "Linhas por página",value=13,min=1,max=25)),
          
            htmlOutput("raw"),
            value = 1),
          
          ## Segunda aba
          tabPanel("Plot", 
                   h4(textOutput("caption2")),
                   plotOutput("plot1"),
                   h4(textOutput("caption6")),
                   plotOutput("plot2"),
                   h4(textOutput("caption7")),
                   plotOutput("plot3")),
          
          
          ## Terceira aba
          tabPanel("Sumário", 
                   h4(textOutput("caption3")),
                   verbatimTextOutput("summary")),
          
          ## Quarta aba
          tabPanel("Estimativas", 
                   h4(textOutput("caption4")),
                   verbatimTextOutput("regressao"),
                   h4(textOutput("caption5")),
                   verbatimTextOutput("estimativas"),
                   uiOutput("ui"),
                   h4(textOutput("caption8")),
                   verbatimTextOutput("confiabilidade")),
          
          ## Quinta aba
          tabPanel("Testes de Aderência",
                   h4(textOutput("caption10")),
                   plotOutput('plot5'),
                   h4(textOutput("caption11")),
                   verbatimTextOutput('aderencia')),
          
          
          ## Sexta aba
          tabPanel("Gráfico Cullen and Frey",
                   h4(textOutput("caption9")),
                   plotOutput('plot4'),
                   verbatimTextOutput("cullen"))
        )
    )
  )
))