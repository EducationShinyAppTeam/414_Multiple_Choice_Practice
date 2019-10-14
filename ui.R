library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)
library(shinyjs)
library(V8)
library(discrimARTs)
library(leaflet)
library(raster)
library(shinyWidgets)
source("helpers.R")


shinyUI(dashboardPage(
  skin = "blue",
  #Title
  dashboardHeader(
    title = "Probability Application",
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(href = 'https://shinyapps.science.psu.edu/',
             icon("home", lib = "font-awesome"))
    ),
    tags$li(class = "dropdown",
            actionLink("info", icon("info"), class =
                         "myClass"))
  ),
  #Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      
      menuItem("Overview", tabName = "information", icon = icon("dashboard")),
      menuItem(
        "Pre-requisites",
        tabName = "concepts",
        icon = icon("book")
      ),
      menuItem("Game", tabName = "test", icon = icon("gamepad"))
    )
  ),
  
  #Content within the tabs
  dashboardBody(
    tags$head(
      #tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "boast.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
    ),
    tabItems(
      tabItem(
        tabName = "information",
        tags$a(
          href = 'http://stat.psu.edu/',
          tags$img(src = 'PS-HOR-RGB-2C.png', align = "left", width = 180)
        ),
        br(),
        br(),
        br(),
        
        h3(strong("About: ")),
        p(
          "This app quizzes your knowledge of distribution application using a hangman game format."
        ),
        br(),
        h3(strong("Instructions:")),
        tags$ul(
          tags$li(
            "You'll start this game with a little man on the top of a tree, and you are trying to prevent his fall to the ground. If you provide a wrong answer, he falls to a lower branch and eventually to the ground. If you get 10 questions correct before he falls to the ground, you have won the game and saved the little man!"
          ),
          tags$li(
            "Read the given text before you make your choice. Make sure you understand the scenario text provided."
          ),
          tags$li(
            "If you need some extra help, click the 'hint' button (shown as a question mark symbol)."
          ),
          tags$li("After you select the choice, click 'Submit' to check your answer."),
          tags$li(
            "Once you click 'Submit', you cannot revise your answer. You can only click 'Next Question' to move on your challenge."
          )
        ),
        
        div(
          style = "text-align: center;",
          bsButton(
            inputId = "go",
            label = "GO!",
            size = "large",
            icon = icon("bolt"),
            class = "circle grow"
          )
        ),
        br(),
        h3(strong("Acknowledgements:")),
        p("This app was developed and coded by Yiyang Wang.")
        
      ),
      
      ####Pre-requisites Page####
      tabItem(
        tabName = "concepts",
        withMathJax(),
        fluidRow(div(
          style = "text-align: center;",
          h3(
            "Here are some concepts you may want to review before doing the practice"
          )
        )),
        fluidRow(
          with = 12,
          box(
            title = strong("General expectation equations"),
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            background = NULL,
            collapsible = TRUE,
            column(
              width = 4,
              wellPanel(
                style = "background-color: #d3efff",
                fluidRow(
                  width = 12,
                  style = "text-align:left",
                  p(
                    '\\(\\text{E}\\left[\\sum_i{a_{i}X_{i}}\\right]=\\sum_i{a_{i}\\text{E}\\left[X_{i}\\right]}\\)'
                  ),
                  p(
                    '\\(\\text{Var}\\left[aX+b\\right]=a^2\\text{Var}\\left[X\\right]\\)'
                  ),
                  p(
                    '\\(\\text{Var}\\left[X\\right]=\\text{E}\\left[X^2\\right]-\\left(\\text{E}\\left[X\\right]\\right)^2\\)'
                  ),
                  p('\\(\\text{Cov}(aX,bY)=ab\\text{Cov}(X,Y)\\)'),
                  p(
                    '\\(\\text{Cov}(X,Y)=\\text{E}\\left[XY\\right]-\\text{E}\\left[X\\right]\\cdot\\text{E}\\left[Y\\right]\\)'
                  )
                )
              )
            ),
            column(
              width = 8,
              wellPanel(
                style = "background-color: #d3efff",
                fluidRow(
                  width = 12,
                  style = "text-align:left",
                  p(
                    '\\(\\text{Var}\\left[X+Y\\right]=\\text{Var}\\left[X\\right]+\\text{Var}\\left[Y\\right]+2\\text{Cov}\\left(X,Y\\right)\\)'
                  ),
                  p(
                    '\\(\\text{Var}\\left[\\sum_{i}X_{i}\\right]=\\sum_{i}\\text{Var}\\left[X_{i}\\right]+2\\underset{i\\neq j}{\\sum_{i}\\sum_{j}}\\text{Cov}\\left(X_{i},X_{j}\\right)\\)'
                  )
                ),
                fluidRow(
                  width = 12,
                  style = "text-align:left",
                  column(width = 4,
                         p(
                           '\\(M_X(t)=\\text{E}\\left[e^{tX}\\right]\\)'
                         )),
                  column(width = 4,
                         p(
                           "\\(M'_X(0)=\\text{E}\\left[X\\right]\\)"
                         )),
                  column(width = 4,
                         p(
                           "\\(M''_X(0)=\\text{E}\\left[X^2\\right]\\)"
                         ))
                ),
                p(
                  "Transformations of Random Variablies using any function, \\(g\\)."
                ),
                p(
                  'Discrete Case: \\(\\text{E}\\left[g(X)\\right]=\\sum\\limits_{x\\in\\mathcal{X}}g(x)p(x)\\)'
                ),
                p(
                  'Continuous Case:  \\(\\text{E}\\left[g(X)\\right]=\\int\\limits_{\\mathcal{X}}g(x)f(x)dx\\)'
                )
              )
            )
          )
        ),
        box(
          title = strong("Discrete random variable"),
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          background = NULL,
          collapsible = TRUE,
          wellPanel(style = "background-color: #d3efff",
                    fluidRow(
                      width = 12,
                      column(width = 6,
                             h5('$$ f(x)	=	P(X=x)$$')),
                      column(width = 6,
                             h5('$$ F(x) =	P[X≤x]$$'))
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Bernoulli random variable with parameter θ:")
                    ),
                    fluidRow(
                      width = 12,
                      column(
                        width = 6,
                        h5("$$f(x) =\\theta^{x}(1-θ)^{1-x}$$"),
                        h5("$$Var(X)	=	n\\theta(1−\\theta)$$")
                      ),
                      column(
                        width = 6,
                        h5("$$E(X)	=	n\\theta$$"),
                        h5("$$Mx(t)	=	[(1-\\theta) + θe^{t}$$")
                      )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Binomial random variable with parameters n and θ:")
                    ),
                    fluidRow(
                      width = 12,
                      column(
                        width = 6,
                        h5("$$f(x) =	[n!/x!(n-x)!]θ^{x}(1-θ)^{n-x}$$"),
                        h5("$$Var(X)	=	n^{θ}(1−θ)$$")
                      ),
                      column(
                        width = 6,
                        h5("$$E(X)	=	n^θ$$"),
                        h5("$$Mx(t)	=	[(1-\\theta) + θe^{t}]^n$$")
                      )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Geometric random variable with parameter θ:")
                    ),
                    fluidRow(
                      width = 12,
                      column(
                        width = 6,
                        h5("$$f(x) =θ(1-θ)^{x-1}$$"),
                        h5("$$Var(X)	=	\\frac{(1-θ)}{θ^{2}}$$")
                      ),
                      column(
                        width = 6,
                        h5("$$E(X)	=	\\frac{1}{θ}$$"),
                        h5("$$Mx(t)	=	\\frac{θe^{t}}{[1-(1-θ)e^{t}]}$$")
                      )
                    ))
        ),
        box(
          title = strong("Continuous random variable"),
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          background = NULL,
          collapsible = TRUE,
          wellPanel(style = "background-color: #d3efff",
                    fluidRow(
                      width = 12,
                      column(width = 6,
                             h5('$$ f(x)	=	P(X=x)$$')),
                      column(width = 6,
                             div(
                               style = "height:20px;", helpText('$$ F(a) = P(X≤a) = {∫^{a}_{−∞}}f(x)dx$$')
                             ))
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Uniform random variable between A and B:")
                    ),
                    fluidRow(
                      width = 12,
                      column(
                        width = 6,
                        h5("$$f(x) =	\\frac{1}{(B-A)}$$"),
                        h5("$$	Var(X)	=	\\frac{(B-A)^{2}}{12}	$$")
                      ),
                      column(
                        width = 6,
                        h5("$$E(X)	=	\\frac{(A+B)}{2}$$"),
                        h5("$$Mx(t)	=\\frac{(e^{tB}- e^{tA})}{t(B-A)}$$")
                      )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Normal random variable 	with	mean µ and standard	deviation σ:")
                    ),
                    fluidRow(
                      width = 12,
                      column(
                        width = 6,
                        h5(
                          "$$φ(x)=\\frac{1}{σ\\sqrt{2\\pi}}exp{[\\frac{-(x-µ)^2}{2σ^2}]}$$"
                        ),
                        h5("$$	Var(X)	=	σ^2	$$")
                      ),
                      column(width = 6,
                             h5("$$E(X)	=	µ	$$"),
                             h5("$$Mx(t) = exp(µt + 0.5(σt)^2$$"))
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Exponential random variable with parameter λ:")
                    ),
                    fluidRow(
                      width = 12,
                      column(
                        width = 6,
                        h5("$$f(x) = λe^{-λx}$$"),
                        h5("$$	Var(X)	=	\\frac{1}{λ^{2}}	$$")
                      ),
                      column(
                        width = 6,
                        h5("$$E(X)	=	\\frac{1}{λ}$$"),
                        h5("$$Mx(t)	=\\frac{λ}{(λ-t)}$$")
                      )
                    ))
        ),
        br(),
        div(
          style = "text-align: center;",
          bsButton(
            inputId = "ready",
            label = "I'm ready!",
            size = "large",
            icon = icon("bolt"),
            class = "circle grow"
          )
        )
      ),
      
      ######Game Page#######
      tabItem(
        tabName = "test",
        withMathJax(),
        sidebarLayout(
          sidebarPanel(
            h3("Challenge"),
            wellPanel(style = "background-color: #EAF2F8",
                      uiOutput("question")),
            h4(
              "Which expression addresses the question?",
              tags$li(
                style = "display: inline-block;",
                circleButton(
                  "hint",
                  icon = icon("question"),
                  status = "myClass",
                  size = "xs"
                )
              )
            ),
            wellPanel(
              style = "background-color: #EAF2F8",
              width = 8,
              
              fluidRow(
                width = 12,
                withMathJax(),
                radioGroupButtons(
                  inputId = "mc1",
                  label = "Select the best answer",
                  status = 'game',
                  direction = 'vertical',
                  checkIcon = list(yes = icon("check-square"),
                                   no = icon("square-o")),
                  choices = list("\\(\\frac{1}{4}\\)",
                                 "\\(\\frac{2}{4}\\)",
                                 "\\(\\frac{3}{4}\\)",
                                 "\\(\\frac{4}{4}\\)"),
                  width = '100%',
                  justified = FALSE,
                  individual = FALSE
                )
              ),
              uiOutput('test1'),
              uiOutput('test2'),
            ),     
            fluidRow(
              width = 12,
              column(
                1,
                bsButton(
                  'submit',
                  "   Submit   ",
                  size = "large",
                  style = "warning",
                  disabled = FALSE
                ),
                offset = 0
              ),
              column(4,
                     withBusyIndicatorUI(
                       bsButton(
                         'nextq',
                         "Next Question",
                         size = "large",
                         style = "success",
                         disabled = TRUE
                       )
                     ),
                     offset = 2
                     ),
              column(
                3,
                bsButton(
                  'restart',
                  "Restart",
                  size = "large",
                  style = "warning",
                  disabled = FALSE
                ),
                offset = 0
              )
            ),
            tags$head(
            tags$style(
              HTML("#result {font-size: 17px;background-color:#EAF2F8}")
            )),
            width = 6
          ),
          mainPanel(
            br(),
            width = 6,
            fluidRow(uiOutput("correct", align = 'center')),
            br(),
            br(),
            fluidRow(uiOutput("distPlot", align = 'center')),
          )
        )
      )
    )
  )
))
