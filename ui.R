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
          h2(
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
                  style = "text-align:center",
                  p(
                    '\\(\\text{E}\\left[\\sum_i{a_{i}X_{i}}\\right]=\\sum_i{a_{i}\\text{E}\\left[X_{i}\\right]}\\)'
                  ),br(),
                  p(
                    '\\(\\text{Var}\\left[aX+b\\right]=a^2\\text{Var}\\left[X\\right]\\)'
                  ),
                  p(
                    '\\(\\text{Var}\\left[X\\right]=\\text{E}\\left[X^2\\right]-\\left(\\text{E}\\left[X\\right]\\right)^2\\)'
                  ),br(),
                  p('\\(\\text{Cov}(aX,bY)=ab\\text{Cov}(X,Y)\\)'),
                  p(
                    '\\(\\text{Cov}(X,Y)=\\text{E}\\left[XY\\right]-\\text{E}\\left[X\\right]\\cdot\\text{E}\\left[Y\\right]\\)'
                  ),br()
                )
              )
            ),
            column(
              width = 8,
              wellPanel(
                style = "background-color: #d3efff",
                fluidRow(
                  width = 12,
                  style = "text-align:center",
                  p(
                    '\\(\\text{Var}\\left[X+Y\\right]=\\text{Var}\\left[X\\right]+\\text{Var}\\left[Y\\right]+2\\text{Cov}\\left(X,Y\\right)\\)'
                  ),
                  p(
                    '\\(\\text{Var}\\left[\\sum_{i}X_{i}\\right]=\\sum_{i}\\text{Var}\\left[X_{i}\\right]+2\\underset{i\\neq j}{\\sum_{i}\\sum_{j}}\\text{Cov}\\left(X_{i},X_{j}\\right)\\)'
                  )
                ),
                fluidRow(
                  width = 12,
                  style = "text-align:center",
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
                      style = "text-align:center",
                      width = 12,
                      column(width = 6,
                             p('$$ f(x)	=	P(X=x)$$')),
                      column(width = 6,
                             p('$$ F(x) =	P[X≤x]$$'))
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Discrete Uniform distribution on the set\\(\\{x_{i},\\;i=1,2,\\ldots,k\\}\\)")
                    ),
                    fluidRow(
                      width = 12,
                      style = "text-align:center",
                      column(
                        width = 6,
                        
                        p("$$P[X=x_{i}]=1/k$$"),
                        p("$$Var(X)=(k^{2}-1)/12$$")
                        
                      ),
                      column(
                        width = 6,
                        p("$$E(X)=\\frac{k+1}{2}$$")
                      )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Bernoulli random variable with parameter θ:")
                    ),
                    fluidRow(
                      width = 12,
                      style = "text-align:center",
                      column(
                        width = 6,
                        
                        p("$$f(x) =\\theta^{x}(1-θ)^{1-x}$$"),
                        p("$$Var(X)	=	n\\theta(1−\\theta)$$")
                      ),
                      column(
                        width = 6,
                        p("$$E(X)	=	n\\theta$$"),
                        p("$$Mx(t)	=	(1-\\theta) + θe^{t}$$")
                      )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Binomial random variable with parameters n and θ:")
                    ),
                    fluidRow(
                      style = "text-align:center",
                      width = 12,
                      column(
                        width = 6,
                        p("$$f(x) =	[n!/x!(n-x)!]θ^{x}(1-θ)^{n-x}$$"),
                        p("$$Var(X)	=	n^{θ}(1−θ)$$")
                      ),
                      column(
                        width = 6,
                        p("$$E(X)	=	n^θ$$"),
                        p("$$Mx(t)	=	[(1-\\theta) + θe^{t}]^n$$")
                      )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Geometric random variable with parameter θ:")
                    ),
                    fluidRow(
                      style = "text-align:center",
                      width = 12,
                      column(
                        width = 6,
                        p("$$f(x) =θ(1-θ)^{x-1}$$"),
                        p("$$Var(X)	=	\\frac{(1-θ)}{θ^{2}}$$")
                      ),
                      column(
                        width = 6,
                        p("$$E(X)	=	\\frac{1}{θ}$$"),
                        p("$$Mx(t)	=	\\frac{θe^{t}}{[1-(1-θ)e^{t}]}$$")
                      )
                    )),
          #Negative Bionomial
          wellPanel(style = "background-color: #d3efff",
                    tags$style(type='text/css', 
                               '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                               '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                    h5(strong("Negative Bionomial random variable with parameters r and θ:")),
                    fluidRow(width=12,
                             style = "text-align:center",
                             column(width=6,
                                    p("$$f(x) =\\frac{(x-1)!}{(r-1)!(x-r)!}$$"),
                                    p("$$Var(X)	=	\\frac{r(1-θ)}{θ^{2}}$$")),
                             column(width=6,
                                    p("$$E(X)	=	\\frac{r}{θ}$$"),
                                    p("$$Mx(t)	=	(\\frac{θe^{t}}{1-(1-θ)e^{t}})^r$$")))
          ),
          #Poisson
          wellPanel(style = "background-color: #d3efff",
                    tags$style(type='text/css', 
                               '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                               '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                    h5(strong("Poisson random variable with parameter λ:")),
                    fluidRow(width=12,
                             style = "text-align:center",
                             column(width=6,
                                    p("$$f(x) = (\\frac{λ^{x}}{x!})e^{-λ}$$"),
                                    p("$$Var(X)	=	λ$$")),
                             column(width=6,
                                    p("$$E(X)	=	λ$$"),
                                    p("$$Mx(t)	=	exp{λ(e^{t}-1)}$$")))
          )
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
                      style = "text-align:center",
                      column(width = 6,
                             p('$$ f(x)	=	P(X=x)$$')),
                      column(width = 6,
                            p('$$ F(a) = P(X≤a) = {∫^{a}_{−∞}}f(x)dx$$')
                             )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Uniform random variable between A and B:")
                    ),
                    fluidRow(
                      style = "text-align:center",
                      width = 12,
                      column(
                        width = 6,
                        p("$$f(x) =	\\frac{1}{(B-A)}$$"),
                        p("$$	Var(X)	=	\\frac{(B-A)^{2}}{12}	$$")
                      ),
                      column(
                        width = 6,
                        p("$$E(X)	=	\\frac{(A+B)}{2}$$"),
                        p("$$Mx(t)	=\\frac{(e^{tB}- e^{tA})}{t(B-A)}$$")
                      )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Normal random variable 	with	mean µ and standard	deviation σ:")
                    ),
                    fluidRow(
                      style = "text-align:center",
                      width = 12,
                      column(
                        width = 6,
                        p(
                          "$$φ(x)=\\frac{1}{σ\\sqrt{2\\pi}}exp{[\\frac{-(x-µ)^2}{2σ^2}]}$$"
                        ),
                        p("$$	Var(X)	=	σ^2	$$")
                      ),
                      column(width = 6,
                             p("$$E(X)	=	µ	$$"),
                             p("$$Mx(t) = exp(µt + 0.5(σt)^2$$"))
                    )),
          wellPanel(style = "background-color: #d3efff",
                    h5(
                      strong("Exponential random variable with parameter λ:")
                    ),
                    fluidRow(
                      style = "text-align:center",
                      width = 12,
                      column(
                        width = 6,
                        p("$$f(x) = λe^{-λx}$$"),
                        p("$$	Var(X)	=	\\frac{1}{λ^{2}}	$$")
                      ),
                      column(
                        width = 6,
                        p("$$E(X)	=	\\frac{1}{λ}$$"),
                        p("$$Mx(t)	=\\frac{λ}{(λ-t)}$$")
                      )
                    )),
          wellPanel(style = "background-color: #d3efff",
                    tags$style(type='text/css', 
                               '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                               '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                    h5(strong("Gamma random variable with parameters λ and α:")),
                    fluidRow(width=12,
                             style = "text-align:center",
                             column(width=6,
                                    p("$$f(x) =	\\frac{λe^{-λx}(λx)^{α-1}}{Γ(α)}$$"),
                                    p("$$Var(X)	=	\\frac{α}{λ^{2}}$$")),
                             column(width=6,
                                    p("$$E(X)	=	\\frac{α}{λ}$$"),
                                    p("$$Mx(t)	=	[\\frac{λ}{(λ-t)}]^{α}$$")))
          ),
          wellPanel(style = "background-color: #d3efff",
                    tags$style(type='text/css', 
                               '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                               '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                    h5(strong("Chi-square random	variable	with	k	degrees	of freedom:")),
                    fluidRow(width=12,
                             style = "text-align:center",
                             column(width=6,
                                    p("$$f(x) =	\\frac{0.5e^{\\frac{-x}{2}}(0.5x)^{\\frac{k-2}{2}}}{Γ(k/2)}$$"),
                                    p("$$Var(X)	=	2k$$")),
                             column(width=6,
                                    p("$$E(X)	=	k$$"),
                                    p("$$Mx(t)	=	{1-2t}^{\\frac{-k}{2}}$$")))
          )
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
                #style = "text-align:center",
                width = 12,
                withMathJax(),
                column(12, offset=0,
                radioGroupButtons(
                  inputId = "mc1",
                  label = NULL,
                  status = 'game',
                  direction = 'vertical',
                  selected = NULL,
                  checkIcon = list(yes = icon("check-square"),
                                   no = icon("square-o")),
                  choices = list("\\(\\frac{1}{4}\\)",
                                 "\\(\\frac{2}{4}\\)",
                                 "\\(\\frac{3}{4}\\)",
                                 "\\(\\frac{4}{4}\\)"),
                  width = '100%',
                  justified = FALSE,
                  individual = FALSE
                ))
              ),
              uiOutput('test1'),
              uiOutput('test2')
            ),     
            fluidRow(
              width = 12,
              column(1, uiOutput('mark')),
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
              column(3,
                       bsButton(
                         'nextq',
                         "Next Question",
                         size = "large",
                         style = "success",
                         disabled = TRUE
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
                offset = 1
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
            fluidRow(uiOutput("distPlot", align = 'center'))
          )
        )
      )
    )
  )
))
