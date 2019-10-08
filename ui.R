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
shinyUI(dashboardPage(skin="blue",
              #Title
              dashboardHeader(title="Probability Application",titleWidth=250,
                              tags$li(class="dropdown",
                                      tags$a(href='https://shinyapps.science.psu.edu/',
                                             icon("home", lib = "font-awesome"))),
                              tags$li(class="dropdown",
                                      actionLink("info",icon("info"),class="myClass"))),
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(id = "tabs",
                            
                            menuItem("Overview", tabName = "information", icon = icon("dashboard")),
                            menuItem("Pre-requisites", tabName = "concepts", icon = icon("book")),
                            menuItem("Game", tabName = "test", icon = icon("gamepad"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
                ),
                tags$style(
                  type = "text/css",
                  ".content-wrapper,.right-side {
                  background-color: white;
                  }"
                ),
                tabItems(
                  tabItem(tabName = "information",
                          tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                          br(),br(),br(),
                          
                          h3(strong("About: ")),
                          h4("This app quizzes your knowledge of distribution application using a hangman game format."),
                          br(),
                          h3(strong("Instructions:")),
                          h4(tags$li("You'll start this game with a little man on the top of a tree, and you are trying to prevent his fall to the ground. If you provide a wrong answer, he falls to a lower branch and eventually to the ground. If you get 10 questions correct before he falls to the ground, you have won the game and saved the little man!")),
                          h4(tags$li("Read the given text before you make your choice. Make sure you understand the scenario text provided.")),
                          h4(tags$li("If you need some extra help, click the 'hint' button (shown as a question mark symbol).")),
                          h4(tags$li("After you select the choice, click 'Submit' to check your answer.")),
                          h4(tags$li("Once you click 'Submit', you cannot revise your answer. You can only click 'Next Question' to move on your challenge.")),
                          
                          div(style = "text-align: center;",
                              bsButton(inputId = "go", label = "G O !", size = "large",icon = icon("bolt"), class="circle grow")
                          ),
                          br(),
                          h3(strong("Acknowledgements:")),
                          h4("This app was developed and coded by Yiyang Wang.")
                          
                  ),
                  
                  ####Pre-requisites Page####
                  tabItem(tabName = "concepts",
                          fluidRow(
                            div(style = "text-align: center;",
                                h3("Here are some concepts you may want to review before doing the practice")
                            )
                          ),
                          withMathJax(),
                          fluidRow(with=12,
                            box(title = strong("General expectation equations"),solidHeader = TRUE, status="primary", width = 12, background = NULL, collapsible=TRUE,
                               column(width=4,
                                 wellPanel(style = "background-color: #d3efff",
                                          fluidRow(width=12, style = "text-align:left",
                                                          h5('$$E[∑{a_{i}X_{i}}]	=	∑{a_{i}E(X_{i}})$$'),
                                                          h5('$$Var[aX+b]=a^2Var(X)$$'),
                                                          h5('$$Var(X) = E[X^2]	– [E(X)]^2$$'),
                                                          h5('$$Cov(aX,bY)=abCov(X,Y)$$'),
                                                          h5('$$Cov(X,Y) = E(XY)-E(X)E(Y)$$')
                                                   ))),
                               column(width=8,
                                wellPanel(style = "background-color: #d3efff",
                                          fluidRow(width=12, style = "text-align:left",
                                                          h5('$$Var(X+Y)=Var(X)+Var(Y)+2Cov(X,Y)$$'),
                                                   
                                                          h5('$$Var(∑X_{i})	=	∑Var(X_{i})	+	2∑∑Cov(X_{i},X_{j})$$')),
                                          fluidRow(width=12, style = "text-align:left",
                                                   column(width=4, 
                                                          h5('$$Mx(t)	=	E[X^{tX}]$$')),
                                                   column(width=4,
                                                          h5("$$Mx^{'}(0)	=	E[X]$$")),
                                                   column(width=4,
                                                          h5("$$Mx^{''}(0) = E[X^2]$$"))),

                                            withMathJax(h5('\\(E[g(X)]	=	∑g(x)p(x)\\)  where	p(x) is the	pmf	of the	discrete	random	variable	X or	=	 g(x)f(x)dx in'))))),
                                          
                                          tags$style(type='text/css', 
                                                     '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                     '.well { padding: 0px; margin-bottom: 15px; max-width: 600000px; }')
                                ),
                            box(title = strong("Discrete random variable"), solidHeader = TRUE, status="primary", width = 6, background = NULL, collapsible=TRUE,
                                wellPanel(style = "background-color: #d3efff",
                                    fluidRow(width=12,
                                        column(width=6,
                                            h5('$$ f(x)	=	P(X=x)$$')),
                                        column(width=6,
                                            h5('$$ F(x) =	P[X≤x]$$'))),
                                        tags$style(type='text/css', 
                                                    '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                    '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }')
                                        ),
                                wellPanel(style = "background-color: #d3efff",
                                          tags$style(type='text/css', 
                                                     '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                     '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                                          h5(strong("Bernoulli random variable with parameter θ:")),
                                          fluidRow(width=12,
                                            column(width=6,
                                              h5("$$f(x) =\\theta^{x}(1-θ)^{1-x}$$"),
                                              h5("$$Var(X)	=	n\\theta(1−\\theta)$$")),
                                            column(width=6,
                                              h5("$$E(X)	=	n\\theta$$"),
                                              h5("$$Mx(t)	=	[(1-\\theta) + θe^{t}$$")))
                                          ),
                                wellPanel(style = "background-color: #d3efff",
                                          tags$style(type='text/css', 
                                                     '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                     '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                                          h5(strong("Binomial random variable with parameters n and θ:")),
                                          fluidRow(width=12,
                                                   column(width=6,
                                                          h5("$$f(x) =	[n!/x!(n-x)!]θ^{x}(1-θ)^{n-x}$$"),
                                                          h5("$$Var(X)	=	n^{θ}(1−θ)$$")),
                                                   column(width=6,
                                                          h5("$$E(X)	=	n^θ$$"),
                                                          h5("$$Mx(t)	=	[(1-\\theta) + θe^{t}]^n$$")))
                                ),
                                wellPanel(style = "background-color: #d3efff",
                                          tags$style(type='text/css', 
                                                     '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                     '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                                          h5(strong("Geometric random variable with parameter θ:")),
                                          fluidRow(width=12,
                                                   column(width=6,
                                                          h5("$$f(x) =θ(1-θ)^{x-1}$$"),
                                                          h5("$$Var(X)	=	\\frac{(1-θ)}{θ^{2}}$$")),
                                                   column(width=6,
                                                          h5("$$E(X)	=	\\frac{1}{θ}$$"),
                                                          h5("$$Mx(t)	=	\\frac{θe^{t}}{[1-(1-θ)e^{t}]}$$")))
                                )),
                            box(title = strong("Continuous random variable"), solidHeader = TRUE, status="primary", width = 6, background = NULL, collapsible=TRUE,
                               wellPanel(style = "background-color: #d3efff",
                                        tags$style(type='text/css', 
                                                  '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                  '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                                        fluidRow(width=12,
                                                 column(width=6,
                                                        h5('$$ f(x)	=	P(X=x)$$')),
                                                 column(width=6,
                                                        div(style = "height:20px;", helpText('$$ F(a) = P(X≤a) = {∫^{a}_{−∞}}f(x)dx$$'))))
                                      ),
                               wellPanel(style = "background-color: #d3efff",
                                         tags$style(type='text/css', 
                                                    '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                    '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                                         h5(strong("Uniform random variable between A and B:")),
                                         fluidRow(width=12,
                                                  column(width=6,
                                                         h5("$$f(x) =	\\frac{1}{(B-A)}$$"),
                                                         h5("$$	Var(X)	=	\\frac{(B-A)^{2}}{12}	$$")),
                                                  column(width=6,
                                                         h5("$$E(X)	=	\\frac{(A+B)}{2}$$"),
                                                         h5("$$Mx(t)	=\\frac{(e^{tB}- e^{tA})}{t(B-A)}$$")))
                                         ),
                               wellPanel(style = "background-color: #d3efff",
                                         tags$style(type='text/css', 
                                                    '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                    '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                                         h5(strong("Normal random variable 	with	mean µ and standard	deviation σ:")),
                                         fluidRow(width=12,
                                                  column(width=6,
                                                          h5("$$φ(x)=\\frac{1}{σ\\sqrt{2\\pi}}exp{[\\frac{-(x-µ)^2}{2σ^2}]}$$"),
                                                         h5("$$	Var(X)	=	σ^2	$$")),
                                                  column(width=6,
                                                         h5("$$E(X)	=	µ	$$"),
                                                         h5("$$Mx(t) = exp(µt + 0.5(σt)^2$$")))
                                         ),
                               wellPanel(style = "background-color: #d3efff",
                                         tags$style(type='text/css', 
                                                    '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                    '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                                         h5(strong("Exponential random variable with parameter λ:")),
                                         fluidRow(width=12,
                                                  column(width=6,
                                                         h5("$$f(x) = λe^{-λx}$$"),
                                                         h5("$$	Var(X)	=	\\frac{1}{λ^{2}}	$$")),
                                                  column(width=6,
                                                         h5("$$E(X)	=	\\frac{1}{λ}$$"),
                                                         h5("$$Mx(t)	=\\frac{λ}{(λ-t)}$$")))
                               )
                            ),
                          br(),
                          div(style = "text-align: center;",
                              bsButton(inputId = "ready", label = "I'm ready!", size = "large",icon = icon("bolt"), class="circle grow")
                          )
                  ),
                
                ######Game Page#######
                  tabItem(tabName = "test",
                          br(),br(),
                          sidebarLayout(
                            sidebarPanel(
                              h3("Challenge"),
                              wellPanel(style = "background-color: #EAF2F8",
                                        uiOutput("question"),
                                        tags$style(type='text/css', '#question {font-weight:bold;font-size: 25px;background-color: #EAF2F8;color: black;}','.well { padding: 12px; margin-bottom: 15px; max-width: 1000px; }')
                              ),
                              h3("Which expression addresses the question?",tags$li(style="display: inline-block;", circleButton("hint",icon = icon("question"), status = "myClass",size = "xs"))),
                              wellPanel(style = "background-color: #EAF2F8", width=8,
                                        
                                        fluidRow(width=12, 
                                                 column(width=1,checkboxInput('answerA','',value=FALSE, width=NULL)),
                                                 column(width=6,uiOutput('ansA'))
                                                 ),
                                        fluidRow(width=12, 
                                                 column(width=1,checkboxInput('answerB','',value=FALSE, width=NULL)),
                                                 column(width=6,uiOutput('ansB'))
                                        ),
                                        fluidRow(width=12, 
                                                 column(width=1,checkboxInput('answerC','',value=FALSE, width=NULL)),
                                                 column(width=6,uiOutput('ansC'))
                                        ),
                                        fluidRow(width=12, 
                                                 column(width=1,checkboxInput('answerD','',value=FALSE, width=NULL)),
                                                 column(width=6,uiOutput('ansD'))
                                        ),
                                        br(),br()

                                        ),

                              fluidRow(width=12,
                                column(1, uiOutput('mark')),
                                column(3,
                                       bsButton('submit', "   Submit   ", size= "large", style="warning",disabled =FALSE)),
                                
                                column(3,
                                       withBusyIndicatorUI(bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE)))
                              ),
                              
                              br(),
                              br(),
                              br(),
                              
                              tags$head(tags$style(HTML("#result {font-size: 17px;background-color:#EAF2F8}"))),
                              
                              width = 6),
                            mainPanel(
                              br(),
                              width = 6,
                              
                              fluidRow(
                                uiOutput("correct", align = 'center')
                              ),
                              
                              br(),
                              br(),
                              
                              fluidRow(
                                uiOutput("distPlot", align = 'center')
                              ),
                              br(),
                              br(),
                              br(),
                              div(style = "text-align: center;",
                                bsButton('restart', "Restart", size ="large", style="warning",disabled=TRUE)
                                )
                            ),
                            position ="left"
                          ))
                ))))


