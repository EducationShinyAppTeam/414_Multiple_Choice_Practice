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

shinyUI(dashboardPage(skin="blue",
              #Title
              dashboardHeader(title="Practice",titleWidth=250,
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
                            menuItem("Concept Check Game", tabName = "test", icon = icon("gamepad"))
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
                          h4(tags$li("There are some instructions")),
                          # h4(tags$li("Click the tic-tac-toe image to begin.")),
                          # h4(tags$li("To play the game, you will select the square that you want to place an X.")),
                          # h4(tags$li("Then you will answer the question that is given, if you get it right, an X will go in the square you selected - if not, an O will go in that spot.")),
                          # h4(tags$li("You can only submit an answer after choosing a spot on the image.")),
                          # h4(tags$li("You are playing as the X's, the object of the game is to get 3 X's in a row. (i.e., When you have 3 X's line up horizontally, vertically, or diagonally).")),
                       
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
                            #theme = "Feature.css",
                            box(title = strong("General expectation equations"),solidHeader = TRUE, status="primary", width = 12, background = NULL, collapsible=TRUE,
                                wellPanel(style = "background-color: #d3efff",with=12,
                                          fluidRow(width=12,
                                                   column(width=3,
                                                          h5('$$E[∑{a_{i}X_{i}}]	=	∑{a_{i}E(X_{i}})$$')),
                                                   column(width=3,
                                                          h5('$$Var[aX+b]=a^2Var(X)$$')),
                                                   column(width=3,
                                                          h5('$$Var(X) = E[X^2]	– [E(X)]^2$$')),
                                                   column(width=3,
                                                          h5('$$Cov(aX,bY)=abCov(X,Y)$$'))),
                                          fluidRow(width=12, style = "text-align:left",
                                                   column(width=4, 
                                                          h5('$$Cov(X,Y) = E(XY)-E(X)E(Y)$$')),
                                                   column(width=4,
                                                          h5('$$Var(X+Y)=Var(X)+Var(Y)+2Cov(X,Y)$$')),
                                                   column(width=4,
                                                          h5('$$Var(∑X_{i})	=	∑Var(X_{i})	+	2∑∑Cov(X_{i},X_{j})$$'))),
                                          fluidRow(width=12, style = "text-align:left",
                                                   column(width=4, 
                                                          h5('$$Mx(t)	=	E[X^{tX}]$$')),
                                                   column(width=4,
                                                          h5("$$Mx^{'}(0)	=	E[X]$$")),
                                                   column(width=4,
                                                          h5("$$Mx^{''}(0) = E[X^2]$$"))),

                                            withMathJax(h5('\\(E[g(X)]	=	∑g(x)p(x)\\)  where	p(x) is the	pmf	of the	discrete	random	variable	X or	=	 g(x)f(x)dx in'))
                                          ),
                                          tags$style(type='text/css', 
                                                     '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                     '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }')
                                )),
                            box(title = strong("Discrete random variable"), solidHeader = TRUE, status="primary", width = 6, background = NULL, collapsible=TRUE,
                                wellPanel(style = "background-color: #d3efff",
                                    #h4(strong("General expectaction equations :")),
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
                                          h5(strong("Bernoulli random variable with parameter θ")),
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
                                                         h5("$$f(x) =	1/(B-A)$$"),
                                                         h5("$$	Var(X)	=	(B-A)^{2}/12	$$")),
                                                  column(width=6,
                                                         h5("$$E(X)	=	(A+B)/2$$"),
                                                         h5("$$Mx(t)	=(e^{tB}- e^{tA})/t(B-A)$$")))
                                         ),
                               wellPanel(style = "background-color: #d3efff",
                                         tags$style(type='text/css', 
                                                    '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}',
                                                    '.well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }'),
                                         h5(strong("Normal random variable 	with	mean µ and standard	deviation σ:")),
                                         fluidRow(width=12,
                                                  column(width=6,
                                                         h5("$$φ(x) =	{(σ\\sqrt{2pi}}exp{-(x-µ)^{2}/2σ^2)$$"),
                                                         h5("$$	Var(X)	=	σ^2	$$")),
                                                  column(width=6,
                                                         h5("$$E(X)	=	µ	$$"),
                                                         h5("$$Mx(t) = exp(µt + 0.5(σt)^2$$")))
                                         )
                            ),
                          br(),br(),br(),br(),br(),
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
                              wellPanel(style = "background-color: #EAF2F8",
                                        radioButtons("choices", "",c("choice 1", "choice 2", "choice 3", "choice 4"),
                                                     selected = NULL)),
                              fluidRow(width=12,
                                column(1, uiOutput('mark')),
                                column(3,
                                       bsButton('submit', "   Submit   ", size= "large", style="warning",disabled =FALSE)),
                                
                                column(2,
                                       bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE))
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
                              br()
                            ),
                            position ="left"
                          ))
                ))))


