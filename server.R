library(RColorBrewer)
library(shiny)
library(shinyjs)
library(raster)
library(rgdal)
library(leaflet)
library(shiny)
library(DT)
library(shinyjs)
library(shinyBS)
library(V8)

shinyServer(function(input, output, session) {
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Pick a answer and click 'submit' to see if it's right, then click 'next' to continue.",
      type = "info"
    )
  })
  
  ###QUESTION BANK###
  bank <- read.csv('practicee.csv', stringsAsFactors = FALSE)
  Qs <- nrow(bank)
  value <- reactiveValues(index =  1, mistake = 0,correct = 0)
  hint <- as.matrix(bank[1:12,3])
  
  ###Hangman game###
  output$distPlot <- renderUI({
    img(src = "Cell01.jpg")
       if(value[["mistake"]] == 0){
         img(src = "Cell01.jpg")
       }
      
      else if(value[["mistake"]] ==1 ) {
        img(src = "Cell02.jpg")
      }
      
      else if(value[["mistake"]] == 2) {
        img(src="Cell03.jpg")
      }
      
      else if(value[["mistake"]] ==3 ) {
        img(src="Cell04.jpg")
      }
      
      else if(value[["mistake"]] ==4) {
        img(src= "Cell05.jpg")
      }
  })

  #### go button ####
  observeEvent(input$go, priority=1, {
    updateTabItems(session, "tabs", "concepts")
  })

  ### ready button###
   observeEvent(input$ready, priority=1, {
     updateTabItems(session, "tabs", "test")
   })
  
  ####start over; new game####
  observeEvent(input$restart,{
    
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"restart",disabled =FALSE)
    
    output$question <- renderUI({
      return(NULL)
    })
    
    observeEvent(input$hint,{
      h4("please select the distribution")
    })
    
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    value[["mistake"]] <<-0
    value$correct <<- 0
  })
  
  ####PRINT NUMBER OF CORRECT ANSWERS####
  output$correct <- renderUI({
    
    h3("Number of correct answers:","",value$correct )
  })
  
  ###SUBMIT BUTTON###
  observeEvent(input$submit,{ 
    output$mark <- renderUI({
      correct_answer <- bank[id,9]
      selected = input$choices
      #if (!is.null(input$selected)){
      if (selected == correct_answer){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
}})})

  ######## QUESTIONS #########
  
  #### random question ####
  numbers <- reactiveValues(question = c())
  observeEvent(input$image_click, once=TRUE, priority = 9, {
    
    numbers$question[1] <- sample(1:Qs, 1)
    updateButton(session, 'nextq', style = "color: white;
                 background-color: #1C2C5B;", disabled = TRUE)
    output$directions <- renderText({"Now answer the question and press submit"})
  })


  ####output random question####
  output$question <- renderUI({
    id <<-sample(1:1, 1, replace = FALSE, prob = NULL)

    if (id==1){
      str1<-withMathJax('$$1-{\\phi(\\frac{4.3-5}{0.5})}^2$$')[2]
      print(length(str1))
      updateRadioButtons(session, "choices", label=NULL, choices = NULL,
                         choiceNames=c(withMathJax('\\(\\Phi(4.5)\\)')[2], withMathJax('$${\\Phi(\\frac{4.3-5}{0.5})}^2$$')[2], withMathJax('$$\\Phi(\\frac{4.3-5}{0.5})$$')[2], withMathJax('$$1-{\\Phi(\\frac{4.3-5}{0.5})}^2$$')[2]), 
                         choiceValues = c(withMathJax('A')[2], withMathJax('B')[2],withMathJax('C')[2], withMathJax('D')[2]), selected = NULL)
    }
    if (id==2){
      updateRadioButtons(session, "choices", label=NULL, choices = NULL,
                         choiceNames=c(withMathJax('$$φ(4.5)-φ(4.0)$$')[2], withMathJax('$${∫^{4.5}_{4}}e^{-5x}dx$$'), withMathJax('$${∫^{-1}_{-2}}e^{-5x}dx$$'), withMathJax('$$φ(\\frac{4.3-5}{0.5})-φ(\\frac{4.0-5}{0.5})$$')), 
                         choiceValues = c('A', withMathJax('B'), withMathJax('C'), withMathJax('D')), selected = NULL)
    }
    if (id==3){
      updateRadioButtons(session, "choices", label=NULL, choices = NULL,
                         choiceNames=c(withMathJax('$${∫^{-0.5}_{-1}}e^{-5x}dx$$'), withMathJax('$$φ(\\frac{4.75-5}{0.5})-φ(\\frac{4.5-5}{0.5})$$'), withMathJax('$$φ(\\frac{4.75-5}{0.5/\\sqrt{4}})-φ(\\frac{4.5-5}{0.5/\\sqrt{4}})$$'), withMathJax('$${∫^{-1}_{-2}}e^{-5x}$$')), 
                         choiceValues = c(withMathJax('A'), withMathJax('B'), withMathJax('C'), withMathJax('D')), selected = NULL)
    }
    return(withMathJax(h4(bank[id, 2])))
  })

  observeEvent(input$go, {
    updateButton(session, "submit", disabled = FALSE)
  })
  observeEvent(input$go, {
    updateButton(session, "nextq", disabled = FALSE)
  })
  
  ####CHECK ANSWERS####
  observeEvent(input$submit, {
     correct_answer <- bank[id,9]
     selected = input$choices
     #if (!is.null(input$selected)){
       if (selected == correct_answer){
         print('correct')
         value$correct <<- value$correct + 1
       }
       else{
         print('wrong')
         value[["mistake"]] <<- value[["mistake"]]+1
         }
       #}
     updateButton(session, "submit", disabled = TRUE)
     updateButton(session, "nextq", disabled = FALSE)
     })
  
  ###PRINT HINTS###
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Hints:",
      type = "info",
      closeOnClickOutside = TRUE,
      h4(bank[id,3])
    )
  })
  
  ###NEXT QUESTION BUTTON###
  observeEvent(input$nextq,{
    id <<-sample(2:3, 1, replace = FALSE, prob = NULL)
    hint<-bank[id,3]
    output$question <- renderUI({
      return(bank[id,2])
    })
    if (id==1){
      updateRadioButtons(session, "choices", label=NULL, choices = NULL,
                         choiceNames=c(withMathJax('$$\\Phi(4.5)$$'), withMathJax('$${\\Phi(\\frac{4.3-5}{0.5})}^2$$'), withMathJax('$$φ(\\frac{4.3-5}{0.5})$$'), withMathJax('$$1-{φ(\\frac{4.3-5}{0.5})}^2$$')), 
                         choiceValues = c(withMathJax('A'), withMathJax('B'), withMathJax('C'), withMathJax('D')), selected = NULL)
    }
    if (id==2){
      updateRadioButtons(session, "choices", label=NULL, choices = NULL,
                         choiceNames=c(withMathJax('$$\\Phi(4.5)-(4.0)$$')[2], withMathJax('$${∫^{4.5}_{4}}e^{-5x}dx$$')[2], withMathJax('$${∫^{-1}_{-2}}e^{-5x}dx$$')[2], withMathJax('$$\\Phi(\\frac{4.3-5}{0.5})-\\Phi(\\frac{4.0-5}{0.5})$$')), 
                         choiceValues = c('A', 'B', 'C', withMathJax('D')), selected = NULL)
    }
    if (id==3){
      updateRadioButtons(session, "choices", label=NULL, choices = NULL,
                         choiceNames=c(withMathJax('$${∫^{-0.5}_{-1}}e^{-5x}dx$$')[2], withMathJax('$$\\Phi(\\frac{4.75-5}{0.5})-\\Phi(\\frac{4.5-5}{0.5})$$')[2], withMathJax('$$\\Phi(\\frac{4.75-5}{0.5/\\sqrt{4}})-\\Phi(\\frac{4.5-5}{0.5/\\sqrt{4}})$$')[2],withMathJax('$${∫^{-1}_{-2}}e^{-5x}$$')), 
                         choiceValues = c('A', 'B', 'C', withMathJax('D')), selected = NULL)
    }
    updateButton(session, "submit", disabled = FALSE)
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
  })
  
})