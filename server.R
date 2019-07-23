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
source("helpers.R")
shinyServer(function(input, output, session) {
  ###Variables starting value###
  selected <<-c()
  correct_answer<<-c()
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Pick a answer and click 'submit' to see if it's right, then click 'next' to continue.",
      type = "info"
    )
  })
  
  ###QUESTION BANK###
  bank <- read.csv('complete.csv', stringsAsFactors = FALSE)
  Qs <- nrow(bank)
  Qs_array<-c(1:Qs)
  value <- reactiveValues(index =  1, mistake = 0,correct = 0)
  hint <- as.matrix(bank[1:Qs,3])
  
  #### go button ####
  observeEvent(input$go, priority=1, {
    updateTabItems(session, "tabs", "concepts")
  })
  observeEvent(input$go, {
    updateButton(session, "submit", disabled = FALSE)
  })
  observeEvent(input$go, {
    updateButton(session, "nextq", disabled = FALSE)
  })

  ### ready button###
   observeEvent(input$ready, priority=1, {
     updateTabItems(session, "tabs", "test")
   })
  
  ####start over; new game####
  observeEvent(input$restart,{
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = FALSE)
    updateButton(session,"restart",disabled =TRUE)
    updateCheckboxInput(session,'answerA', label=NULL, value=FALSE)
    updateCheckboxInput(session,'answerB', label=NULL, value=FALSE)
    updateCheckboxInput(session,'answerC', label=NULL, value=FALSE)
    updateCheckboxInput(session,'answerD', label=NULL, value=FALSE)
    Qs <<- nrow(bank)
    Qs_array<<-c(1:Qs)
    output$question <- renderUI({
      id<<-1
      output$ansA <- renderUI({
        withMathJax(bank[id,4])
      })
      output$ansB<- renderUI({
        withMathJax(bank[id,5])
      })
      output$ansC<- renderUI({
        withMathJax(bank[id,6])
      })
      output$ansD<- renderUI({
        withMathJax(bank[id,7])
      })
      hint<<-bank[id,3]
      return(bank[id, 2])
    })
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    value[["mistake"]] <<-0
    value$correct <<- 0
  })
  
  ####PRINT OUT RANDOM QUESTION####
  output$question <- renderUI({
    id <<-sample(Qs_array, 1, replace = FALSE, prob = NULL)
    Qs_array<<-Qs_array[!Qs_array %in% id]
    output$ansA <- renderUI({
      withMathJax(bank[id,4])
    })
    output$ansB<- renderUI({
      withMathJax(bank[id,5])
    })
    output$ansC<- renderUI({
      withMathJax(bank[id,6])
    })
    output$ansD<- renderUI({
      withMathJax(bank[id,7])
    })
    hint<<-bank[id,3]
    return(bank[id, 2])
  })
  
  ###NEXT QUESTION BUTTON###
  observeEvent(input$nextq,{
    
    if (length(Qs_array)>1){
      id <<-sample(Qs_array, 1, replace = FALSE, prob = NULL)
      Qs_array<<-Qs_array[!Qs_array %in% id]
      hint<<-bank[id,3]
      output$question <- renderUI({
        return(bank[id,2])
      })
      updateCheckboxInput(session,'answerA', label=NULL, value=FALSE)
      updateCheckboxInput(session,'answerB', label=NULL, value=FALSE)
      updateCheckboxInput(session,'answerC', label=NULL, value=FALSE)
      updateCheckboxInput(session,'answerD', label=NULL, value=FALSE)
      withBusyIndicatorServer('nextq',{
        output$ansA <- renderUI({
          withMathJax(bank[id,4])
        })
        output$ansB<- renderUI({
          withMathJax(bank[id,5])
        })
        output$ansC<- renderUI({
          withMathJax(bank[id,6])
        })
        output$ansD<- renderUI({
          withMathJax(bank[id,7])
        })
        updateButton(session, "submit", disabled = FALSE)
        output$mark <- renderUI({
          img(src = NULL,width = 30)
        })
    })}
    else if(length(Qs_array)==1){
       id<<-Qs_array[1]
       Qs_array<<-Qs_array[!Qs_array %in% id]
       hint<<-bank[id,3]
       output$question <- renderUI({
         return(bank[id,2])
       })
       updateCheckboxInput(session,'answerA', label=NULL, value=FALSE)
       updateCheckboxInput(session,'answerB', label=NULL, value=FALSE)
       updateCheckboxInput(session,'answerC', label=NULL, value=FALSE)
       updateCheckboxInput(session,'answerD', label=NULL, value=FALSE)
       withBusyIndicatorServer('nextq',{
         output$ansA <- renderUI({
           withMathJax(bank[id,4])
         })
         output$ansB<- renderUI({
           withMathJax(bank[id,5])
         })
         output$ansC<- renderUI({
           withMathJax(bank[id,6])
         })
         output$ansD<- renderUI({
           withMathJax(bank[id,7])
         })
         updateButton(session, "submit", disabled = FALSE)
         output$mark <- renderUI({
           img(src = NULL,width = 30)
         })
    })}
    else{
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session,"nextq",disabled =TRUE)
      updateButton(session, "restart", disabled = FALSE)
      sendSweetAlert(
          session = session,
          title = "Run out of question",
          type = "error",
          closeOnClickOutside = TRUE,
          h4('Run out of question. Please click Restart to start over')
        )
       output$question<-renderUI({
         return(NULL)
       })
       output$ansA <- renderUI({
         return(NULL)
       })
       output$ansB<- renderUI({
         withMathJax(NULL)
       })
       output$ansC<- renderUI({
         withMathJax(NULL)
       })
       output$ansD<- renderUI({
         withMathJax(NULL)
       })
    }
    })
  
  ###SUBMIT BUTTON###
  observeEvent(input$submit,{ 
      correct_answer <<- bank[id,9]
      if (input$answerA == TRUE & input$answerB==FALSE & input$answerC==FALSE & input$answerD==FALSE){
        selected<<-'A'
      }
      else if (input$answerB == TRUE & input$answerA==FALSE & input$answerC==FALSE & input$answerD==FALSE){
        selected<<-'B'
      }
      else if (input$answerC == TRUE & input$answerB==FALSE & input$answerA==FALSE & input$answerD==FALSE){
        selected<<-'C'
      }
      else if (input$answerD == TRUE & input$answerB==FALSE & input$answerC==FALSE & input$answerA==FALSE){
        selected<<-'D'
      }
      else{
        selected<<-'E'
        sendSweetAlert(
          session = session,
          title = "Error:",
          type = "error",
          closeOnClickOutside = TRUE,
          h4('Please select one choice')
        )
        updateButton(session, "submit", disabled = FALSE)
        updateCheckboxInput(session,'answerA', label=NULL, value=FALSE)
        updateCheckboxInput(session,'answerB', label=NULL, value=FALSE)
        updateCheckboxInput(session,'answerC', label=NULL, value=FALSE)
        updateCheckboxInput(session,'answerD', label=NULL, value=FALSE)
      }
      if (selected == correct_answer){
        print('correct')
        value$correct <<- value$correct + 1
        if (value$correct==8){
            sendSweetAlert(
                  session = session,
                  title = "Success:",
                  type = "success",
                  closeOnClickOutside = TRUE,
                  h4('Congrats! You Win! Please click Restart to start over.')
                )
             updateButton(session, "submit", disabled = TRUE)
             updateButton(session, "nextq", disabled = TRUE)
             updateButton(session, "restart", disabled = FALSE)
           }
      }
      else if(selected == 'E'){
        print("error")
        value$correct=value$correct
        value[["mistake"]] <<- value[["mistake"]]
      }
      else{
        print('wrong')
        value[["mistake"]] <<- value[["mistake"]]+1
        if (value[["mistake"]]==4){
          sendSweetAlert(
            session = session,
            title = "Lost:",
            type = "error",
            closeOnClickOutside = TRUE,
            h4('You lost. Please click Restart to start over')
          )
          updateButton(session, "submit", disabled = TRUE)
          updateButton(session, "nextq", disabled = TRUE)
          updateButton(session, "restart", disabled = FALSE)
        }
      }
    updateButton(session, 'restart', disabled=FALSE)
    output$mark <- renderUI({
      if (selected == correct_answer){
        img(src = "check.png",width = 30)
      }
      else if (selected =='E'){
        img(NULL)
      }
      else{
        img(src = "cross.png",width = 30)
      }
    })})
  
  ####PRINT NUMBER OF CORRECT ANSWERS####
  output$correct <- renderUI({
    
    h3("Number of correct answers:","",value$correct )
  })

  ###PRINT HINTS###
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Hint:",
      type = NULL,
      closeOnClickOutside = TRUE,
      h4(bank[id,3])
    )
  })
  ###SUBMIT BUTTON###
  # observeEvent(input$nextq,{
  #   updateCheckboxInput(session,'answerA', label=NULL, value=FALSE)
  #   updateCheckboxInput(session,'answerB', label=NULL, value=FALSE)
  #   updateCheckboxInput(session,'answerC', label=NULL, value=FALSE)
  #   updateCheckboxInput(session,'answerD', label=NULL, value=FALSE)
  # })
  
  ###Cartoon###
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
  
})