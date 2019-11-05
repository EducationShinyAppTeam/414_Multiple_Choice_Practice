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
  selected <<- c()
  correct_answer <<- c()
  
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Pick a answer and click 'submit' to see if it's right, then click 'next' to continue.",
      type = "info"
    )
  })
  
  ###QUESTION BANK###
  bank <- read.csv('complete.csv', stringsAsFactors = FALSE)
  Qs_array <- c(1:nrow(bank))
  value <- reactiveValues(index =  1,
                          mistake = 0,
                          correct = 0)
  hint <- as.matrix(bank[1:nrow(bank), 3])
  
  # Go button 
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "concepts")
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = FALSE)
  })
  
  # Ready button
  observeEvent(input$ready, {
    updateTabItems(session, "tabs", "test")
  })
  
  # Reset button
  observeEvent(input$restart, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = FALSE)
    updateButton(session, "restart", disabled = TRUE)
    Qs <<- nrow(bank)
    Qs_array <<- c(1:Qs)
    id <- 1
    output$question <- renderUI({
      withMathJax()
      hint <<- withMathJax(bank[id, 3])
      return(bank[id, 2])
    })
   updateRadioGroupButtons(session, "mc1", choices=list(
        bank[id, "A"],
        bank[id, "B"],
        bank[id, "C"],
        bank[id, "D"]
      ),
      selected = NULL,
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square-o")
      ),
      status = 'game')
      output$test1 <- renderUI({withMathJax()})
      output$test2 <- renderUI({withMathJax()})
    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
    value[["mistake"]] <<- 0
    value$correct <<- 0
  })
  
  # Print out a question
  output$question <- renderUI({
    id <<- sample(Qs_array, 1, replace = FALSE, prob = NULL)
    Qs_array <<- Qs_array[!Qs_array %in% id]
updateRadioGroupButtons(session, "mc1", choices=list(
      bank[id, "A"],
      bank[id, "B"],
      bank[id, "C"],
      bank[id, "D"]
    ),
    checkIcon = list(
      yes = icon("check-square"),
      no = icon("square-o")
    ),
    status = 'game')
    output$test1 <- renderUI({withMathJax()})
    output$test2 <- renderUI({withMathJax()})
    hint <<- withMathJax(bank[id, 3])
    return(withMathJax(bank[id, 2]))
  })
  
  ###NEXT QUESTION BUTTON###
  observeEvent(input$nextq, {
    if (length(Qs_array) > 1) {
      id <<- sample(Qs_array, 1, replace = FALSE, prob = NULL)
      Qs_array <<- Qs_array[!Qs_array %in% id]
      hint <<- bank[id, 3]
      withBusyIndicatorServer('nextq', {
        updateButton(session, "submit", disabled = FALSE)
        output$question <- renderUI({
        return(withMathJax(bank[id, 2]))
      })
        updateRadioGroupButtons(session, "mc1", choices=list(
          bank[id, "A"],
          bank[id, "B"],
          bank[id, "C"],
          bank[id, "D"]
        ),
        checkIcon = list(
          yes = icon("check-square"),
          no = icon("square-o")
        ),
        status = 'game')
        output$test1 <- renderUI({withMathJax()})
        output$test2 <- renderUI({withMathJax()})
        output$mark <- renderUI({
          img(src = NULL, width = 30)
        })
      })
    }
    else if (length(Qs_array) == 1) {
      id <<- Qs_array[1]
      Qs_array <<- Qs_array[!Qs_array %in% id]
      hint <<- bank[id, 3]
      withBusyIndicatorServer('nextq', {
      output$question <- renderUI({
        return(withMathJax(bank[id, 2]))
      })
        updateButton(session, "submit", disabled = FALSE)
        updateRadioGroupButtons(session, "mc1", choices=list(
          bank[id, "A"],
          bank[id, "B"],
          bank[id, "C"],
          bank[id, "D"]
        ),
        checkIcon = list(
          yes = icon("check-square"),
          no = icon("square-o")
        ),
        status = 'game')
        output$test1 <- renderUI({withMathJax()})
        output$test2 <- renderUI({withMathJax()})
        output$mark <- renderUI({
          img(src = NULL, width = 30)
        })
      })
    }
    else{
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session, "restart", disabled = FALSE)
      sendSweetAlert(
        session = session,
        title = "Run out of question",
        type = "error",
        closeOnClickOutside = TRUE,
        h4('Run out of question. Please click Restart to start over')
      )
      output$question <- renderUI({
        return(NULL)
      })
      updateRadioGroupButtons(session, "mc1", choices=list(
        bank[id, "A"],
        bank[id, "B"],
        bank[id, "C"],
        bank[id, "D"]
      ),
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square-o")
      ),
      status = 'game')
      output$test1 <- renderUI({withMathJax()})
      output$test2 <- renderUI({withMathJax()})
    }
  })
  
  ###SUBMIT BUTTON###
  observeEvent(input$submit, {
    cAnswer <- bank[id, 10]
       
    if (input$mc1 == cAnswer) {
      print('correct')
      value$correct <- value$correct + 1
      if (value$correct == 10) {
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
      updateButton(session, "submit", disabled = TRUE)
    } else {
      print('wrong')
      value[["mistake"]] <<- value[["mistake"]] + 1
      if (value[["mistake"]] == 4) {
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
      updateButton(session, "submit", disabled = TRUE)
    }
    output$mark <- renderUI({
      if (input$mc1 == cAnswer) {
        img(src = "check.png", width = 30)
      } else {
        img(src = "cross.png", width = 30)
      }
    })
  })
  
  ####PRINT NUMBER OF CORRECT ANSWERS####
  output$correct <- renderUI({
    h3("Number of correct answers:", "", value$correct)
  })
  
  ###PRINT HINTS###
  observeEvent(input$hint, {
    sendSweetAlert(
      session = session,
      title = "Hint:",
      type = NULL,
      closeOnClickOutside = TRUE,
      h4(bank[id, 3])
    )
  })
  
  ###Cartoon###
  output$distPlot <- renderUI({
    img(src = "Cell01.jpg")
    if (value[["mistake"]] == 0) {
      img(src = "Cell01.jpg")
    }
    
    else if (value[["mistake"]] == 1) {
      img(src = "Cell02.jpg")
    }
    
    else if (value[["mistake"]] == 2) {
      img(src = "Cell03.jpg")
    }
    
    else if (value[["mistake"]] == 3) {
      img(src = "Cell04.jpg")
    }
    
    else if (value[["mistake"]] == 4) {
      img(src = "Cell05.jpg")
    }
  })
  
})