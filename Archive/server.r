library(shinyjs)

#drfortino
NAME = "drfortino"
PASSWORD = "drfortino"

answers <- read.csv("data/Answer.csv")
questions <- read.csv("data/Question.csv")
results <- read.csv("data/Result.csv")
testQA <- cbind(questions, answers[,-c(1)])
# define some reusable functions

#----------------func 0-----------------
# a query to select between test
get_test<- function(num){
  testQA[testQA$Test == num, ]
}
#---------------------------------------

# ---------------func 1-----------------
# get questions for selected test
get_questions <- function(test_df,num){
  paste(test_df$Question[num],".",test_df$Problem[num])
}

get_questions_num <- function(test_df,num){
  test_df$Question[num]
}
# --------------------------------------

# ---------------func 2-----------------
# get answers for selected test
get_answers<- function(test_df, num){
  rowVal = as.list(test_df[num,])
  out = c()
  for (i in 5:length(rowVal)) {
    if(rowVal[i] != ""){
      out <- c(out,as.character(rowVal[i]))
    }
  }
  out
}
# --------------------------------------


# ---------------func 3-----------------
# to random select answers and record the right one
shuffle_answers <- function(test_df){
  correct_answer = c()
  for (i in 1:nrow(test_df)) {
    r = test_df[i, -c(1:4)]
    answers = r[r != ""]
    num_valid_answer = length(answers)
    s = sample(num_valid_answer)
    correct_answer <- c(correct_answer, which(s == 1))
    
    test_df[i, c(5:(4 + num_valid_answer))] = answers[s]
  } 
  foo <- vector("list", length = 2)
  names(foo) <- c("test_df", "correctAnswer")
  foo$test_df = test_df
  foo$correctAnswer = correct_answer
  foo
}
# --------------------------------------

# ---------------func 4-----------------
# get correct answer
get_correct_answer <- function(correctAnswer, num){
  correctAnswer[testQA$Test == num]
}
# --------------------------------------


# ---------------initiating-----------------
foo = shuffle_answers(testQA)
testQA = foo$test_df 
correctAnswer = foo$correctAnswer
# ------------------------------------------

server <- function(input, output, session) {
  
  # initiatives
  counters <- reactiveValues()
  counters$page <- 0
  counters$n <- 0
  counters$selection <- vector(length = 0)
  counters$selection2 <- vector('list', length = nrow(testQA))
  counters$selection3 <- rep(list(list()), nrow(results))
  # names(counters$selection3) <- results$SectionName
  counters$test <- testQA
  counters$correctAnswer <- c()
  counters$grade <- 0
  
  #-----------------------------------------------------------------------------
  # switch between test, javascript include
  observeEvent(input$start,{
    if(!is.null(input$RBstart)){
      counters$test <- get_test(input$RBstart)
      counters$n <- nrow(counters$test)
      counters$selection <- vector(length = counters$n)
      shinyjs::hide("RBstart")
      shinyjs::hide("start")
      shinyjs::hide("scoreButton")
      shinyjs::hide("downloadSelect")
      
      shinyjs::hide("ClassInput")
      shinyjs::hide("InstructorInput")
      shinyjs::hide("ModalityInput")
      shinyjs::hide("DataInput")
      shinyjs::hide("StartInput")
      shinyjs::hide("ArrivalInput")
      shinyjs::hide("TopicInput")
      
      shinyjs::show("goButton")
      shinyjs::show("backButton")
      shinyjs::show("question")
      shinyjs::show("answers") 
      # shinyjs::show("submitButton")
      shinyjs::show("toMain")
    }
  })

  
  observeEvent(input$submitButton,{
    shinyjs::hide("goButton")
    shinyjs::hide("backButton")
    shinyjs::hide("question")
    shinyjs::hide("submitButton")
    shinyjs::hide("answers")
    shinyjs::show("select")
    shinyjs::show("downloadSelect")
    # changed, show "back" button
    shinyjs::show("toMain")
    
  })
  
  viewscore <- observeEvent(input$scoreButton,{
    shinyjs::hide("RBstart")
    shinyjs::hide("start")
    shinyjs::show("toMain")
    shinyjs::show("nameInput")
    shinyjs::show("passwordInput")
    shinyjs::show("loginButton")
  })
  
  backscore<- observeEvent(input$toMain,{
    shinyjs::show("RBstart")
    shinyjs::show("start")
    shinyjs::show("downloadSelect")
    
    shinyjs::show("ClassInput")
    shinyjs::show("InstructorInput")
    shinyjs::show("ModalityInput")
    shinyjs::show("DataInput")
    shinyjs::show("StartInput")
    shinyjs::show("ArrivalInput")
    shinyjs::show("TopicInput")
    
    shinyjs::show("scoreButton")
    shinyjs::hide("toMain")
    shinyjs::hide("nameInput")
    shinyjs::hide("passwordInput")
    shinyjs::hide("loginButton")
    shinyjs::hide("scoretable")
    shinyjs::hide("download")
    shinyjs::hide("bp")
    # shinyjs::hide("grade")
    shinyjs::hide("histogram")
    shinyjs::hide("select")
    # shinyjs::hide("downloadSelect")
    shinyjs::hide("question")
    shinyjs::hide("answers") 
    shinyjs::hide("submitButton")
    shinyjs::hide("goButton")
    shinyjs::hide("backButton")

  })
  #-----------------------------------------------------------------------
  
  page_forward <- observeEvent(input$goButton,{
    # ---------- test------------
    if(!is.null(input$answerButton)){
      n <- get_questions_num(counters$test,counters$page%%counters$n + 1)
      counters$selection2[[n]] <- input$answerButton
      cat(str(counters$selection2))
    }
    # ---------------------------
    counters$page <- counters$page + 1
  })
  
  page_backward <- observeEvent(input$backButton,{
    if(!is.null(input$answerButton)){
      n <- get_questions_num(counters$test,counters$page%%counters$n + 1)
      counters$selection2[[n]] <- input$answerButton
      cat(str(counters$selection2))
    }
    counters$page <- counters$page - 1
  })
  
  output$question <- renderText({
    get_questions(counters$test,counters$page%%counters$n + 1)
  })

  output$answers <- renderUI({
    checkboxGroupInput("answerButton", "select from following", 
                       choices = get_answers(counters$test,counters$page%%counters$n + 1),
                       selected = counters$selection[counters$page%%counters$n+1],
                 )
  })
  
  render_selection <- function() {
    res = ""
    for (i in 1:length(counters$selection2)) {
      r = counters$test[i, ]
      p = r[3]
      s = counters$selection2[[i]]
      res = paste(res, "Problem:", p, tags$br(), "Selected:", paste(s, collapse="; "), tags$br(), tags$br())
    }
    res
  }
  
  output$select <- renderText({
    render_selection()
  })
  
  output$downloadSelect <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      ql = list()
      sl = list()
      for (i in 1:length(counters$selection2)) {
        r = counters$test[i, ]
        p = r[3]
        s = counters$selection2[[i]]
        ql[[i]] = p
        sl[[i]] = s
      }
      
      inp <- list(
        input$ClassInput,
        input$InstructorInput,
        input$ModalityInput,
        input$DataInput,
        input$StartInput,
        input$ArrivalInput,
        input$TopicInput
      )
      
      params <- list(ql = testQA,
                     sl = counters$selection2,
                     inp = inp
                     )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}