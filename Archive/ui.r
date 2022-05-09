library(shiny)
library(shinyjs)

# Define UI for app that draws a histogram ----
results <- read.csv("data/Result.csv")
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Classroom Observation Analyzer"),
  sidebarLayout(
    sidebarPanel(
      #-------------------------------
      # test on starting page
      textInput("ClassInput", "Class", value = "", width = NULL, placeholder = NULL),
      textInput("InstructorInput", "Instructor", value = "", width = NULL, placeholder = NULL),
      textInput("ModalityInput", "Modality", value = "", width = NULL, placeholder = NULL),
      textInput("DataInput", "Class Date", value = "", width = NULL, placeholder = NULL),
      textInput("StartInput", "Start time", value = "", width = NULL, placeholder = NULL),
      textInput("ArrivalInput", "Arrival time", value = "", width = NULL, placeholder = NULL),
      textInput("TopicInput", "Topic Covered", value = "", width = NULL, placeholder = NULL),
      radioButtons("RBstart","Welcome to the Observation",
                   selected = T,
                   choiceNames = results$SectionName,
                   choiceValues = results$Test),
      actionButton("start", "observation elements"),
      
      # ------------------------------
      
      shinyjs::hidden(actionButton("backButton","Prev")),
      shinyjs::hidden(actionButton("goButton", "Next")),
      shinyjs::hidden(actionButton('submitButton', 'Submit')),
      shinyjs::hidden(actionButton("toMain","Back")),
      # actionButton("scoreButton", "studentScore"),
      downloadButton("downloadSelect","download the table"),
      ),
      mainPanel(shinyjs::hidden(textOutput("question")),
                shinyjs::hidden(uiOutput("answers")),
                shinyjs::hidden(tableOutput("grade")),
                shinyjs::hidden(tableOutput("select")),
                shinyjs::hidden(textOutput("test")),
                shinyjs::hidden(textOutput("test2")),
                shinyjs::hidden(tableOutput("scoretable")),
                shinyjs::hidden(downloadButton("download","download the table")),
                # shinyjs::hidden(downloadButton("downloadSelect","download the table")),
                shinyjs::hidden(textInput("nameInput","name")),
                shinyjs::hidden(textInput("passwordInput", "password")),
                shinyjs::hidden(actionButton("loginButton","login")),
                )
      )
)
