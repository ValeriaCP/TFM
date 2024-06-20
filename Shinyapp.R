library(shiny)
library(glmnet)
library(tidymodels)
library(dplyr)
library(tidyr)
library(fastDummies)
library(bslib)
library(jsonlite)
library(ggplot2)
library(plotly)
library(ranger)
library(rlang)
library(kknn)
library(xgboost)
library(naivebayes)
library(kernlab)
library(discrim)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  
  theme = bslib::bs_theme(bootswatch = "morph"),
  sidebarLayout(
    sidebarPanel(
      hidden(div(id = "question1",
                 selectInput(inputId = "Age", label = "How old are you?", choices = as.character(seq(1, 100))),
                 actionButton("next1", "Next")
      )),
      hidden(div(id = "question2",
                 selectInput(inputId = "Sex", label = "How do you identify yourself?", choices = c("Male", "Female", "Others")),
                 actionButton("prev2", "Previous"),
                 actionButton("next2", "Next")
      )),
      hidden(div(id = "question3",
                 selectInput(inputId = "Relationship_status", label = "Relationship status", choices = c("In a relationship", "Single", "Married", "Divorced")),
                 actionButton("prev3", "Previous"),
                 actionButton("next3", "Next")
      )),
      hidden(div(id = "question4",
                 selectInput(inputId = "Occupation", label = "Occupation", choices = c("University Student", "School Student", "Salaried Worker", "Retired")),
                 actionButton("prev4", "Previous"),
                 actionButton("next4", "Next")
      )),
      hidden(div(id = "question5",
                 selectInput(inputId = "Affiliations", label = "What type of organizations are you affiliated with?", 
                             choices = c("University","Private","School", "Company","Goverment","N/A"), multiple = TRUE),
                 actionButton("prev5", "Previous"),
                 actionButton("next5", "Next")
      )),
      hidden(div(id = "question6",
                 selectInput(inputId = "Social_Media_User", label = "Are you a social media user?", choices = c("Yes", "No")),
                 actionButton("prev6", "Previous"),
                 actionButton("next6", "Next")
      )),
      hidden(div(id = "question7",
                 selectInput(inputId = "Platforms_Used", label = "What social media platforms do you commonly use?",
                             choices = c("Facebook","Twitter","Instagram","YouTube","Discord","Reddit","Pinterest","TikTok","Snapchat"), multiple = TRUE),
                 actionButton("prev7", "Previous"),
                 actionButton("next7", "Next")
      )),
      hidden(div(id = "question8",
                 selectInput(inputId = "Time_Spent", label = "What is the average time you spend on social media every day?", 
                             choices = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", "Between 4 and 5 hours", 
                                         "More than 5 hours")),
                 actionButton("prev8", "Previous"),
                 actionButton("next8", "Next")
      )),
      hidden(div(id = "question9",
                 radioButtons(inputId = "ADHD_Q1", label = "How often do you find yourself using social media without a specific purpose?", 
                              choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev9", "Previous"),
                 actionButton("next9", "Next")
      )),
      hidden(div(id = "question10",
                 radioButtons(inputId = "ADHD_Q2", label = "How often do you get distracted by social media when you are busy doing something?", 
                              choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev10", "Previous"),
                 actionButton("next10", "Next")
      )),
      hidden(div(id = "question11",
                 radioButtons(inputId = "Anxiety_Q1", label = "Do you feel restless if you haven't used social media in a while?", 
                              choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev11", "Previous"),
                 actionButton("next11", "Next")
      )),
      hidden(div(id = "question12",
                 radioButtons(inputId = "ADHD_Q3", label = "On a scale of 1 to 5, how easily distracted are you?", choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev12", "Previous"),
                 actionButton("next12", "Next")
      )),
      hidden(div(id = "question13",
                 radioButtons(inputId = "Anxiety_Q2", label = "On a scale of 1 to 5, how much are you bothered by worries?", 
                              choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev13", "Previous"),
                 actionButton("next13", "Next")
      )),
      hidden(div(id = "question14",
                 radioButtons(inputId = "ADHD_Q4", label = "Do you find it difficult to concentrate on things?", choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev14", "Previous"),
                 actionButton("next14", "Next")
      )),
      hidden(div(id = "question15",
                 radioButtons(inputId = "Self_Esteem_Q1", 
                              label = "On a scale of 1 to 5, how often do you compare yourself to other successful people through the use of social media?", 
                              choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev15", "Previous"),
                 actionButton("next15", "Next")
      )),
      hidden(div(id = "question16",
                 radioButtons(inputId = "Self_Esteem_Q2", label = "Following the previous question, how do you feel about these comparisons, generally speaking?", 
                              choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev16", "Previous"),
                 actionButton("next16", "Next")
      )),
      hidden(div(id = "question17",
                 radioButtons(inputId = "Self_Esteem_Q3", label = "How often do you look to seek validation from features of social media?", 
                              choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev17", "Previous"),
                 actionButton("next17", "Next")
      )),
      hidden(div(id = "question18",
                 radioButtons(inputId = "Depression_Q1", label = "How often do you feel depressed or down?", choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev18", "Previous"),
                 actionButton("next18", "Next")
      )),
      hidden(div(id = "question19",
                 radioButtons(inputId = "Depression_Q2", label = "On a scale of 1 to 5, how frequently does your interest in daily activities fluctuate?", 
                              choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev19", "Previous"),
                 actionButton("next19", "Next")
      )),
      hidden(div(id = "question20",
                 radioButtons(inputId = "Depression_Q3", label = "On a scale of 1 to 5, how often do you face issues regarding sleep?", choices = c("1", "2", "3", "4", "5")),
                 actionButton("prev20", "Previous"),
                 actionButton("submit", "Submit")
      ))
    ),
    mainPanel(
      verbatimTextOutput("prediction"),
      uiOutput("resultBox")
    )
  )
)

model <- readRDS("final_ensemble.rds")

server <- function(input, output, session) {
  shinyjs::show("question1")
  
  observeEvent(input$next1, {
    shinyjs::hide("question1")
    shinyjs::show("question2")
  })
  observeEvent(input$prev2, {
    shinyjs::hide("question2")
    shinyjs::show("question1")
  })
  observeEvent(input$next2, {
    shinyjs::hide("question2")
    shinyjs::show("question3")
  })
  observeEvent(input$prev3, {
    shinyjs::hide("question3")
    shinyjs::show("question2")
  })
  observeEvent(input$next3, {
    shinyjs::hide("question3")
    shinyjs::show("question4")
  })
  observeEvent(input$prev4, {
    shinyjs::hide("question4")
    shinyjs::show("question3")
  })
  observeEvent(input$next4, {
    shinyjs::hide("question4")
    shinyjs::show("question5")
  })
  observeEvent(input$prev5, {
    shinyjs::hide("question5")
    shinyjs::show("question4")
  })
  observeEvent(input$next5, {
    shinyjs::hide("question5")
    shinyjs::show("question6")
  })
  observeEvent(input$prev6, {
    shinyjs::hide("question6")
    shinyjs::show("question5")
  })
  observeEvent(input$next6, {
    shinyjs::hide("question6")
    shinyjs::show("question7")
  })
  observeEvent(input$prev7, {
    shinyjs::hide("question7")
    shinyjs::show("question6")
  })
  observeEvent(input$next7, {
    shinyjs::hide("question7")
    shinyjs::show("question8")
  })
  observeEvent(input$prev8, {
    shinyjs::hide("question8")
    shinyjs::show("question7")
  })
  observeEvent(input$next8, {
    shinyjs::hide("question8")
    shinyjs::show("question9")
  })
  observeEvent(input$prev9, {
    shinyjs::hide("question9")
    shinyjs::show("question8")
  })
  observeEvent(input$next9, {
    shinyjs::hide("question9")
    shinyjs::show("question10")
  })
  observeEvent(input$prev10, {
    shinyjs::hide("question10")
    shinyjs::show("question9")
  })
  observeEvent(input$next10, {
    shinyjs::hide("question10")
    shinyjs::show("question11")
  })
  observeEvent(input$prev11, {
    shinyjs::hide("question11")
    shinyjs::show("question10")
  })
  observeEvent(input$next11, {
    shinyjs::hide("question11")
    shinyjs::show("question12")
  })
  observeEvent(input$prev12, {
    shinyjs::hide("question12")
    shinyjs::show("question11")
  })
  observeEvent(input$next12, {
    shinyjs::hide("question12")
    shinyjs::show("question13")
  })
  observeEvent(input$prev13, {
    shinyjs::hide("question13")
    shinyjs::show("question12")
  })
  observeEvent(input$next13, {
    shinyjs::hide("question13")
    shinyjs::show("question14")
  })
  observeEvent(input$prev14, {
    shinyjs::hide("question14")
    shinyjs::show("question13")
  })
  observeEvent(input$next14, {
    shinyjs::hide("question14")
    shinyjs::show("question15")
  })
  observeEvent(input$prev15, {
    shinyjs::hide("question15")
    shinyjs::show("question14")
  })
  observeEvent(input$next15, {
    shinyjs::hide("question15")
    shinyjs::show("question16")
  })
  observeEvent(input$prev16, {
    shinyjs::hide("question16")
    shinyjs::show("question15")
  })
  observeEvent(input$next16, {
    shinyjs::hide("question16")
    shinyjs::show("question17")
  })
  observeEvent(input$prev17, {
    shinyjs::hide("question17")
    shinyjs::show("question16")
  })
  observeEvent(input$next17, {
    shinyjs::hide("question17")
    shinyjs::show("question18")
  })
  observeEvent(input$prev18, {
    shinyjs::hide("question18")
    shinyjs::show("question17")
  })
  observeEvent(input$next18, {
    shinyjs::hide("question18")
    shinyjs::show("question19")
  })
  observeEvent(input$prev19, {
    shinyjs::hide("question19")
    shinyjs::show("question18")
  })
  observeEvent(input$next19, {
    shinyjs::hide("question19")
    shinyjs::show("question20")
  })
  observeEvent(input$prev20, {
    shinyjs::hide("question20")
    shinyjs::show("question19")
  })
  
  prediction <- eventReactive(input$submit, { 
    all_options <- data.frame(
      Age = NA_integer_,
      Sex = factor(NA, levels = c("Male", "Female", "Others")),
      `Relationship Status` = factor(NA, levels = c("In a relationship", "Single", "Married", "Divorced")),
      Occupation = factor(NA, levels = c("University Student", "School Student", "Salaried Worker", "Retired")),
      `Social Media User?` = factor(NA, levels = c("Yes", "No")),
      `Time Spent` = factor(NA, levels = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", 
                                           "Between 4 and 5 hours", "More than 5 hours")),
      `ADHD Q1` = NA_integer_,
      `ADHD Q2` = NA_integer_,
      `Anxiety Q1` = NA_integer_,
      `ADHD Q3` = NA_integer_,
      `Anxiety Q2` = NA_integer_,
      `ADHD Q4` = NA_integer_,
      `Self Esteem Q1` = NA_integer_,
      `Self Esteem Q2` = NA_integer_,
      `Self Esteem Q3` = NA_integer_,
      `Depression Q1` = NA_integer_,
      `Depression Q2` = NA_integer_,
      `Depression Q3` = NA_integer_,
      `Platforms Used` = NA_character_,
      `Affiliations` = NA_character_,
      check.names = FALSE
    )
    
    user_data <- data.frame(
      Age = as.integer(input$Age),
      Sex = factor(input$Sex, levels = c("Male", "Female", "Others")),
      `Relationship Status` = factor(input$Relationship_status, levels = c("In a relationship", "Single", "Married", "Divorced")),
      Occupation = factor(input$Occupation, levels = c("University Student", "School Student", "Salaried Worker", "Retired")),
      `Social Media User?` = factor(input$Social_Media_User, levels = c("Yes", "No")),
      `Time Spent` = factor(input$Time_Spent, levels = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours",
                                                         "Between 4 and 5 hours", "More than 5 hours")),
      `ADHD Q1` = as.numeric(input$ADHD_Q1),
      `ADHD Q2` = as.numeric(input$ADHD_Q2),
      `Anxiety Q1` = as.numeric(input$Anxiety_Q1),
      `ADHD Q3` = as.numeric(input$ADHD_Q3),
      `Anxiety Q2` = as.numeric(input$Anxiety_Q2),
      `ADHD Q4` = as.numeric(input$ADHD_Q4),
      `Self Esteem Q1` = as.numeric(input$Self_Esteem_Q1),
      `Self Esteem Q2` = as.numeric(input$Self_Esteem_Q2),
      `Self Esteem Q3` = as.numeric(input$Self_Esteem_Q3),
      `Depression Q1` = as.numeric(input$Depression_Q1),
      `Depression Q2` = as.numeric(input$Depression_Q2),
      `Depression Q3` = as.numeric(input$Depression_Q3),
      `Platforms Used` = ifelse(length(input$Platforms_Used) > 0, paste(input$Platforms_Used, collapse = ", "), NA_character_),
      `Affiliations` = ifelse(length(input$Affiliations) > 0, paste(input$Affiliations, collapse = ", "), NA_character_),
      check.names = FALSE
    )
    
    combined_data <- all_options
    for (col in colnames(user_data)) {
      combined_data[[col]] <- user_data[[col]]
    }
    
    combined_data <- combined_data %>%
      mutate(Age = as.integer(Age),
             `Self Esteem Q2` = as.numeric(`Self Esteem Q2`),
             `Self Esteem Q2` = case_when(
               `Self Esteem Q2`== 1 ~ 5,
               `Self Esteem Q2` == 2 ~ 4,
               `Self Esteem Q2` == 3 ~ 3,
               `Self Esteem Q2` == 4 ~ 2,
               `Self Esteem Q2` == 5 ~ 1,
               TRUE ~ `Self Esteem Q2`
             ),
             ADHD_Score = rowSums(select(., starts_with("ADHD Q")), na.rm = TRUE),
             Anxiety_Score = rowSums(select(., starts_with("Anxiety Q")), na.rm = TRUE),
             Self_Esteem_Score = rowSums(select(., starts_with("Self Esteem Q")), na.rm = TRUE),
             Depression_Score = rowSums(select(., starts_with("Depression Q")), na.rm = TRUE),
             Total_Score = ADHD_Score + Anxiety_Score + Self_Esteem_Score + Depression_Score,
             Outcome = ifelse(Total_Score < 40, "0", "1")) %>%
      select(-starts_with("ADHD Q"), -starts_with("Anxiety Q"), -starts_with("Self Esteem Q"), -starts_with("Depression Q"))
    
    combined_data <- combined_data %>% 
      mutate(`Time Spent` = as.factor(`Time Spent`)) %>%
      dummy_cols("Time Spent", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Time Spent_", "", .x)), starts_with("Time Spent_"))
    
    combined_data <- combined_data %>% 
      fastDummies::dummy_cols("Sex", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("Sex_", "", .x), starts_with("Sex_"))
    
    combined_data <- combined_data %>%
      separate_rows(`Platforms Used`, sep = ",\\s*") %>%
      mutate(`Platforms Used` = as.factor(`Platforms Used`)) %>%
      dummy_cols("Platforms Used", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Platforms Used_", "", .x)), starts_with("Platforms Used_"))
    
    combined_data <- combined_data %>%
      mutate(`Relationship Status` = as.factor(`Relationship Status`)) %>%
      dummy_cols("Relationship Status", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Relationship Status_", "", .x)), starts_with("Relationship Status_"))
    
    combined_data <- combined_data %>%
      mutate(`Occupation` = as.factor(`Occupation`)) %>%
      dummy_cols("Occupation", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Occupation_", "", .x)), starts_with("Occupation_"))
    
    combined_data <- combined_data %>%
      separate_rows(`Affiliations`, sep = ",\\s*") %>%
      mutate(`Affiliations` = as.factor(`Affiliations`)) %>%
      dummy_cols("Affiliations", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Affiliations_", "", .x)), starts_with("Affiliations_"))
    
    required_columns <- c('Discord', 'Facebook','Instagram', 'Reddit', 'Snapchat', 'TikTok', 'Twitter', 'YouTube', 
                          'Company', 'Goverment', 'Private', 'School', 'University', 'Pinterest', 'N/A')
    for (col in required_columns) {
      if (!(col %in% colnames(combined_data))) {
        combined_data[[col]] <- 0
      }
    }
    
    combined_data <- combined_data %>%
      mutate(across(c(
        `Female`,`Male`,`Others`, 
        `Between 1 and 2 hours`, `Between 2 and 3 hours`, `Between 3 and 4 hours`, `Between 4 and 5 hours`, 
        `Less than an Hour`, `More than 5 hours`, 
        Discord, Facebook, Instagram, Pinterest, Reddit, Snapchat, TikTok, Twitter, YouTube, 
        Divorced, `In a relationship`, Married, Single, 
        Retired, `Salaried Worker`, `School Student`, `University Student`, 
        Company, Goverment, `N/A`, Private, School, University, 
        Outcome
      ), as.factor))
    
    tree_model <- readRDS("tree_model.rds")
    knn_model <- readRDS("knn_model.rds")
    log_model <- readRDS("log_model.rds")
    boost_model <- readRDS("boost_model.rds")
    rf_model <- readRDS("rf_model.rds")
    svm_model <- readRDS("svm_model.rds")
    nb_model <- readRDS("nb_model.rds")
    
    combined_data$.pred_tree <- as.numeric(unlist(predict(tree_model, combined_data)))
    combined_data$.pred_knn <- as.numeric(unlist(predict(knn_model, combined_data)))
    combined_data$.pred_log <- as.numeric(unlist(predict(log_model, combined_data)))
    combined_data$.pred_boost <- as.numeric(unlist(predict(boost_model, combined_data)))
    combined_data$.pred_rf <- as.numeric(unlist(predict(rf_model, combined_data)))
    combined_data$.pred_svm <- as.numeric(unlist(predict(svm_model, combined_data)))
    combined_data$.pred_nb <- as.numeric(unlist(predict(nb_model, combined_data)))
    
    prediction_columns <- c('.pred_tree', '.pred_svm', '.pred_rf', '.pred_boost', '.pred_log', '.pred_knn', '.pred_nb')
    for (col in prediction_columns) {
      if (!(col %in% colnames(combined_data))) {
        combined_data[[col]] <- 0
      }
    }
    
    prediction <- predict(model, combined_data)
    
    
    prediction <- as.character(prediction[1])
    
    return(list(prediction = prediction, combined_data = combined_data))
  })
  
  output$resultBox <- renderUI({
    req(prediction())
    result <- prediction()
    score <- result$combined_data$Total_Score[1] 
    print(paste("Total Score:", score))
    div(class = "result-box",
        h1(paste("Your score is", score, " / ", 60)),
        p(ifelse(result$prediction == "1", "Based on your responses, you might be experiencing some mental health issues. We recommend consulting a professional.",
                 "Based on your responses, you do not seem to be experiencing significant mental health issues."))
    )
  })
}
shinyApp(ui = ui, server = server)