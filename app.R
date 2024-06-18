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


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "morph"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Age", label = "How old are you?", choices = as.character(seq(1, 100))),
      selectInput(inputId = "Sex", label = "How do you identify yourself?", choices = c("Male", "Female", "Others")),
      selectInput(inputId = "Relationship_status", label = "Relationship status", choices = c("In a relationship", "Single", "Married", "Divorced")),
      selectInput(inputId = "Occupation", label = "Occupation", choices = c("University Student", "School Student", "Salaried Worker", "Retired")),
      selectInput(inputId = "Affiliations", label = "What type of organizations are you affiliated with?", 
                  choices = c("University","Private","School", "Company","Goverment","N/A"), multiple = TRUE),
      selectInput(inputId = "Social_Media_User", label = "Are you a social media user?", choices = c("Yes", "No")),
      selectInput(inputId = "Platforms_Used", label = "What social media platforms do you commonly use?",
                  choices = c("Facebook","Twitter","Instagram","YouTube","Discord","Reddit","Pinterest","TikTok","Snapchat"), multiple = TRUE),
      selectInput(inputId = "Time_Spent", label = "What is the average time you spend on social media every day?", 
                  choices = c("Less than an Hour", "Between 1 and 2 hours", "Between 2 and 3 hours", "Between 3 and 4 hours", "Between 4 and 5 hours", 
                              "More than 5 hours")),
      radioButtons(inputId = "ADHD_Q1", label = "How often do you find yourself using social media without a specific purpose?", 
                   choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "ADHD_Q2", label = "How often do you get distracted by social media when you are busy doing something?", 
                   choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "Anxiety_Q1", label = "Do you feel restless if you haven't used social media in a while?", 
                   choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "ADHD_Q3", label = "On a scale of 1 to 5, how easily distracted are you?", choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "Anxiety_Q2", label = "On a scale of 1 to 5, how much are you bothered by worries?", 
                   choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "ADHD_Q4", label = "Do you find it difficult to concentrate on things?", choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "Self_Esteem_Q1", 
                   label = "On a scale of 1 to 5, how often do you compare yourself to other successful people through the use of social media?", 
                   choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "Self_Esteem_Q2", label = "Following the previous question, how do you feel about these comparisons, generally speaking?", 
                   choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "Self_Esteem_Q3", label = "How often do you look to seek validation from features of social media?", 
                   choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "Depression_Q1", label = "How often do you feel depressed or down?", choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "Depression_Q2", label = "On a scale of 1 to 5, how frequently does your interest in daily activities fluctuate?", 
                   choices = c("1", "2", "3", "4", "5")),
      radioButtons(inputId = "Depression_Q3", label = "On a scale of 1 to 5, how often do you face issues regarding sleep?", choices = c("1", "2", "3", "4", "5")),
      actionButton(inputId = "submit", label = "Submit")
    ),
    mainPanel(
      verbatimTextOutput("prediction"),
      plotlyOutput("plot", height = "600px", width = "800px"),
      plotlyOutput("pieChart",height = "600px", width = "800px")
      
    
    )
  )
)

model <- readRDS("final_ensemble.rds")

server <- function(input, output) {
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
    
    print("Columns after combining with user data:")
    print(colnames(combined_data))
    
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
    
    print("After initial transformations:")
    print(colnames(combined_data))  
    
    combined_data <- combined_data %>% 
      mutate(`Time Spent` = as.factor(`Time Spent`)) %>%
      dummy_cols("Time Spent", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Time Spent_", "", .x)), starts_with("Time Spent_"))
    
    combined_data <- combined_data %>% 
      fastDummies::dummy_cols("Sex", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("Sex_", "", .x), starts_with("Sex_"))
    
    print("Columns after Sex transformation:")
    print(colnames(combined_data))
    
    combined_data <- combined_data %>%
      separate_rows(`Platforms Used`, sep = ",\\s*") %>%
      mutate(`Platforms Used` = as.factor(`Platforms Used`)) %>%
      dummy_cols("Platforms Used", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Platforms Used_", "", .x)), starts_with("Platforms Used_"))
    
    print("Columns after Platforms Used transformation:")
    print(colnames(combined_data))
    
    combined_data <- combined_data %>%
      mutate(`Relationship Status` = as.factor(`Relationship Status`)) %>%
      dummy_cols("Relationship Status", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Relationship Status_", "", .x)), starts_with("Relationship Status_"))
    
    print("Columns after Relationship Status transformation:")
    print(colnames(combined_data))
    
    combined_data <- combined_data %>%
      mutate(`Occupation` = as.factor(`Occupation`)) %>%
      dummy_cols("Occupation", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Occupation_", "", .x)), starts_with("Occupation_"))
    
    print("Columns after Occupation transformation:")
    print(colnames(combined_data))
    
    combined_data <- combined_data %>%
      separate_rows(`Affiliations`, sep = ",\\s*") %>%
      mutate(`Affiliations` = as.factor(`Affiliations`)) %>%
      dummy_cols("Affiliations", remove_selected_columns = TRUE) %>%
      rename_with(~ gsub("_", " ", gsub("Affiliations_", "", .x)), starts_with("Affiliations_"))
    
    print("After Affiliations transformation:")
    print(colnames(combined_data)) 
    
    print("Transformed data:")
    print(combined_data)
    
    required_columns <- c('Discord', 'Facebook','Instagram', 'Reddit', 'Snapchat', 'TikTok', 'Twitter', 'YouTube', 
                          'Company', 'Goverment', 'Private', 'School', 'University', 'Pinterest', 'N/A')
    for (col in required_columns) {
      if (!(col %in% colnames(combined_data))) {
        combined_data[[col]] <- "0"
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
    
    return(list(prediction = prediction, combined_data = combined_data))
  })
  
  output$prediction <- renderText({
    req(prediction())
    paste("The predicted outcome is:", prediction()$prediction)
  })
  

  output$plot <- renderPlotly({
    req(prediction())
    
    prediction_result <- as.character(prediction()$prediction)
    if (is.list(prediction_result)) {
      prediction_result <- unlist(prediction_result)
    }
    
    prediction_labels <- ifelse(prediction_result == "1", "YOU'RE HEALTHY", "PLEASE CONSULT A PROFESSIONAL")
    
    data <- data.frame(Prediction = prediction_result, Label = prediction_labels)
    
    colors <- ifelse(data$Prediction == "1", "green", "red")
    
    plot_ly(data, labels = ~Label, type = 'pie', 
            textinfo = 'label', 
            insidetextorientation = 'radial', 
            marker = list(colors = colors)) %>%
      layout(title = 'Final results',
             showlegend = FALSE,
             margin = list(l = 50, r = 50, b = 50, t = 50),
             font = list(size = 14))
  })


output$pieChart <- renderPlotly({
  req(prediction())  
  
  combined_data <- prediction()$combined_data
  
  max_scores <- c(ADHD_Score = 20, Anxiety_Score = 10, Self_Esteem_Score = 15, Depression_Score = 15)
  
  percentages <- combined_data %>%
    mutate(ADHD_Percentage = (ADHD_Score / max_scores["ADHD_Score"]) * 100,
           Depression_Percentage = (Depression_Score / max_scores["Depression_Score"]) * 100,
           Anxiety_Percentage = (Anxiety_Score / max_scores["Anxiety_Score"]) * 100,
           Self_Esteem_Percentage = (Self_Esteem_Score / max_scores["Self_Esteem_Score"]) * 100)%>%
    pivot_longer(cols = c(ADHD_Percentage, Depression_Percentage, Anxiety_Percentage,Self_Esteem_Percentage),
                 names_to = "Symptom",
                 values_to = "Percentage")
  
  colors <- c("ADHD_Percentage" = "#ce3c3c", "Anxiety_Percentage" = "#3c335a", 
              "Depression_Percentage" = "#638d96", "Self_Esteem_Percentage" = "#a9f4c5")
  
  custom_labels <- c("ADHD_Percentage" = "ADHD","Depression_Percentage" = "Depression",
                     "Anxiety_Percentage" = "Anxiety", "Self_Esteem_Percentage" = "Self Esteem")
  
  plot_ly(percentages, labels = ~custom_labels[Symptom], values = ~Percentage, type = 'pie', 
          textinfo = 'custom_labels', 
          insidetextfont = list(color = '#FFFFFF', orientation = 'horizontal'),
          text = ~paste(custom_labels[Symptom]),  
          insidetextorientation = 'horizontal',  
          hoverinfo = 'label+percent',
          marker = list(colors = colors[percentages$Symptom])) %>%
    layout(title = 'Your mental health symptoms results',
           showlegend = FALSE,
           margin = list(l = 50, r = 50, b = 50, t = 50),
           font = list(size = 14))
  
})
}

shinyApp(ui = ui, server = server)
