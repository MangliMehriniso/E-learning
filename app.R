library(nnet)
library(dplyr)
library(class)
library(rpart)	
library(caret)
library(rattle)					
library(rpart.plot)			
library(RColorBrewer)				
library(party)					
library(partykit)				
library(tree)
library(shiny)
library(ggplot2)
library(readr)
library(C50)
library(xtable) 
library(e1071) 
library(caTools)
library(readr)
library(randomForest)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title = "E-learning Process Performance Analysis",
    titleWidth = 450
    ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Preprocessing Results", tabName = "preprocessing", icon = icon("columns")),
      menuItem("Demographic Data Analysis", tabName = "demo", icon = icon("bezier-curve"),
               menuSubItem("Naive Bayes", tabName = "naive"),
               menuSubItem("Random Forest", tabName = "rf"),
               menuSubItem("KNN", tabName = "knn")
               ),
      menuItem("Numeric Data Analysis", tabName = "utilize", icon = icon("chart-bar"),
               menuSubItem("Naive Bayes", tabName = "naive_u"),
               menuSubItem("Random Forest", tabName = "rf_u"),
               menuSubItem("KNN", tabName = "knn_u"),
               menuSubItem("Neural Network", tabName = "nn_u")
               ),
      menuItem("Numeric And Demographic Data Analysis", tabName = "and", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "preprocessing",
              fluidRow(
                
                box(
                  selectInput(inputId = "dataset",
                              label = "Choose a dataset:",
                              choices = c("enrolments", "comments", "step_activity","following","preprocessed")),
                  
                  width = 4,
                  numericInput(inputId = "obs",
                               label = "Number of observations to view:",
                               value = 10)),
                
                box(
                  title = "Dataset",
                  width =8,
                  div(style = 'overflow-x: scroll', tableOutput('view'))
                )
              )
      ),

      tabItem(tabName = "naive",
              fluidRow(
                box(
                  title = "Model Selection Panel",
                  radioButtons("radio_n1", label = h3("Select dataset"),
                               choices = list("Preprocessed Data" = 1,
                                              "Language Groups" = 2,
                                              "English as Official Language" = 3,
                                              "English as Official but not Primary Language"=4,
                                              "English as Second Language" = 5), 
                               selected = 1),
                  width = 4,
                  numericInput("ratio", "Train And Test division %", value=50/100, min = 50/100, max = 90/100, step=0.1),
                  actionButton('data_n1',label = 'Process',icon("random",lib="glyphicon"))
                ),
                box(
                  tabsetPanel( 
                    tabPanel("Model Result", tableOutput("result")), 
                    tabPanel("Model Summary", verbatimTextOutput('summary'))
                  ),
                  width = 8
                )
           )  
      ),
      tabItem(tabName = "rf",
              fluidRow(
                box(
                  title = "Model Selection Panel",
                  radioButtons("radio_rf1", label = h3("Select dataset"),
                               choices = list("Preprocessed Data" = 1,
                                              "Language Groups" = 2,
                                              "English as Official Language" = 3,
                                              "English as Official but not Primary Language"=4,
                                              "English as Second Language" = 5), 
                               selected = 1),
                  width = 4,
                  numericInput("ratio_rf", "Train And Test division %", value=50/100, min = 50/100, max = 90/100, step=0.1),
                  actionButton('data_rf1',label = 'Process',icon("random",lib="glyphicon"))
                ),
                box(
                  tabsetPanel( 
                    tabPanel("model result", tableOutput("result_rf")), 
                    tabPanel("model summary", verbatimTextOutput('summary_rf')),
                    tabPanel("model plot", plotOutput('plot_rf'))
                  ),
                  width = 8
                )
              )
      ),
      tabItem(tabName = "knn",
              fluidRow(
                box(
                  title = "Model Selection Panel",
                  radioButtons("radio_knn1", label = h3("Select dataset"),
                               choices = list("Preprocessed Data" = 1,
                                              "Language Groups" = 2,
                                              "English as Official Language" = 3,
                                              "English as Official but not Primary Language"=4,
                                              "English as Second Language" = 5), 
                               selected = 1),
                  width = 4,
                  numericInput("ratio_knn", "Train And Test division %", value=50/100, min = 50/100, max = 90/100, step=0.1),
                  actionButton('data_knn1',label = 'Process',icon("random",lib="glyphicon"))
                ),
                box(
                  tabsetPanel( 
                    tabPanel("model result", tableOutput("result_knn")), 
                    tabPanel("model summary", verbatimTextOutput('summary_knn'))
                  ),
                  width = 8
                )
              )
      ),
      tabItem(tabName = "naive_u",
              fluidRow(
                box(
                  title = "Model Selection Panel",
                  radioButtons("radio_n2", label = h3("Select dataset"),
                               choices = list("Preprocessed Data" = 1,
                                              "Language Groups" = 2,
                                              "English as Official Language" = 3,
                                              "English as Official but not Primary Language"=4,
                                              "English as Second Language" = 5), 
                               selected = 1),
                  width = 4,
                  numericInput("ratio_k1", "Train And Test division %", value=50/100, min = 50/100, max = 90/100, step=0.1),
                  actionButton('data_n2',label = 'Process',icon("random",lib="glyphicon"))
                ),
                box(
                  tabsetPanel( 
                    tabPanel("model result", tableOutput("result_k1")), 
                    tabPanel("model summary", verbatimTextOutput('summary_k1'))
                  ),
                  width = 8
                )
              )
      ),
      tabItem(tabName = "rf_u",
              fluidRow(
                box(
                  title = "Model Selection Panel",
                  radioButtons("radio_rf2", label = h3("Select dataset"),
                               choices = list("Preprocessed Data" = 1,
                                              "Language Groups" = 2,
                                              "English as Official Language" = 3,
                                              "English as Official but not Primary Language"=4,
                                              "English as Second Language" = 5), 
                               selected = 1),
                  width = 4,
                  numericInput("ratio_rfk1", "Train And Test division %", value=50/100, min = 50/100, max = 90/100, step=0.1),
                  actionButton('data_rf2',label = 'Process',icon("random",lib="glyphicon"))
                ),
                box(
                  tabsetPanel( 
                    tabPanel("model result", tableOutput("result_rfk1")), 
                    tabPanel("model summary", verbatimTextOutput('summary_rfk1')),
                    tabPanel("model plot", plotOutput('plot_rfk1'))
                  ),
                  width = 8
                )
              )
      ),
      tabItem(tabName = "knn_u",
              fluidRow(
                box(
                  title = "Model Selection Panel",
                  radioButtons("radio_knn2", label = h3("Select dataset"),
                               choices = list("Preprocessed Data" = 1,
                                              "Language Groups" = 2,
                                              "English as Official Language" = 3,
                                              "English as Official but not Primary Language"=4,
                                              "English as Second Language" = 5), 
                               selected = 1),
                  width = 4,
                  numericInput("ratio_knnk1", "Train And Test division %", value=50/100, min = 50/100, max = 90/100, step=0.1),
                  sliderInput('k', 'Select the Number of Nearest Neighbours', value = 10, min = 1, max = 50),
                  actionButton('data_knn2',label = 'Process',icon("random",lib="glyphicon"))
                ),
                box(
                  tabsetPanel( 
                    tabPanel("model result", tableOutput("result_knnk1")), 
                    tabPanel("model summary", verbatimTextOutput('summary_knnk1'))
                  ),
                  width = 8
                )
              )
      ),
      tabItem(tabName = "nn_u",
              fluidRow(
                box(
                  title = "Model Selection Panel",
                  radioButtons("radio_nn2", label = h3("Select dataset"),
                               choices = list("Preprocessed Data" = 1,
                                              "Language Groups" = 2,
                                              "English as Official Language" = 3,
                                              "English as Official but not Primary Language"=4,
                                              "English as Second Language" = 5), 
                               selected = 1),
                  width = 4,
                  numericInput("ratio_nnk1", "Train And Test division %", value=50/100, min = 50/100, max = 90/100, step=0.1),
                  actionButton('data_nn2',label = 'Process',icon("random",lib="glyphicon"))
                ),
                box(
                  tabsetPanel( 
                    tabPanel("model result", tableOutput("result_nnk1")), 
                    tabPanel("model summary", verbatimTextOutput('summary_nnk1'))
                  ),
                  width = 8
                )
              )
      ),
      tabItem(tabName = "and",
              fluidRow(
                box(
                  radioButtons("radio_and", label = h3("Select algorithm"),
                               choices = list("Naive Bayes" = 1,
                                              "Random Forest" = 2,
                                              "KNN" = 3,
                                              "Neural Network"=4), 
                               selected = 1),
                  width = 4,
                  sliderInput('k', 'Select the Number of Nearest Neighbours', value = 10, min = 1, max = 50),
                  actionButton('data_and',label = 'Process',icon("random",lib="glyphicon"))
                ),
                box(
                  tabsetPanel( 
                    tabPanel("model result", tableOutput("result_kd")), 
                    tabPanel("model summary", verbatimTextOutput('summary_kd'))
                    
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  #followings <- read.csv("C:/Users/MEHRINISO/Desktop/AraProje/DataSet/followings.csv")
  #comments <- read.csv("C:/Users/MEHRINISO/Desktop/AraProje/DataSet/understanding-language-4_comments.csv")
  #step_activity <- read.csv("C:/Users/MEHRINISO/Desktop/AraProje/DataSet/understanding-language-4_step-activity.csv")
  #enrolments <- read.csv("C:/Users/MEHRINISO/Desktop/AraProje/DataSet/understanding-language-4_enrolments.csv")
  preprocessed<-read.csv("C:/Users/MEHRINISO/Desktop/E-LearningPerformance/preprocessed_data.csv")
  lgroups <- preprocessed[preprocessed$country_type== "not_official" | preprocessed$country_type== "official_not_primary" | preprocessed$country_type== "official_primary",]
  lgroups_ENOL<-lgroups[lgroups$country_type=="not_official",]
  lgroups_EOPL<-lgroups[lgroups$country_type=="official_primary",]
  lgroups_EOL<-lgroups[lgroups$country_type=="official_not_primary",]
  datasetInput <- reactive({
    switch(input$dataset, 
           "enrolments" = enrolments,
           "comments" = comments,
           "step_activity" = step_activity,
           "following" = followings,
           "preprocessed" = preprocessed)
  })

  output$view <- renderTable({
    head(datasetInput(),input$obs)
  })
  
  set.seed(1234)
  
  #Naive bayes
  observe({
  r<-as.numeric(input$ratio)
  
  #preprocessed
  split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(r,1-r))
  training_set = subset(preprocessed, split == 1)
  test_set = subset(preprocessed, split == 2)
  
  #language groups
  split = sample(2, nrow(lgroups), replace = TRUE, prob=c(r,1-r))
  training_set_dil = subset(lgroups, split == 1)
  test_set_dil = subset(lgroups, split == 2)
  
  #ESL 
  split = sample(2, nrow(lgroups_ENOL), replace = TRUE, prob=c(r,1-r))
  training_set_ENOL = subset(lgroups_ENOL, split == 1)
  test_set_ENOL = subset(lgroups_ENOL, split == 2)
  
  
  #EFL
  split = sample(2, nrow(lgroups_EOPL), replace = TRUE, prob=c(r,1-r))
  training_set_EFL = subset(lgroups_EOPL, split == 1)
  test_set_EFL = subset(lgroups_EOPL, split == 2)
  
  #EFL Not Primary
  split = sample(2, nrow(lgroups_EOL), replace = TRUE, prob=c(r,1-r))
  training_set_EOL = subset(lgroups_EOL, split == 1)
  test_set_EOL = subset(lgroups_EOL, split == 2)

  
  if (input$radio_n1==1){
    observeEvent(input$data_n1, {
      full_data_naiveBayes<-naiveBayes(training_set$performance ~.,method='class',data=training_set[,3:8])
      model_pred<-predict(full_data_naiveBayes, test_set[,3:8], type="class")
      
      output$result<-renderTable({
        table(model_pred, test_set$performance)})
      
      output$summary<-renderPrint({
        confusionMatrix(table(model_pred,test_set$performance))
      })
    })
  }
  else if (input$radio_n1==2){
    observeEvent(input$data_n1, {
      dil_datasi_bayes<-naiveBayes( performance ~ . , data = training_set_dil[,3:8])
      pred1<-predict(dil_datasi_bayes,newdata=test_set_dil[,3:8])
      output$result<-renderTable({
        table(pred1,test_set_dil$performance)})
      output$summary<-renderPrint({
        confusionMatrix(table(pred1,test_set_dil$performance))
      })
    })
  }
  else if (input$radio_n1==3){
    observeEvent(input$data_n1, {
      ENOL_bayes<-naiveBayes( performance ~ . , data = training_set_ENOL[,3:8])
      pred_ENOL<-predict(ENOL_bayes,newdata=test_set_ENOL[,3:8])
      output$result<-renderTable({
        table(pred_ENOL,test_set_ENOL$performance)})
      output$summary<-renderPrint({
        confusionMatrix(table(pred_ENOL,test_set_ENOL$performance))
      })
    })
  }
  else if (input$radio_n1==4){
    observeEvent(input$data_n1, {
      EFL_bayes<-naiveBayes( performance ~ . , data = training_set_EFL[,3:8])
      pred_EFL<-predict(EFL_bayes,newdata=test_set_EFL[,3:8])
      output$result<-renderTable({
        table(pred_EFL,test_set_EFL$performance)})
      output$summary<-renderPrint({
        confusionMatrix(table(pred_EFL,test_set_EFL$performance))
      })
    })
  }
  else if (input$radio_n1==5){
    observeEvent(input$data_n1, {
      EOL_bayes<-naiveBayes( performance ~ . , data = training_set_EOL[,3:8])
      pred_EOL<-predict(EOL_bayes,newdata=test_set_EOL[,3:8])
      output$result<-renderTable({
        table(pred_EOL,test_set_EOL$Performance)})
      output$summary<-renderPrint({
        confusionMatrix(table(pred_EOL,test_set_EOL$performance))
      })
    })
  }
  })
  
  #Random Forest
  observe({
    r<-as.numeric(input$ratio_rf)
    
    
    split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(r,1-r))
    training_set = subset(preprocessed, split == 1)
    test_set = subset(preprocessed, split == 2)
    
    
    split = sample(2, nrow(lgroups), replace = TRUE, prob=c(r,1-r))
    training_set_dil = subset(lgroups, split == 1)
    test_set_dil = subset(lgroups, split == 2)
    
    
    split = sample(2, nrow(lgroups_ENOL), replace = TRUE, prob=c(r,1-r))
    training_set_ENOL = subset(lgroups_ENOL, split == 1)
    test_set_ENOL = subset(lgroups_ENOL, split == 2)
    
    split = sample(2, nrow(lgroups_EOPL), replace = TRUE, prob=c(r,1-r))
    training_set_EFL = subset(lgroups_EOPL, split == 1)
    test_set_EFL = subset(lgroups_EOPL, split == 2)
    
    split = sample(2, nrow(lgroups_EOL), replace = TRUE, prob=c(r,1-r))
    training_set_EOL = subset(lgroups_EOL, split == 1)
    test_set_EOL = subset(lgroups_EOL, split == 2)
    
    
    if (input$radio_rf1==1){
      observeEvent(input$data_rf1, {
        require(randomForest)
        rf.fit<- randomForest(training_set$performance ~ ., data=training_set[,3:8],method='class', importance=TRUE, ntree=400)
        rf.pred<- predict(rf.fit, test_set[3:8])
        
        output$result_rf<-renderTable({
          table(rf.pred, test_set$performance)})
        
        output$summary_rf <- renderPrint({
          confusionMatrix(table(rf.pred,test_set$performance))
        })
        
        output$plot_rf <- renderPlot({
          varImpPlot(rf.fit, main="Random Forest model fit, importance of the parameters")
          importance(rf.fit)
        })
      })
    }
    else if (input$radio_rf1==2){
      observeEvent(input$data_rf1, {
        require(randomForest)
        rf.dil<- randomForest(training_set_dil$performance ~ ., data=training_set_dil[,3:8],method='class', importance=TRUE, ntree=400)
        rf.pred.dil<- predict(rf.dil, test_set_dil[3:8])
        
        output$result_rf<-renderTable({
          table(rf.pred.dil, test_set_dil$performance)})
        
        output$summary_rf <- renderPrint({
          confusionMatrix(table(rf.pred.dil,test_set_dil$performance))
        })
        
        output$plot_rf <- renderPlot({
          varImpPlot(rf.dil, main="Random Forest model fit, importance of the parameters")
          importance(rf.dil)
        })
      })
    }
    else if (input$radio_rf1==3){
      observeEvent(input$data_rf1, {
        require(randomForest)
        rf.EFL<- randomForest(training_set_EFL$performance ~ ., data=training_set_EFL[,3:8],method='class', importance=TRUE, ntree=400)
        rf.pred.EFL<- predict(rf.EFL, test_set_EFL[3:8])
        
        output$result_rf<-renderTable({
          table(rf.pred.EFL, test_set_EFL$performance)})
        
        output$summary_rf <- renderPrint({
          confusionMatrix(table(rf.pred.EFL,test_set_EFL$performance))
        })
        
        output$plot_rf <- renderPlot({
          varImpPlot(rf.EFL, main="Random Forest model fit, importance of the parameters")
          importance(rf.EFL)
          
        })
      })
    }
    else if (input$radio_rf1==4){
      observeEvent(input$data_rf1, {
        require(randomForest)
        rf.ESL<- randomForest(training_set_ENOL$performance ~ ., data=training_set_ENOL[,3:8],method='class', importance=TRUE, ntree=400)
        rf.pred.ESL<- predict(rf.ESL, test_set_ENOL[3:8])
        
        output$result_rf<-renderTable({
          table(rf.pred.ESL, test_set_ENOL$performance)})
        
        output$summary_rf <- renderPrint({
          confusionMatrix(table(rf.pred.ESL,test_set_ENOL$performance))
        })
        
        output$plot_rf <- renderPlot({
          varImpPlot(rf.ESL, main="Random Forest model fit, importance of the parameters")
          importance(rf.ESL)
        })
      })
    }
    else if (input$radio_rf1==5){
      observeEvent(input$data_rf1, {
        require(randomForest)
        rf.EFL.Not<- randomForest(training_set_EOL$performance ~ ., data=training_set_EOL[,2:8], importance=TRUE, ntree=400)
        rf.pred.EOL<- predict(rf.EFL.Not, test_set_EOL[2:7])
        
        output$result_rf<-renderTable({
          table(rf.pred.EOL, test_set_EOL$performance)})
        
        output$summary_rf <- renderPrint({
          confusionMatrix(table(rf.pred.EOL,test_set_EOL$performance))
        })
        
        output$plot_rf <- renderPlot({
          varImpPlot(rf.EFL.Not, main="Random Forest model fit, importance of the parameters")
          importance(rf.EFL.Not)
        })
      })
    }
  })
  
  #KNN
  observe({
    r<-as.numeric(input$ratio_knn)
    
    split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(r,1-r))
    training_set = subset(preprocessed, split == 1)
    test_set = subset(preprocessed, split == 2)
    
    
    split = sample(2, nrow(lgroups), replace = TRUE, prob=c(r,1-r))
    training_set_dil = subset(lgroups, split == 1)
    test_set_dil = subset(lgroups, split == 2)
    
    #ESL 
    split = sample(2, nrow(lgroups_ENOL), replace = TRUE, prob=c(r,1-r))
    training_set_ENOL = subset(lgroups_ENOL, split == 1)
    test_set_ENOL = subset(lgroups_ENOL, split == 2)
    
    #EFL icin
    split = sample(2, nrow(lgroups_EOPL), replace = TRUE, prob=c(r,1-r))
    training_set_EFL = subset(lgroups_EOPL, split == 1)
    test_set_EFL = subset(lgroups_EOPL, split == 2)
    
    #EFL Not Primary
    split = sample(2, nrow(lgroups_EOL), replace = TRUE, prob=c(r,1-r))
    training_set_EOL = subset(lgroups_EOL, split == 1)
    test_set_EOL = subset(lgroups_EOL, split == 2)
    
    if (input$radio_knn1==1){
      observeEvent(input$data_knn1, {
        train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
        model <- train(preprocessed$performance~., data=preprocessed[,3:8], trControl=train_control, method="knn")
        x<-predict(model)
        
        output$result_knn<-renderTable({
          table(x, preprocessed$performance)})
        
        output$summary_knn<-renderPrint({
          confusionMatrix(x,preprocessed$performance)
        })
      })
    }
    else if (input$radio_knn1==2){
      observeEvent(input$data_knn1, {
        train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
        model <- train(preprocessed$performance~., data=lgroups[,3:8], trControl=train_control, method="knn")
        x<-predict(model)
        
        output$result_knn<-renderTable({
          table(x, lgroups$performance)})
        
        output$summary_knn<-renderPrint({
          confusionMatrix(x,lgroups$performance)
        })
      })
    }
    else if (input$radio_knn1==3){
      observeEvent(input$data_knn1, {
        train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
        model <- train(preprocessed$performance~., data=lgroups_EOPL[,3:8], trControl=train_control, method="knn")
        x<-predict(model)
        
        output$result_knn<-renderTable({
          table(x, lgroups_EOPL$performance)})
        
        output$summary_knn<-renderPrint({
          confusionMatrix(x,lgroups_EOPL$performance)
        })
      })
    }
    else if (input$radio_knn1==4){
      observeEvent(input$data_knn1, {
        train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
        model <- train(preprocessed$performance~., data=lgroups_ENOL[,3:8], trControl=train_control, method="knn")
        x<-predict(model)
        
        output$result_knn<-renderTable({
          table(x, lgroups_ENOL$performance)})
        
        output$summary_knn<-renderPrint({
          confusionMatrix(x,lgroups_ENOL$performance)
        })
        
      })
    }
    else if (input$radio_knn1==5){
      observeEvent(input$data_knn1, {
        train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
        model <- train(preprocessed$performance~., data=lgroups_EOL[,3:8], trControl=train_control, method="knn")
        x<-predict(model)
        
        output$result_knn<-renderTable({
          table(x, lgroups_EOL$performance)})
        
        output$summary_knn<-renderPrint({
          confusionMatrix(x,lgroups_EOL$performance)
        })
      
      })
    }
  })
  
  #Naive Bayes
  observe({
    r<-as.numeric(input$ratio_k1)
    
    split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(r,1-r))
    training_set = subset(preprocessed, split == 1)
    test_set = subset(preprocessed, split == 2)
    
    split = sample(2, nrow(lgroups), replace = TRUE, prob=c(r,1-r))
    training_set_dil = subset(lgroups, split == 1)
    test_set_dil = subset(lgroups, split == 2)
    
    #ESL
    split = sample(2, nrow(lgroups_ENOL), replace = TRUE, prob=c(r,1-r))
    training_set_ENOL = subset(lgroups_ENOL, split == 1)
    test_set_ENOL = subset(lgroups_ENOL, split == 2)
    
    #EFL
    split = sample(2, nrow(lgroups_EOPL), replace = TRUE, prob=c(r,1-r))
    training_set_EFL = subset(lgroups_EOPL, split == 1)
    test_set_EFL = subset(lgroups_EOPL, split == 2)
    
    #EFL Not Primary
    split = sample(2, nrow(lgroups_EOL), replace = TRUE, prob=c(r,1-r))
    training_set_EOL = subset(lgroups_EOL, split == 1)
    test_set_EOL = subset(lgroups_EOL, split == 2)
    
    if (input$radio_n2==1){
      observeEvent(input$data_n2, {
        full_data_naiveBayes<-naiveBayes(training_set$performance ~.,method='class',data=training_set[9:28])
        model_pred_k1<-predict(full_data_naiveBayes, test_set[9:28], type="class")
        
        output$result_k1<-renderTable({
          table(model_pred_k1, test_set$performance)})
        
        output$summary_k1<-renderPrint({
          confusionMatrix(table(model_pred_k1,test_set$performance))
        })
      })
    }
    else if (input$radio_n2==2){
      observeEvent(input$data_n2, {
        dil_datasi_bayes<-naiveBayes(training_set_dil$performance ~ . , data = training_set_dil[,9:28])
        pred_dil<-predict(dil_datasi_bayes,newdata=test_set_dil[9:28])
        
        output$result_k1<-renderTable({
          table(pred_dil,test_set_dil$performance)})
        
        output$summary_k1<-renderPrint({
          confusionMatrix(table(pred_dil,test_set_dil$performance))
        })
        
      })
    }
    else if (input$radio_n2==3){
      observeEvent(input$data_n2, {
        ENOL_bayes<-naiveBayes( training_set_ENOL$performance ~ . , method='class', data = training_set_ENOL[,9:28])
        pred<-predict(ENOL_bayes,newdata=test_set_ENOL[9:28])
        output$result_k1<-renderTable({
          table(pred,test_set_ENOL$performance)})
        output$summary_k1<-renderPrint({
          confusionMatrix(table(pred,test_set_ENOL$performance))
        })
      })
    }
    else if (input$radio_n2==4){
      observeEvent(input$data_n2, {
        EFL_bayes<-naiveBayes( training_set_EFL$performance ~ . , method='class', data = training_set_EFL[,9:28])
        pred_EFL<-predict(EFL_bayes,newdata=test_set_EFL[9:28])
        
        output$result_k1<-renderTable({
          table(pred_EFL,test_set_EFL$performance)})
        
        output$summary_k1<-renderPrint({
          confusionMatrix(table(pred_EFL,test_set_EFL$performance))
        })
      })
    }
    else if (input$radio_n2==5){
      observeEvent(input$data_n2, {
        EOL_bayes<-naiveBayes( training_set_EOL$performance ~ . , method='class',  data = training_set_EOL[,9:28])
        pred_EOL<-predict(EOL_bayes,newdata=test_set_EOL[9:28])
        
        output$result_k1<-renderTable({
          table(pred_EOL,test_set_EOL$performance)})
        
        output$summary_k1<-renderPrint({
          confusionMatrix(table(pred_EOL,test_set_EOL$performance))
        })
      })
    }
  })
  
  
  #Random Forest
  observe({
    
    r<-as.numeric(input$ratio_rfk1)
    
    split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(r,1-r))
    training_set = subset(preprocessed, split == 1)
    test_set = subset(preprocessed, split == 2)
    
    
    split = sample(2, nrow(lgroups), replace = TRUE, prob=c(r,1-r))
    training_set_dil = subset(lgroups, split == 1)
    test_set_dil = subset(lgroups, split == 2)
    
    #ESL 
    split = sample(2, nrow(lgroups_ENOL), replace = TRUE, prob=c(r,1-r))
    training_set_ENOL = subset(lgroups_ENOL, split == 1)
    test_set_ENOL = subset(lgroups_ENOL, split == 2)
    
    #EFL
    split = sample(2, nrow(lgroups_EOPL), replace = TRUE, prob=c(r,1-r))
    training_set_EFL = subset(lgroups_EOPL, split == 1)
    test_set_EFL = subset(lgroups_EOPL, split == 2)
    
    #EFL Not Primary
    split = sample(2, nrow(lgroups_EOL), replace = TRUE, prob=c(r,1-r))
    training_set_EOL = subset(lgroups_EOL, split == 1)
    test_set_EOL = subset(lgroups_EOL, split == 2)
    
    if (input$radio_rf2==1){
      observeEvent(input$data_rf2, {
        require(randomForest)
        rf.fit1<- randomForest(training_set$performance ~ ., data=training_set[,9:28],method='class', importance=TRUE, ntree=400)
        rf.pred1<- predict(rf.fit1, test_set[9:28])
        
        output$result_rfk1<-renderTable({
          table(rf.pred1, test_set$performance)})
        
        output$summary_rfk1 <- renderPrint({
          confusionMatrix(table(rf.pred1,test_set$performance))
        })
        
        output$plot_rfk1 <- renderPlot({
          varImpPlot(rf.fit1, main="Random Forest model fit, importance of the parameters")
          importance(rf.fit1)
        })
        
      })
    }
    else if (input$radio_rf2==2){
      observeEvent(input$data_rf2,{
        require(randomForest)
        rf.dil<- randomForest(training_set_dil$performance ~ ., data=training_set_dil[,9:28],method='class', importance=TRUE, ntree=400)
        rf.pred.dil<- predict(rf.dil, test_set_dil[9:28])
        
        output$result_rfk1<-renderTable({
          table(rf.pred.dil, test_set_dil$performance)})
        
        output$summary_rfk1 <- renderPrint({
          confusionMatrix(table(rf.pred.dil,test_set_dil$performance))
        })
        
        output$plot_rfk1 <- renderPlot({
          varImpPlot(rf.dil, main="Random Forest model fit, importance of the parameters")
          importance(rf.dil)
        })
      })
    }
    else if (input$radio_rf2==3){
    observeEvent(input$data_rf2,{
      require(randomForest)
      rf.EFL<- randomForest(training_set_EFL$performance ~ ., data=training_set_EFL[,9:28],method='class', importance=TRUE, ntree=400)
      rf.pred.EFL<- predict(rf.EFL, test_set_EFL[9:28])
      
      output$result_rfk1<-renderTable({
        table(rf.pred.EFL, test_set_EFL$performance)})
      
      output$summary_rfk1 <- renderPrint({
        confusionMatrix(table(rf.pred.EFL,test_set_EFL$performance))
      })
      
      output$plot_rfk1 <- renderPlot({
        varImpPlot(rf.EFL, main="Random Forest model fit, importance of the parameters")
        importance(rf.EFL)
      })
    })
    }
    else if (input$radio_rf2==4){
    observeEvent(input$ESL_k1rf,{
      require(randomForest)
      rf.ESL<- randomForest(training_set_ENOL$performance ~ ., data=training_set_ENOL[,9:28],method='class', importance=TRUE, ntree=400)
      rf.pred.ESL<- predict(rf.ESL, test_set_ENOL[9:28])
      
      output$result_rfk1<-renderTable({
        table(rf.pred.ESL, test_set_ENOL$performance)})
      
      output$summary_rfk1 <- renderPrint({
        confusionMatrix(table(rf.pred.ESL,test_set_ENOL$performance))
      })
      
      output$plot_rfk1 <- renderPlot({
        varImpPlot(rf.ESL, main="Random Forest model fit, importance of the parameters")
        importance(rf.ESL)
      })
    })
    }
    else if (input$radio_rf2==5){
    observeEvent(input$data_rf2,{
      require(randomForest)
      rf.EFL.Not<- randomForest(training_set_EOL$performance ~ ., data=training_set_EOL[,9:28], importance=TRUE, ntree=400)
      rf.pred.EOL<- predict(rf.EFL.Not, test_set_EOL[9:28])
      
      output$result_rfk1<-renderTable({
        table(rf.pred.EOL, test_set_EOL$performance)})
      
      output$summary_rfk1 <- renderPrint({
        confusionMatrix(table(rf.pred.EOL,test_set_EOL$performance))
      })
      
      output$plot_rfk1 <- renderPlot({
        varImpPlot(rf.EFL.Not, main="Random Forest model fit, importance of the parameters")
        importance(rf.EFL.Not)
      })
    })
    }
  })
  
  
  #Neural Network
  observe({
    r<-as.numeric(input$ratio_nnk1)
    
    #language groups
    split = sample(2, nrow(lgroups), replace = TRUE, prob=c(r,1-r))
    training_set_dil = subset(lgroups, split == 1)
    test_set_dil = subset(lgroups, split == 2)
    
    #ESL
    split = sample(2, nrow(lgroups_ENOL), replace = TRUE, prob=c(r,1-r))
    training_set_ENOL = subset(lgroups_ENOL, split == 1)
    test_set_ENOL = subset(lgroups_ENOL, split == 2)
    
    #EFL
    split = sample(2, nrow(lgroups_EOPL), replace = TRUE, prob=c(r,1-r))
    training_set_EFL = subset(lgroups_EOPL, split == 1)
    test_set_EFL = subset(lgroups_EOPL, split == 2)
    
    #EFL Not Primary
    split = sample(2, nrow(lgroups_EOL), replace = TRUE, prob=c(r,1-r))
    training_set_EOL = subset(lgroups_EOL, split == 1)
    test_set_EOL = subset(lgroups_EOL, split == 2)
    
    
    if (input$radio_nn2==1){
      observeEvent(input$data_nn2, {
        require(nnet)
        nn_model4<-nnet(training_set_dil$performance ~ ., data = training_set_dil[,9:28],size=10)
        prednn4<-predict(nn_model4,test_set_dil[,9:28],type = "class")
        
        output$result_nnk1<-renderTable({
          table(prednn4, test_set_dil$performance)})
        
        output$summary_nnk1 <- renderPrint({
          confusionMatrix(table(data = prednn4, reference = test_set_dil$performance))
        })
      })
    }
    else if (input$radio_nn2==2){
      #EFL nnet
      observeEvent(input$data_nn2, {
        require(neuralnet)
        
        nn_model1<-nnet(training_set_EFL$performance ~ ., data = training_set_EFL[,9:28],size=10)
        prednn2<-predict(nn_model1,test_set_EFL[,9:28],type = "class")
        
        output$result_nnk1<-renderTable({
          table(prednn2, test_set_EFL$performance)  })
        
        output$summary_nnk1 <- renderPrint({
          confusionMatrix(data = prednn2, reference = test_set_EFL$performance)
        })
        
      })
    }
    else if (input$radio_nn2==3){
      observeEvent(input$data_nn2, {
        require(neuralnet)
        
        nn_model<-nnet(training_set_ENOL$performance ~ ., data = training_set_ENOL[,9:28],size=10)
        prednn1<-predict(nn_model,test_set_ENOL[,9:28],type = "class")
        
        output$result_nnk1<-renderTable({
          table(prednn1, test_set_ENOL$performance)  })
        
        output$summary_nnk1 <- renderPrint({
          confusionMatrix(data = prednn1, reference = test_set_ENOL$performance)
        })
      })
    }
    else if (input$radio_nn2==4){  
      #EFL Not Primary
      observeEvent(input$data_nn2, {
        require(neuralnet)
        
        nn_model3<-nnet(training_set_EOL$performance ~ ., data = training_set_EOL[,9:28],size=10)
        prednn3<-predict(nn_model3,test_set_EOL[,9:28],type = "class")
        
        output$result_nnk1<-renderTable({
          table(prednn3, test_set_EOL$performance)})
        
        output$summary_nnk1 <- renderPrint({
          confusionMatrix(data = prednn3, reference = test_set_EOL$performance)
        })
      })
    }
    
    
  })
  
  
  #KNN
  observe({
    r<-as.numeric(input$ratio_knnk1)
    
    split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(r,1-r))
    training_set = subset(preprocessed, split == 1)
    test_set = subset(preprocessed, split == 2)
    
    
    split = sample(2, nrow(lgroups), replace = TRUE, prob=c(r,1-r))
    training_set_dil = subset(lgroups, split == 1)
    test_set_dil = subset(lgroups, split == 2)
    
    #ESL icin 
    split = sample(2, nrow(lgroups_ENOL), replace = TRUE, prob=c(r,1-r))
    training_set_ENOL = subset(lgroups_ENOL, split == 1)
    test_set_ENOL = subset(lgroups_ENOL, split == 2)
    
    #EFL icin
    split = sample(2, nrow(lgroups_EOPL), replace = TRUE, prob=c(r,1-r))
    training_set_EFL = subset(lgroups_EOPL, split == 1)
    test_set_EFL = subset(lgroups_EOPL, split == 2)
    
    #EFL Not Primary
    split = sample(2, nrow(lgroups_EOL), replace = TRUE, prob=c(r,1-r))
    training_set_EOL = subset(lgroups_EOL, split == 1)
    test_set_EOL = subset(lgroups_EOL, split == 2)
    
    if (input$radio_knn2==1){
      observeEvent(input$data_knn2, {
        cl2=training_set_EFL[,8]
        Pred_efl<-knn(train=training_set_EFL[,9:28], test=test_set_EFL[,9:28], cl2, k=input$k)
        output$result_knnk1<-renderTable({
          table(Pred_efl, test_set_EFL$performance)  })
        output$summary_knnk1 <- renderPrint({
          confusionMatrix(data = Pred_efl, reference = test_set_EFL$performance)
        })
        
      })
    }
    else if (input$radio_knn2==2){
      observeEvent(input$data_knn2, {
        cl2=training_set_ENOL[,8]
        Pred_esl<-knn(train=training_set_ENOL[,9:28], test=test_set_ENOL[,9:28], cl2, k=input$k)
        output$result_knnk1<-renderTable({
          table(Pred_esl, test_set_ENOL$performance)  })
        output$summary_knnk1 <- renderPrint({
          confusionMatrix(data = Pred_esl, reference = test_set_ENOL$performance)
        })
        
      })
    }
    else if (input$radio_knn2==3){
      observeEvent(input$data_knn2, {
        cl2=training_set_EOL[,8]
        Pred_efl_not<-knn(train=training_set_EOL[,9:28], test=test_set_EOL[,9:28], cl2, k=input$k)
        output$result_knnk1<-renderTable({
          table(Pred_efl_not, test_set_EOL$performance)  })
        output$summary_knnk1 <- renderPrint({
          confusionMatrix(data = Pred_efl_not, reference = test_set_EOL$performance)
        })
        
      })
    }
    else if (input$radio_knn2==4){
      observeEvent(input$data_knn2, {
        cl2=training_set[,8]
        Pred_full_data<-knn(train=training_set[,9:28], test=test_set[,9:28], cl2, k=input$k,prob = F, use.all = T)
        output$result_knnk1<-renderTable({
          table(Pred_full_data, test_set$performance)  })
        output$summary_knnk1 <- renderPrint({
          confusionMatrix(data = Pred_full_data, reference = test_set$performance)
        })
        
      })
    }
    else if (input$radio_knn2==5){
      observeEvent(input$data_knn2, {
        cl2=training_set_dil[,8]
        Pred_dil_data<-knn(train=training_set_dil[,9:28], test=test_set_dil[,9:28], cl2, k=input$k)
        output$result_knnk1<-renderTable({
          table(Pred_dil_data, test_set_dil$performance)  })
        output$summary_knnk1 <- renderPrint({
          confusionMatrix(data = Pred_dil_data, reference = test_set_dil$performance)
        })
        
      })
    }
  })
  
  
  #Utilization And Demographic
  observe({
    if(input$radio_and==1){
      observeEvent(input$data_and, {
        split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(0.7,0.3))
        training_set = subset(preprocessed, split == 1)
        test_set = subset(preprocessed, split == 2)
        dil_datasi_bayes<-naiveBayes(training_set$performance ~ . , data = training_set)
        pred_dil<-predict(dil_datasi_bayes,newdata=test_set)
        
        output$result_kd<-renderTable({
          table(pred_dil,test_set$performance)})
        
        output$summary_kd<-renderPrint({
          confusionMatrix(table(pred_dil,test_set$performance))
        })
      })
    }
    else if(input$radio_and==2){

      observeEvent(input$data_and, {
        split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(0.7,0.3))
        training_set = subset(preprocessed, split == 1)
        test_set = subset(preprocessed, split == 2)
        require(randomForest)
        rf.fit<- randomForest(training_set$performance ~ ., data=training_set,method='class', importance=TRUE, ntree=400)
        rf.pred<- predict(rf.fit, test_set)
        
        output$result_kd<-renderTable({
          table(rf.pred, test_set$performance)})
        
        output$summary_kd <- renderPrint({
          confusionMatrix(table(rf.pred,test_set$performance))
        })
      })
    }
    else if(input$radio_and==3){
      observeEvent(input$data_and, {
        train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
        model <- train(preprocessed$performance~., data=preprocessed, trControl=train_control, method="knn")
        x<-predict(model)
        
        output$result_kd<-renderTable({
          table(x, preprocessed$performance) })
        
        output$summary_kd <- renderPrint({
          confusionMatrix(x,preprocessed$performance)
        })
      })
    }
    else if(input$radio_and==4){
      observeEvent(input$data_and, {
        split = sample(2, nrow(preprocessed), replace = TRUE, prob=c(0.7,0.3))
        training_set = subset(preprocessed, split == 1)
        test_set = subset(preprocessed, split == 2)
        require(nnet)
        nn_model4<-nnet(training_set$performance ~ ., data = training_set,size=10)
        prednn4<-predict(nn_model4,test_set,type = "class")
        
        output$result_kd<-renderTable({
          table(prednn4, test_set$performance)})
        
        output$summary_kd <- renderPrint({
          confusionMatrix(table(data = prednn4, reference = test_set$performance))
        })
      })
    }
    
  })
  
  
  }

shinyApp(ui,server)

