library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(DT)
library(ggrepel)
library(factoextra)
library(caret)


#Load data

#epl 19-20
epl20 <- read_csv("https://github.com/vaastav/Fantasy-Premier-League/raw/master/data/2019-20/players_raw.csv")
epl20 <- epl20 %>% mutate(year = 2020)

#replace 'None' with NA
epl20[epl20=="None"] <- NA

#change ep_this column to numeric
epl20$ep_this <- as.numeric(epl20$ep_this)

#epl 18-19
epl19 <- read_csv("https://github.com/vaastav/Fantasy-Premier-League/raw/master/data/2018-19/players_raw.csv")
epl19 <- epl19 %>% mutate(year = 2019)

#epl 17-18
epl18 <- read_csv("https://github.com/vaastav/Fantasy-Premier-League/raw/master/data/2017-18/players_raw.csv")
epl18 <- epl18 %>% mutate(year = 2018)

#epl 16-17
epl17 <- read_csv("https://github.com/vaastav/Fantasy-Premier-League/raw/master/data/2016-17/players_raw.csv")
epl17 <- epl17 %>% mutate(year = 2017)

epl <- bind_rows(epl17, epl18, epl19, epl20)

#epl teams
#eplTeams <- read_csv("https://github.com/vaastav/Fantasy-Premier-League/raw/master/data/2019-20/teams.csv")

#code positions
epl <- epl %>% mutate(position = case_when(element_type == 1 ~ "GK", element_type == 2 ~ "DEF", element_type == 3 ~ "MID", element_type == 4 ~ "FWD"))

#create full name variable
epl <- epl %>% mutate(full_name = paste0(first_name," ",second_name))

#select and rearrange variables
epl <- epl %>% select(first_name, second_name, full_name, position, now_cost, total_points, points_per_game, minutes, status, goals_scored, assists, bonus, bps, goals_conceded, own_goals, penalties_missed, penalties_saved, yellow_cards, red_cards, saves, clean_sheets, creativity, ict_index, influence, threat, selected_by_percent, year)


#shinyServer arguments
shinyServer(function(input, output, session) {
  
  #stop app when closing
  session$onSessionEnded(function() {
    stopApp()
  })
  
  #Explore tab data
  getData <- reactive({
    newData <- epl %>% filter(position == input$positionSelect & year <= max(input$years) & year >=min(input$years))
  }) #end explore tab data
  
  #Explore tab user inputs
  observe({
    
    newData <- getData()
    numVars <- newData %>% select_if(is.numeric)
    numVars <- select_if(numVars, colSums(numVars)>0) %>% select(-year) %>% colnames()
    varList <- as.list(numVars)
    
    updateSelectInput(session, "playerSelect",
                      choices = getData()$full_name %>% sort())
    
    updateSelectInput(session, "compareSelect",
                      choices = getData()$full_name %>% sort())
    
    updateSelectInput(session, "varSelect",
                      choices = varList, selected = "total_points")
    
    updateSelectInput(session, "varXSelect",
                      choices = varList, selected = "now_cost")
    
    #brushed points output
    output$brush_info <- renderPrint({
      #get filtered data
      brushData <- if(input$graphSelect == "Scatterplot") {
        as.data.frame(newData) %>% select(full_name, year, input$varSelect, input$varXSelect)
      } else {
        return()
      }
      
      brushedPoints(brushData, input$plot_brush)
    }) #end brushed points output
    
    # })
  }) #end observe explore tab user inputs
  
  #Explore tab plots
  output$explorePlot <- renderPlot(plot1())
  
  #plot selection
  plot1 <- reactive({
    #get filtered data
    newData <- getData()
    
    #get value of user inputs
    player <- input$playerSelect
    compare <- input$compareSelect
    
    #get player-specific data
    playerData <- newData %>% filter(full_name == player)
    compareData <- newData %>% filter(full_name == compare)
    selectData <- bind_rows(playerData, compareData)
    
    #factor year
    newData$year <- as.factor(newData$year)
    selectData$year <- as.factor(selectData$year)
    playerData$year <- as.factor(playerData$year)
    
    yearTitle <- if(input$years[1]==input$years[2]) {
      paste0("(", input$years[1], ")")
    } else {
      paste0("(", min(input$years),"-", max(input$years), ")")
    }
    
    #create plot
    if(input$graphSelect == "Scatterplot") {
      g <- ggplot(newData, aes_string(x = input$varXSelect, y = input$varSelect))
      if(input$compare == 1) {
        if(input$regression == 1) {
          g + geom_jitter(aes(col=year), alpha = input$opacity/5, width = input$jitter/5) + geom_smooth(method = "lm", se = TRUE) + geom_point(data = selectData, colour = "red") + geom_text_repel(data=selectData, label = paste0(selectData$full_name," (",selectData$year,")")) + labs(title = paste0(input$varSelect, " vs ", input$varXSelect, " - ", input$positionSelect, " ", yearTitle), subtitle = paste0(input$playerSelect, " and ", input$compareSelect, " highlighted")) 
        } else {
          g + geom_jitter(aes(col=year), alpha = input$opacity/5, width = input$jitter/5) + geom_point(data = selectData, colour = "red") + geom_text_repel(data=selectData, label = paste0(selectData$full_name," (",selectData$year,")")) + labs(title = paste0(input$varSelect, " vs ", input$varXSelect, " - ", input$positionSelect, " ", yearTitle), subtitle = paste0(input$playerSelect, " and ", input$compareSelect, " highlighted"))
        }
      } else {
        if(input$regression == 1) {
          g + geom_jitter(aes(col=year), alpha = input$opacity/5, width = input$jitter/5) + geom_smooth(method = "lm", se = TRUE) + geom_point(data = playerData, colour = "red") + geom_text_repel(data=playerData, label = paste0(playerData$full_name," (",playerData$year,")")) + labs(title = paste0(input$varSelect, " vs ", input$varXSelect, " - ", input$positionSelect, " ", yearTitle), subtitle = paste0(input$playerSelect," highlighted")) 
        } else {
          g + geom_jitter(aes(col=year), alpha = input$opacity/5, width = input$jitter/5) + geom_point(data = playerData, colour = "red") + geom_text_repel(data=playerData, label = paste0(playerData$full_name," (",playerData$year,")")) + labs(title = paste0(input$varSelect, " vs ", input$varXSelect, " - ", input$positionSelect, " ", yearTitle), subtitle = paste0(input$playerSelect," highlighted"))
        }
      }
    } else if(input$graphSelect == "Boxplot") {
      g <- ggplot(newData, aes_string("year", y = input$varSelect))
      if(input$compare == 1) {
        g + geom_boxplot() + geom_jitter(alpha = input$opacity/5, width = input$jitter/5) + geom_point(data = selectData, colour = "red") + geom_text_repel(data=selectData, label = paste0(selectData$full_name, " (",selectData$year, ")"), nudge_y = 0.05) + labs(title = paste0(input$varSelect, " by year", " - ", input$positionSelect, " ", yearTitle), subtitle = paste0(input$playerSelect, " and ", input$compareSelect, " highlighted"))
      } else {
        g + geom_boxplot() + geom_jitter(alpha = input$opacity/5, width = input$jitter/5) + geom_point(data = playerData, colour = "red") + geom_text_repel(data=playerData, label = paste0(playerData$full_name, " (",playerData$year, ")"), nudge_y = 0.05) + labs(title = paste0(input$varSelect, " by year", " - ", input$positionSelect, " ", yearTitle), subtitle = paste0(input$playerSelect, " highlighted"))
      }
      
    } 
  }) #end plot 
  
  #reactive year title
  yearTitle <- reactive({
    if(input$years[1]==input$years[2]) {
      paste0("_",input$years[1])
    } else {
      paste0("_",min(input$years),"_", max(input$years))
    }
  }) #end reactive year title
  
  #plot download handler
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("EPL_",input$positionSelect, yearTitle(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1200)
      plot(plot1())
      dev.off()
    }
  ) #end plot download handler
  
  #data download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("EPL_",input$positionSelect, yearTitle(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getData(), file, row.names = FALSE)
    }
  ) #end data download handler
  
  #end explore tab
  
  # ____________________________________________________________________________________________________________________
  
  # #Model tab data
  getDataModel <- reactive({
    newDataModel <- epl %>% filter(position == input$positionSelectModel & year == input$yearsModel) %>% select_if(is.numeric)
  }) #end explore tab data
  
  #Model tab user inputs
  #dependent variable
  observe({
    newDataModel <- getDataModel()
    numVars <- newDataModel %>% select_if(is.numeric)
    numVars <- select_if(numVars, colSums(numVars)>0) %>% select(-year) %>% colnames()
    varList <- as.list(numVars)
    updateSelectInput(session, "varDependent", choices = varList, selected = "total_points")
  }) #end dependent variable user input
  
  #independent variables
  observe({
    newDataModel <- getDataModel()
    numVars <- newDataModel %>% select_if(is.numeric)
    numVars <- select_if(numVars, colSums(numVars)>0) %>% select(-year) %>% colnames()
    varList <- as.list(numVars)
    updateCheckboxGroupInput(session, "varSelectModel", choices = varList, selected = varList[c(1,3:6)])
  }) #end independent variable user input
  
  #player select
  observe({
    newDataModel <- epl %>% filter(position == input$positionSelectModel & year == input$yearsModel)
    
    #update player list
    updateSelectInput(session, "playerSelectModel",
                      choices = newDataModel$full_name %>% sort())
  }) 
  
  #update players
  playerData <- reactive({
    epl %>% filter(full_name == input$playerSelectModel & year == input$yearsModel)
  })
  
  
  #update player values
  observe({
    selectPlayerData <- playerData()
    updateNumericInput(session, "total_points", value = selectPlayerData$total_points)
    updateNumericInput(session, "now_cost", value = as.numeric(selectPlayerData$now_cost))
    updateNumericInput(session, "points_per_game", value = as.numeric(selectPlayerData$points_per_game))
    updateNumericInput(session, "minutes", value = as.numeric(selectPlayerData$minutes))
    updateNumericInput(session, "goals_scored", value = as.numeric(selectPlayerData$goals_scored))
    updateNumericInput(session, "assists", value = as.numeric(selectPlayerData$assists))
    updateNumericInput(session, "bonus", value = as.numeric(selectPlayerData$bonus))
    updateNumericInput(session, "bps", value = as.numeric(selectPlayerData$bps))
    updateNumericInput(session, "goals_conceded", value = as.numeric(selectPlayerData$goals_conceded))
    updateNumericInput(session, "own_goals", value = as.numeric(selectPlayerData$own_goals))
    updateNumericInput(session, "penalties_missed", value = as.numeric(selectPlayerData$penalties_missed))
    updateNumericInput(session, "yellow_cards", value = as.numeric(selectPlayerData$yellow_cards))
    updateNumericInput(session, "red_cards", value = as.numeric(selectPlayerData$red_cards))
    updateNumericInput(session, "saves", value = as.numeric(selectPlayerData$saves))
    updateNumericInput(session, "clean_sheets", value = as.numeric(selectPlayerData$clean_sheets))
    updateNumericInput(session, "creativity", value = as.numeric(selectPlayerData$creativity))
    updateNumericInput(session, "ict_index", value = as.numeric(selectPlayerData$ict_index))
    updateNumericInput(session, "influence", value = as.numeric(selectPlayerData$influence))
    updateNumericInput(session, "threat", value = as.numeric(selectPlayerData$threat))
    updateNumericInput(session, "selected_by_percent", value = as.numeric(selectPlayerData$selected_by_percent))
 })
  
  #end model tab user inputs
  
  #training data set
  train <- reactive({
    
    #get data
    dataModel <- getDataModel()
    
    #set seed prior to sampling
    set.seed(1)
    
    #sample observations for training data set
    train <- sample(1:nrow(dataModel), size = nrow(dataModel)*(as.numeric(input$train)/100))
    
    #use remaining samples for test data set
    test <- dplyr::setdiff(1:nrow(dataModel), train)
    
    return(list(train,test))
    
  }) #end training data set
  
  #Plot output
  output$Modelplot <- renderPlot(
    if(input$modelSelect == "Multiple Linear"){
      plot(plotModel()[[1]], which = 1, main = "Multiple Linear Regression: Residuals vs Fitted Values")
    } else {
      fitModel <- plotModel()[[1]]
      predictValues <- predict(fitModel)
      actualValues <- fitModel$finalModel$data$y
      plot(predictValues-actualValues, ylab = "Residuals", xlab = "Fitted Values", main = "Boosted Trees Model: Residuals vs Fitted Values"); abline(h=0, lty=2)
    }
  ) #end plot output
  
  #Model tab plots
  plotModel <- reactive({
    
    #get value of user inputs
    center <- input$centerModel
    
    #get filtered data
    newDataModel <- getDataModel()
    
    #get test train
    train_test <- train()
    train <- train_test[[1]]
    test <- train_test[[2]]
    
    #create separate training and test data frames
    dfTrain <- newDataModel[train, ]
    dfTest <- newDataModel[test, ]
    
     #formula step 1
    if(input$varInteract == 1 & input$modelSelect == "Multiple Linear") {
      independent <- paste(input$varSelectModel, collapse = "*")
    } else {
      independent <- paste(input$varSelectModel, collapse = "+")
    }
    
    #formula step 2
    fm <- paste(input$varDependent,"~",independent)
    
    #cross-validation
    trctrl <- trainControl(method = "repeatedcv", number = input$folds, repeats = input$repeats)
    
    #tuning grid
    gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                            n.trees = input$numTrees, 
                            shrinkage = 0.1,
                            n.minobsinnode = 20
                            )
    
    #fit models
    if(input$modelSelect == "Multiple Linear") {
      #linear model
      fit <- lm(formula = as.formula(fm), data = as.data.frame(dfTrain))
    } else if(input$CV == 1){
      if(input$trees == 1) {
        fit <- caret::train(as.formula(fm), data = as.data.frame(dfTrain), method = "gbm", trControl = trctrl, verbose = FALSE, tuneGrid = gbmGrid)
      } else {
        fit <- caret::train(as.formula(fm), data = as.data.frame(dfTrain), method = "gbm", trControl = trctrl, verbose = FALSE)
      }
    } else {
      if(input$trees == 1) {
        fit <- caret::train(as.formula(fm), data = as.data.frame(dfTrain), method = "gbm", verbose = FALSE, tuneGrid = gbmGrid)
      } else {
        fit <- caret::train(as.formula(fm), data = as.data.frame(dfTrain), method = "gbm", verbose = FALSE)
      }    
    }
    
    return(list(fit, dfTest, dfTrain))
    
  }) #end model plots reactive
  
  #RMSE output
  output$predictModel <- renderPrint({
    dfTest <- plotModel()[[2]]
    dependent <- as.data.frame(dfTest) %>% select(input$varDependent)
    #get filtered data
    predict <- predict(plotModel()[[1]], newdata = as.data.frame(dfTest))
    #sqrt(mean((predict-dependent)^2))
    error <- unlist(predict-dependent)
    sqrt(mean((error)^2))
  }) #end RMSE output
  
  #summary output
  output$summaryModel <- renderPrint({
    #get filtered data
    summary(plotModel()[[1]])
  }) #end summary output
  
  #predict value output
  observeEvent(input$predictButton, {
    #var dependent
    output$predictInfo <- renderText({
      paste0("Prediction for ", input$varDependent)
    })
    
    output$predictValue <- renderPrint({
      total_points <- input$total_points
      now_cost <- input$now_cost
      points_per_game <- input$points_per_game
      minutes <- input$minutes
      goals_scored <- input$goals_scored
      assists <- input$assists
      bonus <- input$bonus
      bps <- input$bps
      goals_conceded <- input$goals_conceded
      own_goals <- input$own_goals
      penalties_missed <- input$penalties_missed
      penalties_saved <- input$penalties_saved
      yellow_cards <- input$yellow_cards
      red_cards <- input$red_cards
      saves <- input$saves
      clean_sheets <- input$clean_sheets
      creativity <- input$creativity
      ict_index <- input$ict_index
      influence <- input$influence
      threat <- input$threat
      selected_by_percent <- input$selected_by_percent
      
      newData <- as.data.frame(cbind(total_points, now_cost, points_per_game, minutes, goals_scored, assists, bonus, bps, goals_conceded, own_goals, 
                       penalties_missed, penalties_saved, yellow_cards, red_cards, saves, clean_sheets, creativity, ict_index, influence, threat, selected_by_percent))
      
      fit<-plotModel()[[1]]

      predict(fit, newData)
      
    }) #end predict value output
  })
 
  
  #plot download handler
  output$downloadModelPlot <- downloadHandler(
    filename = function() {
      paste("EPL", input$modelSelect, input$positionSelectModel, input$yearsModel, ".png", sep ="_")
    },
    content = function(file) {
      png(file, width = 1200)
      if(input$modelSelect == "Multiple Linear"){
        plot(plotModel()[[1]], which = 1, main = "Multiple Linear Regression: Residuals vs Fitted Values")
      } else {
        fitModel <- plotModel()[[1]]
        predictValues <- predict(fitModel)
        actualValues <- fitModel$finalModel$data$y
        plot(predictValues-actualValues, ylab = "Residuals", xlab = "Fitted Values", main = "Boosted Trees Model: Residuals vs Fitted Values"); abline(h=0, lty=2)
      }
      dev.off()
    }
  ) #end plot download handler
  
  #data download handler (training data)
  output$downloadModelData <- downloadHandler(
    filename = function() {
      paste0("EPL_", input$modelSelect, "_training_data.csv")
    },
    content = function(file) {
        write.csv(plotModel()[[3]], file, row.names = FALSE)
    }
  ) #end data download handler
  
  #data download handler (testing data)
  output$downloadTestData <- downloadHandler(
    filename = function() {
      paste0("EPL_", input$modelSelect, "_testing_data.csv")
    },
    content = function(file) {
      write.csv(plotModel()[[2]], file, row.names = FALSE)
    }
  ) #end data download handler
  
  # #end model tab
  
  # ____________________________________________________________________________________________________________________
  
  #PCA tab data
  getDataPCA <- reactive({
    newDataPCA <- epl %>% filter(position == input$positionSelectPCA & year == input$yearsPCA)
  }) #end PCA tab data
  
  #update principal component numbers for biplot axes selection 
  observe({
    varSelectNum <- (length(input$varSelectPCA))
    PCAvarNum <- as.list(seq(from = 1, to = varSelectNum))
    updateSelectInput(session, "xVarPCA", choices = PCAvarNum, selected = PCAvarNum[1])
    updateSelectInput(session, "yVarPCA", choices = PCAvarNum, selected = PCAvarNum[2])
  })
  
  #PCA tab user inputs
  observe({
    newDataPCA <- getDataPCA()
    numVars <- newDataPCA %>% select_if(is.numeric)
    numVars <- select_if(numVars, colSums(numVars)>0) %>% select(-year) %>% colnames()
    varList <- as.list(numVars)
    updateCheckboxGroupInput(session, "varSelectPCA", choices = varList, selected = varList)
  }) #end PCA tab user inputs
  
  #Plot output
  output$PCAplot <- renderPlot(plotPCA())
  
  #PCA tab plots
  plotPCA <- reactive({
    
    #get value of user inputs
    center <- input$centerPCA
    
    #get filtered data
    newDataPCA <- getDataPCA()
    
    #select variables
    PCAData <- newDataPCA %>% select(input$varSelectPCA)
    
    #center data option
    if(center == 1) {
      PCs <- prcomp(PCAData, center = TRUE, scale = TRUE)
    } else {
      PCs <- prcomp(PCAData, center = FALSE, scale = FALSE)
    }
    
    if(input$plotPCA == "Screeplot") {
      #create plot
      fviz_eig(PCs, addlabels = TRUE)
    } else {
      #create plot
      fviz_pca_biplot(PCs, axes = c(as.numeric(input$xVarPCA), as.numeric(input$yVarPCA)), repel = TRUE)
    }
    
  }) #end PCA plots reactive
  
  #plot download handler
  output$downloadPCAPlot <- downloadHandler(
    filename = function() {
      paste0("EPL_", input$positionSelectPCA, "_", input$plotPCA, "_", input$yearsPCA, ".png")
    },
    content = function(file) {
      png(file, width = 1200)
      plot(plotPCA())
      dev.off()
    }
  ) #end PCA plot download handler
  
  #create separate function for downloading PCA data
  getPCAData <- function(){
    #get value of user inputs
    center <- input$centerPCA
    
    #get filtered data
    newDataPCA <- getDataPCA()
    
    #select variables
    PCAData <- newDataPCA %>% select(input$varSelectPCA)
    
    #center data option
    if(center == 1) {
      PCs <- prcomp(PCAData, center = TRUE, scale = TRUE)
    } else {
      PCs <- prcomp(PCAData, center = FALSE, scale = FALSE)
    }
    
    #user inputs
    if(input$PCAdataSelect == "Importance of Components") {
      dataPCA <- summary(PCs)[6]
    } else if(input$PCAdataSelect == "Loadings") {
      dataPCA <- PCs$rotation
    } else {
      dataPCA <- PCs$x
    }
    
    return(list(as.data.frame(dataPCA),PCAData))
  } #end PCA data download function
  
  #data download handler (selected data)
  output$downloadPCAData <- downloadHandler(
    filename = function() {
      paste0("PCA_", input$PCAdataSelect, ".csv")
    },
    content = function(file) {
      write.csv(getPCAData()[[1]], file, row.names = TRUE)
    }
  ) #end data download handler
  
  #data download handler (raw data)
  output$downloadPCADataRaw <- downloadHandler(
    filename = function() {
      paste0("PCA_raw_data.csv")
    },
    content = function(file) {
      write.csv(getPCAData()[[2]], file, row.names = FALSE)
    }
  ) #end data download handler
  
  #create separate reactive argument for PCA summary
  PCA <- reactive({
    #get value of user inputs
    center <- input$centerPCA
    
    #get filtered data
    newDataPCA <- getDataPCA()
    
    #select variables
    PCAData <- newDataPCA %>% select(input$varSelectPCA)
    
    #center data option
    if(center == 1) {
      PCs <- prcomp(PCAData, center = TRUE, scale = TRUE)
    } else {
      PCs <- prcomp(PCAData, center = FALSE, scale = FALSE)
    }
  }) #end PCA summary
  
  #PCA summary output  
  output$summaryPCA <- renderPrint({
    #get filtered data
    summary(PCA())
  }) #end PCA summary output
  
  #end PCA tab
  
  # ____________________________________________________________________________________________________________________
  
  #Data tab data
  getDataTable <- reactive({
    if(input$positionSelectDT == "ALL") {
      newData <- epl %>% filter(year <= max(input$yearsDT) & year >=min(input$yearsDT)) %>% select(as.vector(input$varSelectData))
    } else {
      newData <- epl %>% filter(position == input$positionSelectDT & year <= max(input$yearsDT) & year >=min(input$yearsDT)) %>% select(as.vector(input$varSelectData))
    }
  }) #end data tab data
  
  #Data tab output
  output$table <- renderDT({
    getDataTable()
  }) #end data tab output
  
  #data download handler
  output$dataDownload <- downloadHandler(
    filename = "EPL_data_table.csv",
    content = function(file) {
      write.csv(getDataTable(), file, row.names = FALSE)
    }
  ) #end data download handler
  
  #end data tab
  
}) #end shiny server arguments