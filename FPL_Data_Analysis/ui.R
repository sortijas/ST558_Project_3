library(ggplot2)
library(shinydashboard)
library(DT)
library(factoextra)
library(plotly)

#sidebar design
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("futbol")),
    menuItem("Explore", tabName = "explore", icon = icon("chart-bar")),
    menuItem("PCA", tabName = "PCA", icon = icon("chart-pie")),
    menuItem("Model", tabName = "model", icon = icon("chart-line")),
    menuItem("Data", tabName = "data", icon = icon("table"))
  )
)

#_____________________________________________________________________________________

#body design  
body <- dashboardBody(
  tabItems(
    #start about tab
    tabItem(tabName = "about",
            tabsetPanel(
              tabPanel("About",
                       h3("TL;DR"),
                       h4("The ", tags$a(href="https://fantasy.premierleague.com/", "Fantasy Premier League "), "(FPL) Data Analysis App allows users to explore and model historical and current FPL player data."),
                       h3("Background"),
                       h4("FPL is the Premier League's official fantasy sports competition which tracks the actual Premier League player performances. FPL players assemble teams teams of 2 goalkeepers, 5 defenders, 5 midfielders, and 3 forwards keeping within a budget of GBP 100 million. Each gameweek, FPL players put together a team of their 11 best Premier League players. Points are accumulated for certain in-game acts such as goals and assists. The amount of points accumulated varies by position, and some positions have variables that apply only to them. As such, exploring data and analyzing data in this App is mostly confined to by-position groupings."),
                       h4("The complete scoring rules can be found", tags$a(href="https://fantasy.premierleague.com/help/rules", "here."))
              ), 
              tabPanel("Data",
                       h3("Data Sets"),
                       h4("The App contains aggregated data from the 2016-17 season to the 2019-20 season. Because the 2019-20 season has not begun, the in-game player performance data is identical to the previous season. However, player attribute variables, including cost ('now_cost' variable) for 2019-20 are updated for the start of the new season. A future build of this App will allow for scraping the FPL API to provide week-by-week player performance data."),
                       h3("Data Dictionary"),
                       h4(tags$ul(
                         tags$li(tags$b("full_name")," - player name, as referenced in the FPL teamsheets"), 
                         tags$li(tags$b("position")," - player position (3 letter abbreviation)"),
                         tags$li(tags$b("total_points")," - aggregate fantasy points scored by the player"),
                         tags$li(tags$b("now_cost")," - cost of the player in the concluding Gameweek of the each season (in units of 0.1 million pounds)"),
                         tags$li(tags$b("points_per_game")," - aggregate fantasy points / total games played"),
                         tags$li(tags$b("minutes")," - aggregate minutes played by the player"),
                         tags$li(tags$b("goals_scored")," - aggregate goals scored by the player"),
                         tags$li(tags$b("assists")," - aggregate assists provided by the player"),
                         tags$li(tags$b("bonus")," - aggregate Gameweek bonus points received by the player"),
                         tags$li(tags$b("bps")," - aggregate bonus points from", tags$a(href="https://www.premierleague.com/news/106533", "Opta bonus points system")),
                         tags$li(tags$b("goals_conceded")," - aggregate number of goals conceded while the player was on the field"),
                         tags$li(tags$b("own_goals")," - aggregate own goals scored by the player"),
                         tags$li(tags$b("penalties_missed")," - aggregate penalties missed by the player"),
                         tags$li(tags$b("penalties_saved")," - aggregate penalties saved by the player (usually applies to goalkeepers)"),
                         tags$li(tags$b("yellow_cards")," - aggregate yellow cards received by the player"),
                         tags$li(tags$b("red_cards")," - aggregate red cards received by the player"),
                         tags$li(tags$b("saves")," - aggregate saves made by the player (usually applies to goalkeepers)"),
                         tags$li(tags$b("clean_sheets")," - aggregate number of games played with no opposing goals allowed"),
                         tags$li(tags$b("ict_index")," - index combining the Influence, Creativity and Threat data "),
                         tags$li(tags$b("creativity")," - assesses player performance in terms of producing goalscoring opportunities for others"),
                         tags$li(tags$b("influence")," - evaluates the degree to which that player has made an impact on a single match or throughout the season"),
                         tags$li(tags$b("threat")," - value that examines a player's threat on goal")
                       ))
              ), 
              tabPanel("Features",
                       h3("Explore"),
                       h4("Plot player variables by position over single or multiple seasons. Individual and comparison players can be highlighted relative to other players in their position. Outputs include: downloadable plots (scatterplot and boxplot), interactive scatterplot featuring selectable plot area, downloadable raw data."),
                       h3("PCA"),
                       h4("Principal Component Analysis (PCA) by position and season. PCA is an unsupervised learning method which reduces multiple variables into fewer linear combinations. Users can select variables and other parameters to build the PCA model and plots. Outputs include: downloadable plots (biplot or screeplot), summary of importance of principal compontents, and downloadable PCA data (summary, loadings, or rotated data)."),
                       h3("Model"),
                       h4("Select between multiple linear regression and boosted trees supervised learning methods. Users can select method-specific inputs and tuning parameters. Outputs include: Plots of residuals vs fitted data, root mean squared errors (RMSE, see formula below) for testing data, model fit summaries, and interface to use models for prediction."),
                       h4(withMathJax(helpText("$$RMSE = \\sqrt{\\frac{\\sum_{i=1}^n(y_i-\\hat{y}_i)^2}{n}}$$"))),
                       h3("Data"),
                       h4("View raw data in data table format. Filter by position or year(s) and select different variables for inclusion in data table. Outputs include: searchable and sortable data table which can be downloaded.")
              )
            )
    ), #end about tab
    
    #_____________________________________________________________________________________
    
    #start graphs tab
    tabItem(tabName = "explore",
            fluidRow(
              column(3,
                     box(width = 12, title = "Player Filter",
                         selectInput("positionSelect", label = "Position", 
                                     choices = list("Defenders" = "DEF", "Forwards" = "FWD", "Midfielders" = "MID", "Goalkeepers" = "GK")),
                         sliderInput("years", "Season(s)",
                                     min = 2017, max = 2020, sep = "", value = c(2020,2020)),
                         selectInput("playerSelect", label = "Player", 
                                     choices = NULL),
                         checkboxInput("compare", tags$b("Compare player")),
                         conditionalPanel(
                           condition = "input.compare == '1'",
                           selectInput("compareSelect", label = "Comparison Player", 
                                       choices = NULL)
                         )
                     ),
                     box(width = 12, title = "Plot Parameters",
                         selectInput("graphSelect", label = "Plot Type", 
                                     choices = list("Scatterplot", "Boxplot")),
                         conditionalPanel(
                           condition = "input.graphSelect != 'Boxplot'",
                           checkboxInput("regression", tags$b("Regression Line"), value = TRUE),
                           selectInput("varXSelect", label = "X-Axis Variable",
                                       choices = NULL)
                         ),
                         selectInput("varSelect", label = "Y-Axis Variable", 
                                     choices = NULL),
                         sliderInput("opacity", "Opacity",
                                     min = 1, max = 5, step = 1, value = 2),
                         sliderInput("jitter", "Jitter",
                                     min = 0, max = 10, step = 1, value = 1)
                     )
              ),
              column(9,
                     box(width = 12,
                         plotOutput("explorePlot", height = 500,
                                    brush = brushOpts(id = "plot_brush")
                         ),
                         downloadButton("downloadPlot", "Plot"),
                         downloadButton("downloadData", "Data")
                     )
              ),
              column(4,
                     h4(textOutput("YSummary")),
                     verbatimTextOutput("summaryYExplore")
              ),
              column(4,
                     conditionalPanel(
                       condition = "input.graphSelect == 'Scatterplot'",
                       h4(textOutput("XSummary")),
                       verbatimTextOutput("summaryXExplore")
                     )
              ),
              column(4,
                     conditionalPanel(
                       condition = "input.graphSelect == 'Scatterplot'",
                       h4("Selected graph region data"),
                       verbatimTextOutput(("brush_info"))
                     )
              ),
              column(5,
                     h4("Player summary"),
                     verbatimTextOutput("playerExplore")
              )
              
            )  
    ), #end graphs tab
    
    #_____________________________________________________________________________________
    
    #start PCA tab
    tabItem(tabName = "PCA",
            fluidRow(
              column(3,
                     box(width = 12, title = "PCA Parameters",
                         selectInput("positionSelectPCA", label = "Position", 
                                     choices = list("Defenders" = "DEF", "Forwards" = "FWD", "Midfielders" = "MID", "Goalkeepers" = "GK")),
                         selectInput("yearsPCA", "Season",
                                     choices = list(2017, 2018, 2019, 2020), selected = 2020),
                         checkboxInput("varPCA", tags$b("Select Variables"), value = FALSE),
                         conditionalPanel(
                           condition = "input.varPCA == 1",
                           checkboxGroupInput("varSelectPCA", label = "Variables", 
                                              choices = NULL)
                         ),
                         checkboxInput("centerPCA", tags$b("Center and Scale Data"), value = TRUE),
                         selectInput("plotPCA", "Plot Type",
                                     choices = list("Biplot","Scree plot"), selected = "Biplot"),
                         conditionalPanel(
                           condition = "input.plotPCA == 'Biplot'",
                           selectInput("xVarPCA", "X-Axis Principal Component",
                                       choices = NULL),
                           selectInput("yVarPCA", "Y-Axis Principal Component",
                                       choices = NULL)
                         )
                     ),
                     box(width = 12, title = "Data Download Options",
                         selectInput("PCAdataSelect", "Select Data",
                                     choices = list("Importance of Components","Loadings","Rotated Data"), selected = "Importance of Components"),
                         downloadButton("downloadPCAData", "Selected Data")
                     )
              ),
              column(9,
                     box(width = 14,
                         plotOutput("PCAplot", height = 500),
                         downloadButton("downloadPCAPlot", "Plot"),
                         downloadButton("downloadPCADataRaw", "Raw Data")
                     ),
                     h4("Model Summary"),
                     verbatimTextOutput(("summaryPCA"))
                     
              )
            )
    ), #end PCA tab
    
    #_____________________________________________________________________________________
    
    #Start models tab
    tabItem(tabName = "model",
            tabsetPanel(
              tabPanel("Model",
                       fluidRow(
                         column(3,
                                box(width = 12, title = "Model Parameters",
                                    selectInput("positionSelectModel", label = "Position",
                                                choices = list("Defenders" = "DEF", "Forwards" = "FWD", "Midfielders" = "MID", "Goalkeepers" = "GK")),
                                    selectInput("yearsModel", "Season",
                                                choices = list(2017, 2018, 2019, 2020), selected = 2020),
                                    selectInput("modelSelect", label = "Model Type",
                                                choices = list("Multiple Linear Regression", "Boosted Trees")),
                                    selectInput("varDependent", label = "Select Dependent Variable",
                                                choices = NULL),
                                    checkboxInput("varModel", tags$b("Select Independent Variables"), value = FALSE),
                                    conditionalPanel(
                                      condition = "input.varModel == 1",
                                      checkboxGroupInput("varSelectModel", label = "Independent Variables",
                                                         #choices = list("total_points", "points_per_game", "minutes", "goals_scored", "assists", "bonus", "bps", "goals_conceded", "own_goals", "penalties_missed", "penalties_saved", "yellow_cards", "red_cards", "saves", "clean_sheets", "creativity", "ict_index", "influence", "threat")
                                                         choices = NULL)
                                    ),
                                    sliderInput("train", "Training Data (% of data set)",
                                                min = 50, max = 100, step = 5, value = 90),
                                    conditionalPanel(
                                      condition = "input.modelSelect == 'Multiple Linear Regression'",
                                      checkboxInput("varInteract", tags$b("Interact Variables"), value = FALSE)
                                    ),
                                    conditionalPanel(
                                      condition = "input.modelSelect == 'Boosted Trees'",
                                      checkboxInput("CV", tags$b("Cross Validation"), value = FALSE),
                                      conditionalPanel(
                                        condition = "input.CV == 1",
                                        sliderInput("folds", "Folds", min = 2, max = 10, value = 10),
                                        sliderInput("repeats", "Repeats", min = 2, max = 10, value = 3)
                                      ),
                                      checkboxInput("trees", tags$b("Select Trees"), value = FALSE),
                                      conditionalPanel(
                                        condition = "input.trees == 1",
                                        sliderInput("numTrees", "Trees", min = 50, max = 1000, step = 50, value = 500)
                                      )
                                    )
                                ),
                                box(width = 12, title = "Output Options",
                                    selectInput("modelPlotSelect", "Plot",
                                                choices = NULL),
                                    selectInput("modelSummarySelect", "Summary",
                                                choices = NULL)
                                )
                         ),
                         column(9,
                                box(width = 14,
                                    plotOutput("Modelplot", height = 500),
                                    downloadButton("downloadModelPlot", "Plot"),
                                    downloadButton("downloadModelData", "Training Data"),
                                    downloadButton("downloadTestData", "Testing Data")
                                ),
                                h4("Testing Data RMSE"),
                                verbatimTextOutput(("predictModel")),
                                h4("Model Summary"),
                                verbatimTextOutput(("summaryModel"))
                         )
                       ) #end fluidRow
              ), #end tabPanel "Model"
              tabPanel("Predict",
                       fluidRow(
                         column(3,
                                box(width = 12, title = "Select Player or Enter Values",
                                    # actionButton("predictButton", label = "Predict"),
                                    # br(),
                                    # br(),
                                    selectInput("playerSelectModel", label = "Player", 
                                                choices = NULL),
                                    numericInput("total_points","total_points", value = NULL), #total_points
                                    numericInput("now_cost","now_cost", value = NULL), #now_cost
                                    numericInput("points_per_game","points_per_game", value = NULL), #points_per_game
                                    numericInput("minutes","minutes", value = NULL), #minutes
                                    numericInput("goals_scored","goals_scored", value = NULL), #goals_scored
                                    numericInput("assists","assists", value = NULL), #assists
                                    numericInput("bonus","bonus", value = NULL), #bonus
                                    numericInput("bps","bps", value = NULL), #bps
                                    numericInput("goals_conceded","goals_conceded", value = NULL), #goals_conceded
                                    numericInput("own_goals","own_goals", value = NULL), #own_goals
                                    numericInput("penalties_missed","penalties_missed", value = NULL), #penalties_missed
                                    numericInput("penalties_saved","penalties_saved", value = NULL), #penalties_saved
                                    numericInput("yellow_cards","yellow_cards", value = NULL), #yellow_cards
                                    numericInput("red_cards","red_cards", value = NULL), #red_cards
                                    numericInput("saves","saves", value = NULL), #saves
                                    numericInput("clean_sheets","clean_sheets", value = NULL), #clean_sheets
                                    numericInput("creativity","creativity", value = NULL), #creativity
                                    numericInput("ict_index","ict_index", value = NULL), #ict_index
                                    numericInput("influence","influence", value = NULL), #influence
                                    numericInput("threat","threat", value = NULL), #threat
                                    numericInput("selected_by_percent","selected_by_percent", value = NULL) #threat
                                    ) #end box
                           
                         ), #end column1
                         column(3,
                                box(width = 12,
                                    textOutput("predictInfo"),
                                    verbatimTextOutput("predictValue")
                                    ) #end box
                                ) #end column2
                         
                       ) #end fluidRow
              ) #end tabPanel "Predict"
              
            ) #end tabsetPanel
            
    ), #end model tab
    
    #_____________________________________________________________________________________
    
    #Data table tab
    tabItem(tabName = "data",
            fluidRow(
              column(3,
                     box(width = 12,
                         selectInput("positionSelectDT", label = "Position", 
                                     choices = list("Defenders" = "DEF", "Forwards" = "FWD", "Midfielders" = "MID", "Goalkeepers" = "GK", "All" = "ALL")),
                         sliderInput("yearsDT", "Season(s)",
                                     min = 2017, max = 2020, sep = "", value = c(2020,2020)),
                         checkboxGroupInput("varSelectData", label = "Select Variables",
                                            choices = list("year", "full_name", "position", "total_points", "now_cost", "points_per_game", "minutes", "goals_scored", "assists", "bonus", "bps", "goals_conceded", "own_goals", "penalties_missed", "penalties_saved", "yellow_cards", "red_cards", "saves", "clean_sheets", "creativity", "ict_index", "influence", "threat"),
                                            selected = list("year", "full_name", "position", "total_points", "now_cost"))
                     )
              ),
              column(9,
                     downloadButton("dataDownload", "Data"),
                     br(),
                     br(),
                     DTOutput("table")
              )
            )
    ) #end data tab
    
  )  #end tabItems
)  #end dashboardBody

#_____________________________________________________________________________________

#App arguments
dashboardPage(
  dashboardHeader(title = "FPL Data Analysis"),
  sidebar,
  body
)
