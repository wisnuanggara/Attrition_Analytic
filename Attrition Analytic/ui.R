library(tidyverse)
library(skimr)
library(GGally)
library(plotly)
library(viridis)
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(xgboost)
#library(h2o)
library(ggcorrplot)
library(rpart.plot)
library(corrgram)
library(lightgbm)
library(ggplot2)
library(ggthemes)
library(psych)
library(scales)
library(treemap)
library(treemapify)
library(repr)
library(cowplot)
library(magrittr)
library(ggpubr)
library(RColorBrewer)
library(plotrix)
library(ggrepel)
library(forcats)
library(reshape2)
library(caTools)
library(tree)
library(rattle)
library(shinydashboard)



shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "Attrition Analytics"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("General Information ", tabName = "general", icon = icon("user-friends")),
        menuItem("Gender Analysis", tabName = "gender_analysis", icon = icon("venus-mars")),
        menuItem("Genderation and Education", tabName = "gen_edu_analysis", icon = icon("university")),
        menuItem("Income", tabName = "income", icon = icon("file-invoice-dollar")),
        menuItem("Working Environment", tabName = "environment", icon = icon("place-of-worship")),
        menuItem("An In-depth Look", tabName = "depth", icon = icon("deezer")),
        menuItem("Analysis and Models", tabName = "models", icon = icon("chart-line"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "general",
                fluidRow(
                  box(width = 12,
                    plotOutput('distriLabel')
                  ),
          )
        ),
        tabItem(tabName = "gender_analysis",
                fluidRow(
                  box(width = 6,
                      title = "Age Distribution by Gender",
                      plotOutput('ageDistriGender')
                    
                  ),
                  box(width = 6,
                      title = "Distribution of Job Satisfaction",
                      plotOutput('distriJobSatis')
                    
                  ),
                  box(width = 6,
                      title = "Monthly Income by Gender",
                      plotOutput('monthIncomeGender')
                      
                  ),
                  box(width = 6,
                      title = "Average Income and Presence by Department",
                      plotOutput('avgIncDeoart')
                      
                  )
                )
          
        ),
        tabItem(tabName = "gen_edu_analysis",
                fluidRow(
                  box(width = 12,
                      title = "Distribution of Number of Companies Worked by Attrition and Age",
                      plotOutput('numComAttrAge')
                      
                  ),
                  box(width = 6,
                      title = "Attrition by Educational Level",
                      plotOutput('attrEduLevel')
                      
                  ),
                  box(width = 6,
                      title = "Attrition by Educational Level (%)",
                      plotOutput('numComAttrAgePercent')
                      
                  )
                )
                
        ),
        tabItem(tabName = "income",
                fluidRow(
                  box(width = 6,
                      title = "Average Income by Department",
                      plotOutput('avgIncDepart')
                  ),
                  box(width = 6,
                      title = "Determining Satisfaction by Income",
                      plotOutput('deteSatInc')
                  ),
                  box(width = 6,
                      title = "Income and the Level of Attrition",
                      plotOutput('incLevAttr')
                  ),
                  box(width = 6,
                      title = "Average and Percent Difference of Daily Rates",
                      plotOutput('avgPerDailyRate')
                  ),
                  box(width = 12,
                      title = "Level of Attrition by Overtime Status",
                      plotOutput('levAttrOvertimeStatus')
                  ),
                )
                
        ),
        tabItem(tabName = "environment",
                fluidRow(
                  box(width = 12,
                      title = "Number of Employees by Job Role",
                      plotOutput('numEmplJobRole')
                  ),
                  box(width = 12,
                      title = "Highest percentage of attrition by JobRole",
                      plotOutput('highPerAttrJobRole')
                  ),
                  box(width = 6,
                      title = "Attrition by Job Role",
                      plotOutput('attrJobRole')
                  ),
                  box(width = 6,
                      title = "Current Managers and Average Satisfaction Score",
                      plotOutput('currManaAvgSatScore')
                  ),
                  box(width = 12,
                      title = "Average Environment Satisfaction",
                      plotOutput('avgEnvSat')
                  )
                )
                
        ),
        tabItem(tabName = "depth",
                fluidRow(
                  box(width = 12,
                      title = "Work Life Balance Environment",
                      plotOutput('workLifeBalanceEnv')
                  ),
                  box(width = 6,
                      title = "Distance From Home",
                      plotOutput('distFromHome')
                  ),
                  box(width = 6,
                      title = "Distance From Work Status",
                      plotOutput('distFromWorkStatus')
                  ),
                  box(width = 12,
                      plotOutput('stockPointLev')
                  ),
                  box(width = 12,
                      plotOutput('attrBussTravel')
                  )
                )
                
        ),
        tabItem(tabName = "models",
                fluidRow(
                  box(width = 6,
                      plotOutput('corrEmplAttr')
                  ),
                  box(width = 6,
                      plotOutput('biVarAnalysis')
                  ),
                  box(width = 12,
                      plotOutput('classTree')
                  ),
                  box(width = 12,
                      plotOutput('featImportance')
                  )
                )
                
        )
    )
  )
)
)