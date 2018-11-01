# Test Variables
tv <- ("1999-01-01, 2000-01-01,78,M,FALSE,FALSE,94611544-6c5c-40eb-ae87-83c12b6208d3, areaData$Julian, areaData$DayAirTmpAvg.Value, JulianDay, DailyTmp, 47, 29.44, 18.33, 12.778, -6.1, 47.2")

#################################################################################
# The following function called "CIMIS_download_analyze_plot" (CIMIS_DAP)       #
#   imports desired weather data from a selected data.frame then creates a      #
#   1 year Julian day plot with DailyAvgAirTmp, DailyHighAirTmp,                #
#   and DailyLowAirTmp in an effort to findcthe best crop growing seasons with  #
#   California Irrigation Management Information System Data.                   #
#                                                                               #
#   Created by: Bryce Stevenson October 2018                                    #
#                                                                               #
# Input:                                                                        #
#     startDate = (YYYY-MM-DD), endDate = (YYYY-MM-DD), targets = (# or #,#),   #
#     unitofMeasure = (I|M), prioritizeSCS = (TRUE|FALSE),                      #
#     includeQC = (TRUE|FALSE), appKey = (""), x_data = (df$column),            #
#     y_data = = (df$column), extremeCropHigh = (#), dayLimitHigh = (#),        #
#     dayLimitLow = (#), nightLimitLow = (#), cropType, recordLow, recordHigh"  #
#################################################################################
CIMI_appKey <- function(appKey)
{
  appKey <- readLine("What is your CIMIS API key?")
  appKey <- as.numeric(appKey)
  print(appkey)
}
CIMIS_DAP <- function(startDate, endDate, targets, unitOfMeasure, prioritizeSCS, includeQC, appKey, x_data, y_data, x_data_title, y_data_title, extremeCropHigh, dayLimitHigh, dayLimitLow, nightLimitLow, cropType, recordLow, recordHigh)
{
  # Load required packages
  library(Rcimis)
  library(ggplot2)
  
  print(startDate, endDate, targets, unitOfMeasure, prioritizeSCS, includeQC, appKey, x_data, y_data, x_data_title, y_data_title, extremeCropHigh, dayLimitHigh, dayLimitLow, nightLimitLow, cropType, recordLow, recordHigh)
  
  
  # Clears Variables
  areaDaily <- NULL
  areaData <- NULL
  
  # User Sets Variables
  areaData <- data.frame()
  areaTitle <- NULL
  x_data <- NULL
  y_data <- NULL
  apiCIMIS <- c("94611544-6c5c-40eb-ae87-83c12b6208d3")
  
  #unitOfMeasure = c("M")
  
  if(is.null(unitOfMeasure))
    unitOfMeasure <- c("M")
  
  # Static Crop Variables
  # Temperature Variables
  # tomatoes <- c("-Inf, 18.33, 29.44, Inf")
  extremeCropHigh <- as.numeric(35)
  dayLimitHigh <- as.numeric(29.44)
  dayLimitLow <- as.numeric(18.33)
  nightLimitLow <- as.numeric(12.778)
  cropType <- c("-Inf, dayLimitLow, dayLimitHigh, Inf")
  areaRecordTemps <- c("-6.1, 47.2")
  
  # Import Data from CIMIS
  areaData <- Rcimis::CIMISweather(start, end, targets, unitOfMeasure, prioritizeSCS, includeQC, apiCIMIS)
  print(areaData)
  
  # Styling Code
  colors <- c("blue", "green", "red")
  
  # Plot Code
  areaDaily <- ggplot(areaData,aes(x=factor(x_data), y=y_data)) +
    geom_point(aes(color = cut(y_data, cropType))) +
    scale_color_manual(name = "Temperature", values = colors, labels = c("Low", "Idea", "High")) +
    scale_y_continuous(limits=areaRecordTemps) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  facet_wrap(~group, ncol = 1, scales = "free_x") +
    geom_hline(yintercept=extremeCropHigh, color = "red") +
    geom_hline(yintercept=dayLimitHigh, color = "green") +
    geom_hline(yintercept=dayLimitLow, color = "green") +
    geom_hline(yintercept=nightLimitLow, color = "blue") +
    ggtitle(spadraTitle) +
    xlab(x_data_title) +
    ylab (y_data_title+"(ºC)")
  
  areaData$group <- as.numeric(cut(x_data, 4))
  
  # Display Plot
  print(areaDaily)}
