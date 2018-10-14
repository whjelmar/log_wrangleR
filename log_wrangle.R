
# Function to Install and Load R Packages
Install_And_Load <- function(Required_Packages) {
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% 
                                              installed.packages()[, "Package"])]
  
  if (length(Remaining_Packages)) {
    install.packages(Remaining_Packages)
  }
  for (package_name in Required_Packages) {
    library(package_name, character.only = TRUE, 
            quietly = TRUE)
  }
}

# Specify the list of required packages to be
# installed and load
Required_Packages = c("tidyverse", "RCurl", "ggplot2", "DT", "devtools", "bit64", "rio", "dygraphs", "metricsgraphics", "githubinstall", "installR",
                      "jsonlite", "magrittr", "tm", "SnowballC", "wordcloud", "sqldf", "Hmisc", "shiny", "janitor", "beepR", "reticulate",
                      "RColorBrewer", "ggplot2", "gplots", "RColorBrewer", "readxl", "data.table", "plotly", "profvis", "remotes",
                      "dplyr", "knitr", "timelineS", "circlize", "fmsb", "lubridate", "officeR", "patchwork")

# Call the Function
Install_And_Load(Required_Packages)

devtools::install_github("hrbrmstr/hrbraddins")
devtools::install_github('cttobin/ggthemr')

cleanme <- function(dataname) {
  
  # SAVE THE ORIGINAL FILE
  oldfile <- write.csv(dataname, file = "oldfile.csv", 
                       row.names = FALSE, na = "")
  
  # CLEAN THE FILE. SAVE THE CLEAN. IMPORT THE CLEAN
  # FILE. CHANGE THE TO A DATAFRAME.
  cleandata <- dataname[complete.cases(dataname), 
                        ]
  cleanfile <- write.csv(cleandata, file = "cleanfile.csv", 
                         row.names = FALSE, na = "")
  cleanfileread <- read.csv(file = "cleanfile.csv")
  cleanfiledata <- as.data.frame(cleanfileread)
  
  # SUBSETTING THE DATA TO TYPES
  logicmeint <- cleanfiledata[, sapply(cleanfiledata, 
                                       is.integer)]
  logicmedouble <- cleanfiledata[, sapply(cleanfiledata, 
                                          is.double)]
  logicmefactor <- cleanfiledata[, sapply(cleanfiledata, 
                                          is.factor)]
  logicmenum <- cleanfiledata[, sapply(cleanfiledata, 
                                       is.numeric)]
  mainlogicmefactors <- cleanfiledata[, sapply(cleanfiledata, 
                                               is.factor) | sapply(cleanfiledata, is.numeric)]
  
  # VIEW ALL FILES
  View(cleanfiledata)
  View(logicmeint)
  View(logicmedouble)
  View(logicmefactor)
  View(logicmenum)
  View(mainlogicmefactors)
  
  # describeFast(mainlogicmefactors)
  
  # ANALYTICS OF THE MAIN DATAFRAME
  cleansum <- summary(cleanfiledata)
  print(cleansum)
  cleandec <- describe(cleanfiledata)
  print(cleandec)
  
  # ANALYTICS OF THE FACTOR DATAFRAME
  factorsum <- summary(logicmefactor)
  print(factorsum)
  factordec <- describe(logicmefactor)
  print(factordec)
  
  # ANALYTICS OF THE NUMBER DATAFRAME
  numbersum <- summary(logicmenum)
  print(numbersum)
  
  numberdec <- describe(logicmefactor)
  print(numberdec)
  
  mainlogicmefactorsdec <- describe(mainlogicmefactors)
  print(mainlogicmefactorsdec)
  
  mainlogicmefactorssum <- describe(mainlogicmefactors)
  print(mainlogicmefactorssum)
  
  # savemenow <- saveRDS('cleanmework.rds') readnow
  # <- readRDS(savemenow)
  
  # HISTOGRAM PLOTS OF ALL TYPES
  hist(cleanfiledata)
  hist(logicmeint)
  hist(logicmedouble)
  hist(logicmefactor)
  hist(logicmenum)
  # plot(mainlogicmefactors)
  
  save(cleanfiledata, logicmeint, mainlogicmefactors, 
       logicmedouble, logicmefactor, logicmenum, numberdec, 
       numbersum, factordec, factorsum, cleandec, 
       oldfile, cleandata, cleanfile, cleanfileread, 
       file = "cleanmework.RData")
}


outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var), eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  boxplot(var_name, main = "With outliers")
  hist(var_name, main = "With outliers", xlab = NA, 
       ylab = NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main = "Without outliers")
  hist(var_name, main = "Without outliers", xlab = NA, 
       ylab = NA)
  title("Outlier Check", outer = TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", 
          tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - 
                                             na1)/tot * 100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt = "Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if (response == "y" | response == "yes") {
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), 
           dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else {
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

sessionInfo()
