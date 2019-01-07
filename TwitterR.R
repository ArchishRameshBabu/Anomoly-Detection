library(AnomalyDetection)
library(ggplot2)
library(caret)
library(e1071)

AD_twitter_data <- function(file)
{
  # This function takes in a csv file with timestamp and value as input.
  # Identifies anomalies in it and display a plot with anomalies.
  
  tdata <- read.csv(file = file)

  tdata <- tdata[,c('timestamps', 'value')]
  tdata$timestamps <- as.POSIXct(tdata$timestamps, origin="1970-01-01")

  res = AnomalyDetectionTs(tdata[,1:2], max_anoms=0.02, direction='both', plot=TRUE)
  adata <- res$anoms
  adata$timestamp <- as.POSIXct(adata$timestamp, origin="1970-01-01", tz = "EST")

  ggplot(tdata, aes(x = timestamps, y = value)) +
  geom_line(col = "blue") +
  geom_point(data = adata, aes(x = timestamp, y = anoms), col = "red") +
  theme_bw()
  
  ggsave("Output/twitter_output.pdf")

}




AD_yahoo_data <- function(directory)
{
  # This function takes in a directory name with csv files as input.
  # returns a dataframe in which each row has the metric information for each file.
  
  files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  metric_df <- data.frame(File = character(), method = character(), TN = integer(), FP = integer(), FN = integer(), TP = integer(), Precision = integer(), Recall = integer(), F1 = integer())
  
  for (file in files)
  {

    tdata <- read.csv(file = file)
    tdata$label <- as.integer(row.names(tdata))
    mod <- lm(value ~ label, data = tdata)
    trend <- coef(summary(mod))['label', 'Estimate']
    tdata$value <- tdata$value - round(trend) * tdata$label
    
    tdata <- tdata[,c('timestamps', 'value', 'anomaly')]
    tdata$timestamps <- as.POSIXct(tdata$timestamps, origin="1970-01-01")
  
    res = AnomalyDetectionTs(tdata[,1:2], max_anoms=0.02, direction='both', plot=TRUE)
    adata <- res$anoms
    if( ncol(adata) == 0)
    {
      adata <- data.frame(timestamp = double(), anoms = integer())
    }

    adata$timestamp <- as.POSIXct(adata$timestamp, origin="1970-01-01", tz = "EST")
  
    ggplot(tdata, aes(x = timestamps, y = value)) +
      geom_line(col = "blue") +
      geom_point(data = adata, aes(x = timestamp, y = anoms), col = "red") +
      theme_bw()
  
    tdata$yahoo = 0
    tdata$yahoo[tdata$timestamps %in% adata$timestamp] = 1
    roc_obj <- roc(tdata$anomaly, tdata$yahoo)
    
    tdata$anomaly <- as.factor(tdata$anomaly)
    tdata$yahoo <- as.factor(tdata$yahoo)
    
    #print(summary(tdata))
    
    result <- confusionMatrix(tdata$anomaly , tdata$yahoo, mode="prec_recall")
    
    
    TN <- result$table[1, 1]
    FP <- result$table[1, 2]
    FN <- result$table[2, 1]
    TP <- result$table[2, 2]
    Precision <- result$byClass["Precision"]
    Recall <- result$byClass["Recall"]
    F1 <- result$byClass["F1"]
    
    #print(file)
    name = strsplit(file, "/")[[1]][2]

    
    metric_df <- rbind(metric_df, data.frame(File = name, Method = "twitter_anomaly_detector", TN = TN, FP = FP, FN = FN, TP = TP, Precision = Precision, Recall = Recall, F1 = F1))
    
    
    
  }
  
  #print(metric_df)
  
  rownames(metric_df) <- NULL
  #print(head(metric_df))
  write.csv(metric_df, "FromR.csv", row.names = FALSE)
  
}

AD_twitter_data("toR.csv")
AD_yahoo_data("A1Benchmark")

