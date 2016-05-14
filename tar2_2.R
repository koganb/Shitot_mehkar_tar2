require(xlsx)

data <- read.xlsx("data_and_headers_processed.xlsx", 1, stringsAsFactors=T)
data$Age <- as.numeric(as.character(data$Age))


data.for.clarity <- cbind(data[,c("C1", "C2", "C3", "C5")], 8-data$C4, 8-data$C6)
clarity <- apply(data.for.clarity, MARGIN = 1, FUN = mean)

data.for.politeness <- cbind(data[,c("P1", "P2", "P4", "P5", "P6")], 8-data$P3)
politeness <- apply(data.for.politeness, MARGIN = 1, FUN = mean)

data.for.satisfaction <- cbind(data[,c("S1", "S2", "S3", "S5", "S6")], 8-data$S4)
satisfaction <- apply(data.for.satisfaction, MARGIN = 1, FUN = mean)

#now adding them to the data frame
data <- cbind(data, clarity = clarity, politeness = politeness, satisfaction = satisfaction)




#2.1



drawPieChart <- function (slices, lbls) {
  pct <- round(slices/sum(slices)*100)  #http://www.statmethods.net/graphs/pie.html
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie3D(slices, labels = lbls, explode=0.1,main="",labelcex =0.8)
  return()
}

drawHist <- function (histData, br) {
  hist(histData, col="red", breaks = br, main = "")
  return()
}


summaryDataFrame<- function(dataCol, dataColName) {
  dataColSummary <- summary(dataCol)
  resframe <- data.frame(matrix(unlist(dataColSummary), nrow=1, byrow=T))
  colnames(resframe) <- names(res)
  rownames(resframe) <- dataColName
  return(resframe)
}


par(mfrow=c(1,2)) 


#data$Age
rbind(summaryDataFrame(data[data$System=='C',]$Age, "data[data$System=='C',]$Age"),
      summaryDataFrame(data[data$System=='S',]$Age, "data[data$System=='S',]$Age"))


#print(summary(data[data$System=='C',]$Age))
#print(summary(data[data$System=='S',]$Age))
drawHist(data[data$System=='C',]$Age, br=20)
drawHist(data[data$System=='S',]$Age, br=20)


#data$Sex
print(summary(data[data$System=='C',]$Sex))
print(summary(data[data$System=='S',]$Sex))
drawPieChart(table(data[data$System=='C',]$Sex), c("men", "women"))
drawPieChart(table(data[data$System=='S',]$Sex), c("men", "women"))


#data$Comp_Use_Freq
print(summary(data[data$System=='C',]$Comp_Use_Freq))
print(summary(data[data$System=='S',]$Comp_Use_Freq))
drawPieChart(table(data[data$System=='C',]$Comp_Use_Freq), c("2-5 hours", "5 hours", "less than one time", "less than 2 hours"))
drawPieChart(table(data[data$System=='S',]$Comp_Use_Freq), c("2-5 hours", "5 hours", "less than one time", "less than 2 hours"))

#data$Comp_Use_Know
print(summary(data[data$System=='C',]$Comp_Use_Know))
print(summary(data[data$System=='S',]$Comp_Use_Know))
drawPieChart(table(data[data$System=='C',]$Comp_Use_Know), c("beginner", "intermediate", "advanced",  "expert"))
drawPieChart(table(data[data$System=='S',]$Comp_Use_Know), c("beginner", "intermediate", "advanced",  "expert"))



#data$Selected_Software
print(summary(data[data$System=='C',]$Selected_Software))
print(summary(data[data$System=='S',]$Selected_Software))
barplot(table(data[data$System=='C',]$Selected_Software), las=2, col = 'red')
barplot(table(data[data$System=='S',]$Selected_Software), las=2, col = 'red')



###########################################################################


data_filtered <- data[data$System == 'C' & data$Age  >= 18 & data$Age<=49,]
#print(data$Comp_Use_Type[1])


# 2.2
require(moments)                    # load e1071
stat_data <- data_filtered[ ,names(data_filtered) %in% c("clarity", "politeness", "satisfaction")]
stat_res <- data.frame(
  apply(stat_data, 2, length), 
  apply(stat_data, 2, mean, na.rm=TRUE), 
  apply(stat_data, 2, sd, na.rm=TRUE), 
  apply(stat_data, 2, min, na.rm=TRUE), 
  apply(stat_data, 2, max, na.rm=TRUE), 
  apply(stat_data, 2, kurtosis, na.rm=TRUE),
  apply(stat_data, 2, skewness, na.rm=TRUE)
)
colnames(stat_res) <- c('count', 'mean', 'sd', 'min', 'max', 'kurtosis', 'skewness')
stat_res

# 2.4

lmodel1 = lm(satisfaction ~ Age+Sex, data = data_filtered)
summary(lmodel1)

lmodel2 = lm(satisfaction ~ Age+Sex+clarity+politeness, data = data_filtered)
summary(lmodel2)

anova(lmodel1, lmodel2)
