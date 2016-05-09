require(xlsx)

data <- read.xlsx("data_and_headers_processed.xlsx", 1, stringsAsFactors=T)
data$Age <- as.numeric(data$Age)


print(summary(data$System))
print(summary(data$Sex))





data_filtered <- data[data$System == 'C' & data$Age  >= 18 & data$Age<=49,]
#print(data$Comp_Use_Type[1])


# 2.2
require(moments)                    # load e1071
stat_data <- data_filtered[ ,names(data_filtered) %in% c("R2", "R3")]
stat_res <- data.frame(
    apply(stat_data, 2, mean, na.rm=TRUE), 
    apply(stat_data, 2, sd, na.rm=TRUE), 
    apply(stat_data, 2, min, na.rm=TRUE), 
    apply(stat_data, 2, max, na.rm=TRUE), 
    apply(stat_data, 2, kurtosis, na.rm=TRUE),
    apply(stat_data, 2, skewness, na.rm=TRUE)
)
colnames(stat_res) <- c('mean', 'sd', 'min', 'max', 'kurtosis', 'skewness')


# 2.4

lmodel = lm(S6 ~ Age+Sex, data = data_filtered)
summary(lmodel)
