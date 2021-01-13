#bivariate analysis
library(gmodels)

df <- read.csv(file = 'C:/_Projects/C744 Data Minig and Analytics II/CleanedChurnData.csv')


#Same line of code for each variable against churn.
CrossTable(df$TechSupport, df$Churn)