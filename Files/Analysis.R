library(plyr)
library(caret)

df <- read.csv(file = 'C:/_Projects/C744 Data Minig and Analytics II/CLEANED_WA_Fn-UseC_-Telco-Customer-Churn .csv')

str(df)

#Next we will begin cleaning our data by recoding values for better grouping...

df$MultipleLines <- as.factor(
    mapvalues(df$MultipleLines,
    from=c("No phone service"),
    to=c("No")
  )
)

df$OnlineSecurity <- as.factor(
    mapvalues(df$OnlineSecurity,
    from=c("No internet service"),
    to=c("No")
  )
)

df$OnlineBackup <- as.factor(
    mapvalues(df$OnlineBackup,
    from=c("No internet service"),
    to=c("No")
  )
)

df$DeviceProtection<- as.factor(
    mapvalues(df$DeviceProtection,
    from=c("No internet service"),
    to=c("No")
  )
)

df$TechSupport <- as.factor(
    mapvalues(df$TechSupport,
    from=c("No internet service"),
    to=c("No")
  )
)

df$StreamingTV <- as.factor(
    mapvalues(df$StreamingTV,
    from=c("No internet service"),
    to=c("No")
  )
)

df$StreamingMovies <- as.factor(
    mapvalues(df$StreamingMovies,
    from=c("No internet service"),
    to=c("No")
  )
)

df$SeniorCitizen <- as.factor(
    mapvalues(df$SeniorCitizen,
    from=c("0", "1"),
    to=c("No", "Yes")
  )
)


summary(df$MonthlyCharges)

summary(df$TotalCharges)

summary(df$tenure)

df$MonthlyCharges <- as.factor(car::recode
	(
		df$MonthlyCharges, "
		1:35 = 'Charge-low';
		35:70 = 'Charge-mid';
		else = 'Charge-high' "
	)
)

df$tenure <- as.factor(car::recode
	(
		df$tenure, "
		1:9 = 'Tenure-low';
		9:29 = 'Tenure-mid';
		else = 'Tenure-high' "
	)
)

df$TotalCharges <- as.factor(car::recode
	(
		df$TotalCharges, "
		1:401 = 'TCharge-low';
		401:1398 = 'TCharge-mid';
		else = 'TCharge-high' "
	)
)

library(FactoMineR)

res.mca <- MCA(df, quali.sup=c(1:6,20), graph = FALSE)

#Eigenvalues / Variances
library("factoextra")
eig.val <- get_eigenvalue(res.mca)
 # head(eig.val)
 
#visualize percentages of interita explained by each MCA dimenions 
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))




fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal()
			 
			 



fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "Churn", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 
			 



fviz_ellipses(res.mca, c(1:6, 20),
              geom = "point")
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
#Predictive modeling Decision tree (classification tree to be more precise)

library(caret)
library(tree)

set.seed(1)

#Train the data by splitting into test set

prep<- createDataPartition(df$Churn,p=0.7,list=FALSE)

trained <- df[prep,]
test <- df[-prep,]

dim(trained); dim(test)

tree <- hclustvar(X.quali = trained[,1:19])