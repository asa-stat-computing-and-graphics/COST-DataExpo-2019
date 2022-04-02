library(readr)
library(glmnet)
library(corrplot)
library(ggplot2)
library(FactoMineR)

load('R1.RData')
#data <- read.csv("intermediate_data/data2.csv")
data$`Condition.of.Stairways..Exterior.and.Interior...No.exterior.steps.or.stairways` <- NULL
rent=data$Rent
df=data[,13:64]
df=as.matrix(df)
colnames(df) <- 1:52

### Table 1
## PCA
pca_result <- prcomp(df, scale = TRUE)
summary(pca_result)

## MCA
dffac = factor(df[,1], ordered = TRUE)
for (i in 1:52){
  tfac = factor(df[,i], ordered = TRUE)
  dffac = data.frame(dffac, tfac)
}
colnames(dffac)=labels(df[1,])
dffac[,53] <- NULL
mca <- MCA(dffac, ncp = 1)
summary(mca)


#Figure 1 in paper
corrplot.mixed(cor(df), lower = "square", upper = "circle", tl.col = "black",tl.cex=0.6)

temp_data=data.frame((df!=0)%>%apply(2,mean))
colnames(temp_data)='frequency'
#Figure 2 in paper
ggplot(temp_data,aes(x=frequency))+geom_histogram(bins = 21)

## Ridge Regression
fit1 <- cv.glmnet(as.matrix(df), rent, family = 'gaussian', nfold = 10, alpha = 0)
#plot(fit1)
ridge <- glmnet(as.matrix(df),rent,family='gaussian',alpha=0,lambda = exp(10))
ridge_weight <- ridge$beta
data$index_ridge <- as.matrix(data[,13:64]) %*% as.matrix(ridge_weight)
data$index <- (data$index_ridge+250)/500
data$condition_rating <- ceiling(data$index*20)-5
data$condition_rating[data$condition_rating<0]=0
data$condition_rating[data$condition_rating>10]=10
data$index_ridge <- NULL
data[,13:64] <- NULL


#write.csv(data,file="intermediate_data/data3.csv",row.names = F)


