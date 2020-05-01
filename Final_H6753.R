setwd("~/Desktop/H6753/Final_H6753")

df <- read.csv("online_shoppers_intention.csv")
df$is_revenue <- as.integer(df$Revenue)
df$is_weekend <- as.integer(df$Weekend)

head(df)
dim(df)
install.packages("corrplot")
library(corrplot)

cor_all <- cor(df[,1:10,12:18])
cor_all

corrplot(cor_all, method = 'number', number.cex = 0.8, diag = FALSE, tl.cex = 0.8)
corrplot(cor_all, add = TRUE, type = 'upper', diag = FALSE, tl.pos = 'n', cl.pos = 'n')


install.packages("ggplot2")
library(ggplot2)

ggplot(data = df, aes(x = ProductRelated, y = PageValues)) +
  geom_point()  

# Give the chart file a name.
png(file = "scatterplot_matrices.png")

# Plot the matrices between 4 variables giving 12 plots.

# One variable with 3 others and total 4 variables.

pairs(~Administrative+Administrative_Duration+Informational+Informational_Duration+ProductRelated+ProductRelated_Duration+BounceRates+ExitRates+PageValues+SpecialDay+Month+OperatingSystems+Browser+Region+TrafficType+VisitorType+Weekend+Revenue, data = df,
      main = "Scatterplot Matrix")

# Save the file.
dev.off()



#correlation between informational & revenue 
cor_matr1 <- cor.test(df$Informational,df$is_revenue)
cor_matr1

#correlation between informational & revenue 
cor_matr2 <- cor.test(df$ProductRelated,df$is_revenue)
cor_matr2

#logistic回归, 广义线性
glm <- glm(df$is_revenue ~ df$Informational, family = binomial(link = logit), data = df)
summary(glm)
# 逐步寻优法  forward & backward后向选择法
logit.step <- step(glm, direction = "both")
summary(logit.step)

glm <- glm(df$is_revenue ~ df$ProductRelated, family = binomial(link = logit), data = df)
summary(glm)
# 逐步寻优法  forward & backward后向选择法
logit.step <- step(glm, direction = "both")
summary(logit.step)




#线性不咋好使，故使用逻辑多元回
lm3 <- lm(df$is_revenue ~ df$Administrative_Duration + df$Informational_Duration + df$ProductRelated_Duration + df$BounceRates + df$ExitRates+ df$PageValues)
lm3

glm2 <- glm(df$is_revenue ~ df$Administrative_Duration + df$Informational_Duration + df$ProductRelated_Duration + df$BounceRates + df$ExitRates + df$PageValues, family = binomial(link = logit), data = df)
summary(glm2)
# 逐步寻优法  forward & backward后向选择法
logit.step <- step(glm, direction = "both")
summary(logit.step)  

model.lm <- lm(df$is_revenue ~., data =df)
summary(model.lm)


#可视化

# visitor type, weekend, special day, month - Revenue
counts1<-table(df$Revenue,df$VisitorType)
counts1


barplot(counts1, main = "TypeOfvisitor_revenue", xlab = "revenue or not",ylab = "Frequency",
 legend=rownames(counts1))

counts2 <- table(df$Revenue,df$Month)
counts2

barplot(counts2, main = "month_revenue", xlab = "Month",ylab = "Frequency")
#,
#  legend=rownames(counts2))

counts3 <-table(df$Revenue,df$Browser)
counts3

barplot(counts3, main = "Broswer_revenue", xlab = "Broser No.",ylab = "Frequency",
        legend=rownames(counts3))

counts4<-table(df$Revenue, df$Weekend)
counts4

barplot(counts4, main = "weekend_revenue", xlab = "revenue or not",ylab = "Frequency",
 legend=rownames(counts4))
require(graphics)
pairs(df[,c(17,18)])
scatter.hist(counts4)

#region - revenue

counts5<-table(df$Revenue, df$Region)
counts5
barplot(counts5, main = "region", xlab = "Region No.",ylab = "Frequency",
        legend=rownames(counts5))

install.packages("ggplot2")
library(ggplot2)


# 基函数：x设置目标变量，fill设置填充色
ggplot(df, aes(x = df$Region, fill = Region)) +
  # 密度曲线函数：alpha设置填充色透明度
  geom_density(alpha = 0.3)


