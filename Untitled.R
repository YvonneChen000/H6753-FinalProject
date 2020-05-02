#Author：TuXin
library(ggplot2)
library(gcookbook)
library(plyr)
library(MASS)
library(tidyverse)
library(hrbrthemes)
library(cowplot)
library(dplyr)
library(reshape2)

df <- read.csv("online_shoppers_intention.csv")


## Histograms of the number of Administrative pages visited grouped by purchase behavior 
# group by "Revenue"
mu1 <- ddply(df, "Revenue", summarise, grp.mean=mean(Administrative,na.rm = TRUE))
#Change line colors by groups
ggplot(df, aes(x=Administrative, color=Revenue, fill=Revenue)) +
  #geom_bar(position="identity", alpha=0.5,stat="bin")+
  geom_histogram(position="identity", alpha=0.5,bins = 30)+
  geom_vline(data=mu1, aes(xintercept=grp.mean, color=Revenue),linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(x="Number of Administrative pages visited", y = "Count")

## Histograms of the number of ProductRelated pages visited grouped by purchase behavior 
mu2 <- ddply(df, "Revenue", summarise, grp.mean=mean(ProductRelated,na.rm = TRUE))# group by "Revenue"
#Change line colors by groups
ggplot(df, aes(x=ProductRelated, color=Revenue, fill=Revenue)) +
  geom_histogram(position="identity", alpha=0.5,bins = 30)+
  geom_vline(data=mu2, aes(xintercept=grp.mean, color=Revenue),linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(x="Number of ProductRelated pages visited", y = "Count")

## Histograms of the number of Informational pages visited grouped by purchase behavior 
mu3 <- ddply(df, "Revenue", summarise, grp.mean=mean(Informational,na.rm = TRUE))# group by "Revenue"
#Change line colors by groups
ggplot(df, aes(x=Informational, color=Revenue, fill=Revenue)) +
  geom_histogram(position="identity", alpha=0.5,bins = 30,)+
  geom_vline(data=mu3, aes(xintercept=grp.mean, color=Revenue),linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(x="Number of Informational pages visited", y = "Count")



# analyze duration variable
Duration <- df[,c('Administrative_Duration','ProductRelated_Duration','Informational_Duration','Revenue')]
dim(Duration)
head(Duration)
# 12330 row     4 column
#delete extreme value
Duration1<-filter(Duration, Administrative_Duration <=1000 & Administrative_Duration >=0)
dim(Duration1)
#12197row     4 column
Duration2<-filter(Duration, Informational_Duration <=2000 & Informational_Duration >=0)
dim(Duration2)
#12277 row    4 column
Duration3<-filter(Duration, ProductRelated_Duration <=2000 & ProductRelated_Duration >=0)
dim(Duration3)
# 10154 row     4 column
#based on these test results above, we conduct overall filter for 3 duration variables
DurationValid<-filter(Duration, Administrative_Duration <=2000 & Administrative_Duration >=0 &
                    Informational_Duration <=2000 & Informational_Duration >=0 &
                    ProductRelated_Duration <=2000 & ProductRelated_Duration >=0)
dim(DurationValid)
# 10150     4

colnames(DurationValid) <- c('Administrative','ProductRelated','Informational','Revenue')
head(DurationValid)


# reshape Duration datafram 
DurationValidMelted <- melt(DurationValid,id.vars=c('Revenue'))
dim(DurationValidMelted)

# graw boxplot of duration in webpages grouped by purchase behavior
ggplot(data = DurationValidMelted, aes(x =Revenue,y=value,color=Revenue))+
  geom_boxplot()+#geom_jitter()+
  facet_grid(.~ variable)+
  ylab("Duration time")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# the Informational Duration is too small
ggplot(data = DurationValid, aes(x =Revenue,y=Informational,color=Revenue))+
  geom_boxplot()+ #geom_jitter()+
  ylab("Informational Duration")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# delete more exterme data for Informational Duration
DurationValid2<-filter(DurationValid, Informational <=100 & Informational >=0)
dim(DurationValid2)
#10150     4 when Informational <=2000 & Informational >=0
#9610    4 when Informational <=100 & Informational >=0

# plot a  histogram of Informational duration grouped by purchase behavior
mu4 <- ddply(DurationValid2, "Revenue", summarise, grp.mean=mean(Informational,na.rm = TRUE))
#Change line colors by groups
ggplot(DurationValid2, aes(x=Informational, color=Revenue, fill=Revenue)) +
  geom_histogram(position="identity", alpha=0.5,bins = 30,)+
  geom_vline(data=mu4, aes(xintercept=grp.mean, color=Revenue),linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(x="Informational Duration", y = "Count")

#  draw a boxplot of Bounce Rates and Exit Rates grouped by purchase behavior
Rates <- df[,c('BounceRates','ExitRates','Revenue')]
dim(Rates)
head(Rates)
# reshape Rate datafram 
RatesMelted <- melt(Rates,id.vars=c('Revenue'))
dim(RatesMelted)
head(RatesMelted)

ggplot(data = RatesMelted, aes(x =Revenue,y=value,color=Revenue))+
  geom_boxplot()+#geom_jitter()+
  facet_grid(.~ variable)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# draw a boxplot of Page Values grouped by purchase behavior
df$PageValues

ggplot(data = df, aes(x =Revenue,y=PageValues,color=Revenue))+
  geom_boxplot()+ #geom_jitter()+
  ylab("PageValues")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# delete more exterme data for PageValues
dim(df)#12330    18
df2<-filter(df, PageValues <=100)
dim(df2)# 12248    18

# draw a histogram of Page Values grouped by purchase behavior
mu5 <- ddply(df2, "Revenue", summarise, grp.mean=mean(PageValues,na.rm = TRUE))
#Change line colors by groups
ggplot(df2, aes(x=PageValues, color=Revenue, fill=Revenue)) +
  geom_histogram(position="identity", alpha=0.5,bins = 30,)+
  geom_vline(data=mu5, aes(xintercept=grp.mean, color=Revenue),linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(x="PageValues", y = "Count")
