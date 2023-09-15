library(dplyr)
library(caTools)
library(DAAG)
vg_data <- read.csv("C:\\Users\\HP\\Documents\\DS project\\It is WOT it ISZ\\Video_Games_Sale.csv")
print(head(vg_data))
vg_data=na.omit(vg_data) 
print(head(vg_data))
summary(vg_data)
str(vg_data)
sample1 <- select_if(vg_data,is.numeric)
print(head(sample1))

TheData <-na.omit(sample1) 
print(TheData)

vgfilter = filter(vg_data, Year!="N/A" & Global_Sales!="N/A" & EU_Sales!="N/A" & JP_Sales!="N/A" & NA_Sales!="N/A" , Genre!="N/A")
Data1 = select(vgfilter, Year, EU_Sales, JP_Sales,NA_Sales, Global_Sales)
print(head(Data1))

filter_data1 <- aggregate(x= Data1$NA_Sales,
                          by= list(Data1$Year),
                          FUN=sum)

filter_data2 <- aggregate(x= Data1$EU_Sales,
                          by= list(Data1$Year),
                          FUN=sum)

filter_data3 <- aggregate(x= Data1$JP_Sales,
                          by= list(Data1$Year),
                          FUN=sum)

filter_data4 <- aggregate(x= Data1$Global_Sales,
                          by= list(Data1$Year),
                          FUN=sum)
print(head(filter_data4))
par(mfrow=c(2,2))
plot(filter_data1, main="North America Sales", xlab="Years", ylab="Sales (in Millions)")
plot(filter_data2, main="Europe Sales", xlab="Years", ylab="Sales (in Millions)")
plot(filter_data3, main="Japan Sales", xlab="Years", ylab="Sales (in Millions)")
plot(filter_data4, main="Global Sales", xlab="Years", ylab="Sales (in Millions)")
par(mfrow=c(1,1))

print(head(vgfilter))
#Filtering Platform
vgfilterPlat=filter(vgfilter, Platform =="PS" | Platform=="PS2" | Platform=="PS3" | Platform=="PS4" | Platform=="XOne" | Platform=="GBA" | Platform=="XB" | Platform=="X360" )
print(head(vgfilterPlat))
#Filtering Genre
vgfilterGenre=filter(vgfilter, Genre =="Action" | Genre=="Racing" | Genre=="Shooter" | Genre=="Sports" | Genre=="Fighting" )
print(head(vgfilterPlat))
barplot(pvsg$Global_Sales,names.arg = pvsg$Platform, col="pink",xlab="Platform", ylab="Global Sales (in millions)", main=("Sales per Platforms"))


# Genre vs Score/Count
pvsg <- aggregate(Global_Sales ~ Platform, data = vgfilterGenre, sum)
csvsg=aggregate(Critic_Score ~ Genre, data = vgfilterGenre, sum)
ccvsg=aggregate(Critic_Count ~ Genre, data = vgfilterGenre, sum)
usvsg=aggregate(User_Score ~ Genre, data = vgfilterGenre, sum)
ucvsg=aggregate(User_Count ~ Genre, data = vgfilterGenre, sum)
par(mfrow=c(2,2))
barplot(csvsg$Critic_Score,col="orange",names.arg=csvsg$Genre,xlab="Genre",ylab="Critic_Score")
barplot(ccvsg$Critic_Count,col="red",names.arg=ccvsg$Genre,xlab="Genre",ylab="Critic_Count")
barplot(usvsg$User_Score,col="yellow",names.arg=usvsg$Genre,xlab="Genre",ylab="User_Score")
barplot(ucvsg$User_Count,col="green",names.arg=ucvsg$Genre,xlab="Genre",ylab="User_Count")
par(mfrow=c(1,1))
# Platform vs Score/Count
csvsP=aggregate(Critic_Score ~ Platform, data = vgfilterPlat, sum)
ccvsP=aggregate(Critic_Count ~ Platform, data = vgfilterPlat, sum)
usvsP=aggregate(User_Score ~ Platform, data = vgfilterPlat, sum)
ucvsP=aggregate(User_Count ~ Platform, data = vgfilterPlat, sum)

par(mfrow=c(2,2))
barplot(csvsP$Critic_Score,col="orange",names.arg=csvsP$Platform,xlab="Platform",ylab="Critic_Score")
barplot(ccvsP$Critic_Count,col="red",names.arg=ccvsP$Platform,xlab="Platform",ylab="Critic_Count")
barplot(usvsP$User_Score,col="yellow",names.arg=usvsP$Platform,xlab="Platform",ylab="User_Score")
barplot(ucvsP$User_Count,col="green",names.arg=ucvsP$Platform,xlab="Platform",ylab="User_Count")
par(mfrow=c(1,1))
vgfilter1 = filter(vg_data, Platform=="PS")
vgfilter2 = filter(vg_data, Platform=="PS2")
vgfilter3 = filter(vg_data, Platform=="PS3")
vgfilter4 = filter(vg_data, Platform=="X360")


top_five_ps <- vgfilter1 %>%
  filter(rank(desc(Global_Sales))<=5)

top_five_ps2 <- vgfilter2 %>%
  filter(rank(desc(Global_Sales))<=5)

top_five_ps3 <- vgfilter3 %>%
  filter(rank(desc(Global_Sales))<=5)

top_five_x360 <- vgfilter4 %>%
  filter(rank(desc(Global_Sales))<=5)

print("Top 5 Best Selling Games published on PS")
print(select(top_five_ps,Name,Publisher,Global_Sales))

print("Top 5 Best Selling Games published on PS2")
print(select(top_five_ps2,Name,Publisher,Global_Sales))

print("Top 5 Best Selling Games published on PS3")
print(select(top_five_ps3,Name,Publisher,Global_Sales))

print("Top 5 Best Selling Games published on X360")
print(select(top_five_x360,Name,Publisher,Global_Sales))

filter_genre <- aggregate(x= vgfilter$Global_Sales,
                          by= list(vgfilter$Genre),
                          FUN=sum)
colors = c("red", "yellow", "green", "violet", 
           "orange", "blue", "pink", "cyan","brown","grey","navy blue","maroon") 
par(mfrow=c(1,1))
pie(filter_genre$x, filter_genre$Group.1,col=colors,main = "Pie Chart of Sales According to Genre")

#MR
# co-relation
round(cor(TheData, method="pearson"),2)

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
split = sample.split(TheData$Global_Sales, SplitRatio = 0.8)
training_set = subset(TheData, split == TRUE)
test_set = subset(TheData, split == FALSE)

ml_reg <- lm(Global_Sales ~ . -Other_Sales ,data=TheData)

summary(ml_reg)
print(ml_reg)

pred <- predict(ml_reg, test_set)

#Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=test_set$Global_Sales, predicteds=pred))  # make actuals_predicteds dataframe.

correlation_accuracy <- cor(actuals_preds)  

head(actuals_preds)
tail(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

print(min_max_accuracy)

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

print(mape)


plot(predict(ml_reg),
     TheData$Global_Sales,
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Observe vs Predicted Values")

abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)

