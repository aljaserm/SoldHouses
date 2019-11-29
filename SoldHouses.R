soldHouses <- read.csv("C:/Users/aljas/OneDrive/Documents/Development/R/SoldHouses/SoldHouses/House-Price.csv", header = TRUE)
boxplot(soldHouses$n_hot_rooms)
pairs(~soldHouses$Sold+soldHouses$rainfall)
barplot(table(soldHouses$airport))
barplot(table(soldHouses$bus_ter))

#Observations
# n_hot_rooms and rainfall has outliers
# n_hos_beds has missing values
# bus_ter is useless

# to clear hotel room outliers
n_hot_rooms_upperValue<-3*quantile(soldHouses$n_hot_rooms, 0.99)
soldHouses$n_hot_rooms[soldHouses$n_hot_rooms>n_hot_rooms_upperValue] <- n_hot_rooms_upperValue
summary(soldHouses$n_hot_rooms)


# to clear rainfall outliers
rainfall_lowerValue= 0.3 *quantile(soldHouses$rainfall, 0.01)
soldHouses$rainfall[soldHouses$rainfall<rainfall_lowerValue] <- rainfall_lowerValue
summary(soldHouses$rainfall)


# handle missing info
mean(soldHouses$n_hos_beds)
mean(soldHouses$n_hos_beds,na.rm = TRUE)
which(is.na(soldHouses$n_hos_beds))
soldHouses$n_hos_beds[is.na(soldHouses$n_hos_beds)] <- mean(soldHouses$n_hos_beds,na.rm = TRUE)
summary(soldHouses$n_hos_beds)

# remove unwated data
soldHouses$avg_Dist = (soldHouses$dist1+soldHouses$dist2 +soldHouses$dist3+ soldHouses$dist4)/4
soldHousesNew <- soldHouses[,-6:-9]
soldHouses <- soldHousesNew
rm(soldHousesNew)

# Remove bus terminal
soldHouses <- soldHouses[,-13]


# replace to non-numeric values
#install.packages("dummies")
soldHouses <- dummy.data.frame(soldHouses)
soldHouses <- soldHouses[,-8]
soldHouses <- soldHouses[,-13]

# Logistc regression with single predictor

glm.fit= glm(Sold~price, data= soldHouses, family = binomial)
summary(glm.fit)

# Logistc regression with multi predictor
glm.fitMulty= glm(Sold~., data= soldHouses, family = binomial)
summary(glm.fitMulty)

glm.probs = predict(glm.fitMulty, type = "response")
glm.probs[1:10]


glm.pred = rep("No", 506)
glm.pred[glm.probs > 0.5] = "Yes"

table(glm.pred, soldHouses$Sold)

# Linear/Quad Discriminant Analysis
# install.packages("MASS")
lda.fit = lda(Sold ~ . , data=soldHouses)
lda.fit
lda.pred = predict(lda.fit, soldHouses)
lda.pred$posterior
lda.class = lda.pred$class
table(lda.class, soldHouses$Sold)

sum(lda.pred$posterior[,1]>0.8)


qda.fit = qda(Sold ~ . , data=soldHouses)
qda.fit
qda.pred = predict(qda.fit, soldHouses)
qda.pred$posterior
qda.class = qda.pred$class
table(qda.class, soldHouses$Sold)

sum(qda.pred$posterior[,1]>0.8)

# test/train split
set.seed(0)
split = sample.split(soldHouses, SplitRatio = 0.8)
trainingSet = subset(soldHouses, split == 'TRUE')
testSet= subset(soldHouses, split == 'FALSE')

train.fit = glm(Sold~., data= trainingSet, family = binomial)
test.probs = predict(train.fit, testSet, type = 'response')
test.pred = rep('NO', 120)
test.pred[test.probs > 0.5] = 'YES'

table(test.pred, testSet$Sold)

ttlda.fit = lda(Sold ~ . , data=trainingSet)
ttlda.fit
ttlda.pred = predict(ttlda.fit, testSet)
ttlda.pred$posterior
ttlda.class = ttlda.pred$class
table(ttlda.class, testSet$Sold)

sum(ttlda.pred$posterior[,1]>0.8)


ttqda.fit = qda(Sold ~ . , data=trainingSet)
ttqda.fit
ttqda.pred = predict(ttqda.fit, testSet)
ttqda.pred$posterior
ttqda.class = ttqda.pred$class
table(ttqda.class, testSet$Sold)

sum(ttqda.pred$posterior[,1]>0.8)
# KNN 
# install.packages("class")
trainX = trainingSet[,-16]
testX = testSet[,-16]
trainY = trainingSet$Sold
testY = testSet$Sold
k = 4
trainX_s= scale(trainX)
testX_s = scale(testX)
set.seed(0)
knn.pred = knn(trainX_s, testX_s, trainY, k=k)
table(knn.pred, testY)

