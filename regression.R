#### Regression Models Code ##############################################
#
#
# Author: Daniel Brown
# Date: 10/26/2017

### Data Preprocessing ###################################################
## Importing R Libraries #################################################
library(tidyverse)
library(randomForest)
library(e1071)
library(rpart)

## Importing the dataset #################################################
dataset <- read.csv("HEALTH_PHMC_24102017160155249.csv") # Importing data
names(dataset) # See different features that the dataset has

## Filtering the dataset for analysis ####################################
dataset <- dplyr::filter(dataset, # Filters by rows containing
                         Measure == "Million US$ at exchange rate")
dataset <- dplyr::select(dataset, year = Year, country = Country, 
                         value = Value) # Filters by column
total.revenue <- aggregate(dataset$value, by=list(dataset$year), FUN=sum)

## Organizing the data into dataframe for ease of use ####################
total.revenue <- total.revenue[1:35,]
total.revenue[1] <- 1:35
names(total.revenue) <- c("year", "value") # names columns for easier reference
names(total.revenue)

### Linear Regression ####################################################
# Below we will be making a 'linear machine' or lm.
regressor <- lm(formula = value ~ year, data = total.revenue)
y.test <- data.frame("year" = 1:35, "value" = 0)
y.pred <- predict(regressor, newdata = y.test)
y.test[2] <- NULL
y.pred <- data.frame("year" = 1:35, "value" = y.pred)

# Creating a ggplot
ggplot(data = total.revenue) + aes(x = year, y = value) +
  geom_point(color = "Blue") + 
  geom_point(data = y.pred, aes(x = year, y = value), 
             color = "Green") + 
  ggtitle("Linear Regression Model")

### Polynomial Regression ################################################
total.poly <- total.revenue
for (i in 2:4){ # This generates some polynomials for us. Fun!
  total.poly[, i+1] <- total.poly[, 1]^i
}
polyreg <- lm(formula = value ~ ., data = total.poly)
poly.test <- total.poly
poly.test[2] <- 0

poly.pred <- predict(polyreg, newdata = poly.test)
poly.pred <- data.frame("year" = 1:35, "value" = poly.pred)

ggplot(data = total.revenue) + aes(x = year, y = value) +
  geom_point(color = "Blue") + 
  geom_point(data = poly.pred, aes(x = year, y = value), 
             color = "Green") + 
  ggtitle("Polynomial Regression Model")

### SV Regression ########################################################
svreg = svm(formula = value ~ year,
            data = total.revenue,
            type = 'eps-regression',
            kernel = 'radial')
svr.pred <- predict(svreg, newdata = y.test)

svr.pred <- data.frame("year" = 1:35, "value" = svr.pred)

ggplot(data = total.revenue) + aes(x = year, y = value) +
  geom_point(color = "Blue") + 
  geom_point(data = svr.pred, aes(x = year, y = value), 
             color = "Green") + ggtitle("SVR Model")

### Decision Tree Regression #############################################
## Atempt 1 ##############################################################
tree.reg = rpart(formula = value ~ year,
                 data = total.revenue,
                 control = rpart.control(minsplit = 1))
tree.pred <- predict(tree.reg, newdata = y.test)

tree.pred <- data.frame("year" = 1:35, "value" = tree.pred)

ggplot(data = total.revenue) + aes(x = year, y = value) + 
  geom_point(color = "Blue") + 
  geom_point(data = tree.pred, aes(x = year, y = value), 
             color = "Green") + ggtitle("Decision Tree Regression Model")

## Atempt 2 ##############################################################
two.tree.reg = rpart(formula = value ~ year,
                     data = total.revenue,
                     control = rpart.control(minsplit = 2))
two.tree.pred <- predict(two.tree.reg, newdata = y.test)

two.tree.pred <- data.frame("year" = 1:35, "value" = two.tree.pred)

ggplot(data = total.revenue) + aes(x = year, y = value) +
  geom_point(color = "Blue") + 
  geom_point(data = two.tree.pred, aes(x = year, y = value), 
             color = "Green") + 
  ggtitle("Decision Tree Regression Model V2")

## Plotting decision tree branches #######################################
plot(two.tree.reg, main = "Decision Tree V2")

### Random Forest Regression #############################################
forest.reg = randomForest(x = total.revenue[-2],
                          y = total.revenue$value,
                          ntree = 500)
forest.pred <- predict(forest.reg, newdata = y.test)

forest.pred <- data.frame("year" = 1:35, "value" = forest.pred)

ggplot(data = total.revenue) + aes(x = year, y = value) +
  geom_point(color = "Blue") + 
  geom_point(data = forest.pred, aes(x = year, y = value), 
             color = "Green") + ggtitle("Random Forest Regression Model")
