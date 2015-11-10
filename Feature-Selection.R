# Feature Selection in order to decrease the input dimension
# Please see the useful link below, which uses the 'FSelector'
# package to cover very useful regression (and maybe classfication)
# functions:
# http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Dimensionality_Reduction/Feature_Selection

# Let's call the package
library('FSelector')


# Let's again read the database train.csv
train_data<-read.csv(
".../train.csv")

train_info<-read.csv(
".../train_update.csv")


Inputs<-train_data[,6:43] # inputs P1:P37+revenue
input2<-train_info[,c(1,2,3,4,5,6,46)] # Inputs big city up to MB + revenue

# Linear Regression and Spearman's correlation:
importance<-linear.correlation(revenue~.,Inputs)
importance2<-linear.correlation(revenue~.,input2)

# cutting off 13 most important attributes
subset<-cutoff.k(importance,13)
print(subset)

print(importance2)
