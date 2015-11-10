# Revenue-Prediction-NN
Using Neural Networks, to predict Annual Revenue

# First Read the train.csv and test.csv

train_info<-read.csv(
".../train_update.csv")
# train_data<-train_data[sample(nrow(train_data)),]

test_data<-read.csv(
".../test_update.csv")

########################################################

# Forming input and outputs for training and testing sets. 
# Observe that column 1 is the ID of Restaurant, which
# is not an input, so we begin from column 2
train_in<-train_info[,1:45]
train_out<-train_info[,46]


# Standardizing the Output
mean_val<-mean(train_out)
std_val<-sd(train_out)
train_out<-(train_out-mean_val)/std_val



train<-cbind(train_in,train_out)
colnames(train)<-c("BigCities","Other","FC","IL","DT","MB","date",
"city","p1","p2","p3","p4","p5",
"p6","p7","p8","p9","p10","p11","p12","p13","p14","p15","p16","p17","p18",
"p19","p20","p21","p22","p23","p24","p25","p26","p27","p28","p29","p30",
"p31","p32","p33","p34","p35","p36","p37","revenue")


####################################################


# Performing "neuralnet" on the data
library("neuralnet")
# we picked the features with higher regression with output
revenue_net<-neuralnet(train_out~date + BigCities + Other + p2 + p28 + p6 + p29 + p13 + 
                            FC + p21 + IL + p11 + p8 + p22 + p10,
train,hidden=8,threshold=0.002,algorithm='rprop+',
linear.output=TRUE)
print(revenue_net)
#plot(revenue_net)

#compute RMSE on train data
rev_train<-revenue_net$net.result
mat_rev_train<-sapply(rev_train,FUN='array')
mat_rev_train<-(mat_rev_train)*std_val+mean_val

RMSE_train<-sqrt(sum((train_info[,46]-mat_rev_train)^2)/length(mat_rev_train))
print(RMSE_train)



######################################

# Evaluating the performance
train_vec<-c("date" , "BigCities" , "Other" , "p2" , "p28" , "p6" , "p29" , "p13"  
               ,"FC",  "p21",  "IL",  "p11",  "p8",  "p22",  "p10")
train_result<-compute(revenue_net,test_data[,train_vec])$net.result
mat_train_result<-sapply(train_result,FUN='array')
mat_train_result<-(mat_train_result)*std_val+mean_val

cols<-c(0:(length(mat_train_result)-1))
mydata<-cbind(cols,mat_train_result)
colnames(mydata)<-c("Id","Prediction")
write.csv(
mydata,".../data.csv",row.names=FALSE)

