#Task 1

#Loading Text file and assigning to a matrix;selecting random sample of 350 from the file

#Loading necessary Libararies

library("ggplot2")
library("gridExtra")

options(max.print=10000)
E<-as.matrix(read.table("Energy20.txt"))
colnames(E)<-c("X1","X2","X3","X4","X5","Y")
head(E)
Y_subset<- E[sample(1:671,350),c(1:6)]
Y_subset
summary(Y_subset)

#Caculating coorelation of X1 to X5 with Y

minkowski<-function(x,y,p=1) (sum(abs(x-y)^p))^(1/p)

manhattan= array(0,5)
euclid= array(0,5)
pearson= array(0,5)
spearman = array(0,5)


for (i in 1:5){
  euclid[i]= minkowski(Y_subset[,6],Y_subset[,i],2)}
euclid

for (i in 1:5){
  manhattan[i]= minkowski(Y_subset[,6],Y_subset[,i],1)}

for (i in 1:5){
  pearson[i]= cor(Y_subset[,6],Y_subset[,i]) }
pearson

for (i in 1:5){
  spearman[i]= cor(Y_subset[,6],Y_subset[,i],method = "spearman")}

all = rbind(euclid,manhattan,pearson,spearman)

all


#Scatter plot of X1 to X5 Variables with respect to Power Consumption Y; converting matrix into dataframe for using in ggplots

Y_data<-as.data.frame(Y_subset)

T_K <- ggplot(Y_data, aes(X1, Y))+ geom_jitter()+ggtitle("Temperature in Kitchen (Celsius) Vs Power Consumption(Wh) ")
T_K 
H_K <- ggplot(Y_data, aes(X2, Y))+ geom_jitter()+ggtitle("Humidity in Kitchen Vs Power Consumption(Wh) ")
H_K 
T_O <- ggplot(Y_data, aes(X3, Y))+ geom_jitter()+ggtitle("Temperature Outside(Celsius) Vs Power Consumption(Wh) ")
T_O 
H_O <- ggplot(Y_data, aes(X4, Y))+ geom_jitter()+ggtitle("Humidity Outside Vs Power Consumption(Wh) ")
H_O 
Visibility <- ggplot(Y_data, aes(X5, Y))+ geom_jitter()+ggtitle("Visibility(Km) Vs Power Consumption(Wh) ")
Visibility


#Displaying All plots togeather for comparison
grid.arrange(T_K,H_K,T_O,H_O,Visibility,nrow=2,ncol=3)

#Histogram for Variables X1 to X5 and Y

x1<-Y_subset[,1]
x2<-Y_subset[,2]
x3<-Y_subset[,3]
x4<-Y_subset[,4]
x5<-Y_subset[,5]
y<-Y_subset[,6]
hist(x1,main="X1",xlab="Temperature in Kitchen (Celsius)",col="yellow")
hist(x2,main="X2",xlab="Humidity in Kitchen",col="red")
hist(x3,main="X3",xlab="Temperature Outside (Celsius)",col="blue")
hist(x4,main="X4",xlab="Humidity Outside",col="pink")
hist(x5,main="X5",xlab="Visibility(km)",col="violet")
hist(y,main="Y",xlab="Power Consumption (wh)",col="green")


#Skewness Test

T_K_hist_test<-ggplot(Y_data, aes(x=X1)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666") 
T_K_hist_test
H_K_hist_test<-ggplot(Y_data, aes(x=X2)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666") 
H_K_hist_test
T_O_hist_test<-ggplot(Y_data, aes(x=X3)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666") 
T_O_hist_test
H_O_hist_test<-ggplot(Y_data, aes(x=X4)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666") 
H_O_hist_test
Visibility_test<-ggplot(Y_data, aes(x=X5)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666") 
Visibility_test
Power_his_test<-ggplot(Y_data, aes(x=Y))+geom_histogram(aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666")+ggtitle("Power Consumption")
Power_his_test

#Task#2: Data Transformation
#Creating matrix for 04 variables X1 to X4 and Y to 
z_sc<-Y_subset[1:350,c("X1","X2","X3","X4","Y")]
head(z_sc)

#Transformation on X1:
Neg_X1<-matrix(1:350,ncol=1)
for(i in Neg_X1){
  Neg_X1[i,1]=(39.22-z_sc[i,1])
}
Neg_X1
neg2=sort(Neg_X1, decreasing = TRUE)
hist(neg2)

#Tranformation on X2:

stand_X2<-matrix(1:350,ncol=1)
for(i in stand_X2){
  stand_X2[i,1]=(z_sc[i,2]-mean(z_sc[,2]))/sd(z_sc[,2])
}
hist(stand_X2)


#Transformation on X3:
Neg_X3<-matrix(1:350,ncol=1)
for(i in Neg_X3){
  Neg_X3[i,1]=(6.71997-z_sc[i,3])
}
Neg_X3
neg3=sort(Neg_X3, decreasing = TRUE)
hist(neg3)

#Transformation on X4:
stand_X4<-matrix(1:350,ncol=1)
for(i in stand_X4){
  stand_X4[i,1]=(z_sc[i,4]-mean(z_sc[,4]))/sd(z_sc[,4])
}
hist(stand_X4)


#Transformation on Y:

log_y<-matrix(1:350,nrow=350,ncol=1)
for (i in log_y){
  log_y[i,1]=(log10(z_sc[i,5]))
}
hist(log_y)
log_y_sort=sort(log_y)
hist(z_sc[,5])
final_y<-cbind(log_y_sort)
colnames(final_y)<-c("Y")
hist(log_y_sort)

#Combining Tranformed data into 01 Matrix:
data_transformed<-cbind(Neg_X1,stand_X2,Neg_X3,stand_X4,final_y)
colnames(data_transformed)<-c("X1_T","X2_T","X3_T","X4_T","Y_T")
head(data_transformed)
summary(data_transformed)

#Calculating Linear Scaling for scaling data to [0,1]

linear_scale<-data_transformed[1:350,c("X1_T","X2_T","X3_T","X4_T","Y_T")]
linear_scale[,1] <- (data_transformed[,1]-min(data_transformed[,1]))/(max(data_transformed[,1])-min(data_transformed[,1]))
linear_scale[,2] <- (data_transformed[,2]-min(data_transformed[,2]))/(max(data_transformed[,2])-min(data_transformed[,2]))
linear_scale[,3] <- (data_transformed[,3]-min(data_transformed[,3]))/(max(data_transformed[,3])-min(data_transformed[,3]))
linear_scale[,4] <- (data_transformed[,4]-min(data_transformed[,4]))/(max(data_transformed[,4])-min(data_transformed[,4]))
linear_scale[,5] <- (data_transformed[,5]-min(data_transformed[,5]))/(max(data_transformed[,5])-min(data_transformed[,5]))

#Writing scaled data into text file

write.table(linear_scale,"M_Kazim_transformed.txt")

#Histogram for Variables X1 to X4 & Y after transformation and scaling

x1_t<-linear_scale[,1]
x2_t<-linear_scale[,2]
x3_t<-linear_scale[,3]
x4_t<-linear_scale[,4]
y_t<-linear_scale[,5]
hist(x1_t,main="X1",xlab="Temperature in Kitchen (Celsius)",col="yellow")
hist(x2_t,main="X2",xlab="Humidity in Kitchen",col="red")
hist(x3_t,main="X3",xlab="Temperature Outside(Celsius)",col="blue")
hist(x4_t,main="X4",xlab="Humidity Outside ",col="pink")
hist(y_t,main="Y",xlab="Power Consumption (wh)",col="green")

par(mfrow=c(3,2))
hist(x1,main="Histogram of X1",xlab="Temperature in Kitchen (Celsius)",col="yellow")
hist(x1_t,main="Histogram of X1_T",xlab="Temperature in Kitchen (Celsius)",col="yellow")
hist(x2,main="Histogram of X2",xlab="Humidity in Kitchen",col="red")
hist(x2_t,main="Histogram of X2_T",xlab="Humidity in Kitchen",col="red")
hist(x3,main="Histogram of X3",xlab="Temperature Outside (Celsius)",col="blue")
hist(x3_t,main="Histogram of X3_T",xlab="Temperature Outside (Celsius)",col="blue")
hist(x4,main="Histogram of X4",xlab="Humidity Outside",col="pink")
hist(x4_t,main="Histogram of X4_T",xlab="Humidity Outside",col="pink")
hist(y,main="Histogram of Y",xlab="Power Consumption (Kwh)",col="green")
hist(y_t,main="Histogram of Y_T",xlab="Power Consumption (Kwh)",col="green")



#Task 3: Building Models

source("AggWaFit718.R")

fit.QAM(linear_scale[,c(1:4,5)],"PMOutput_05.txt","PMStats_05.txt",g=PM05,g.inv=invPM05)
fit.QAM(linear_scale[,c(1:4,5)],"PMOutput_5.txt","PMStats_5.txt",g=PM5,g.inv=invPM5)
fit.QAM(linear_scale[,c(1:4,5)],"WAMOutput.txt","WAMStats.txt")
fit.OWA(linear_scale[,c(1:4,5)],"OWAoutput.txt", "OWAstats.txt")
fit.QAM(linear_scale[,c(1:4,5)],"QMoutput.txt", "QMstats.txt", g=QM, g.inv= invQM)
fit.choquet(linear_scale[,c(1:4,5)], "Choquetoutput2.txt", "Choquetstats2.txt")

#Task 4: Selecting Model for Prediction

energy_predict_weight=c( 0.147513970824479,0.40996628172164,0.306158322208034,0.136361425245848)
#energy_predict_weight_choquet=c(0.0636252342345172,0.0120026193721731,0.410668878768256,0.334752759488832,0.336489045117922,0.661512280751671,0.862149347413484,0,0.0636252342345172,0.577597704788617,0.819409832634167,0.334752759488832,0.525515030248994,0.661512280751648,1.00000000000026)

#Applying Transformations and scaling on new values according to Task 2:
energy_predict<-array(1:4)
energy_predict[1]=17
energy_predict[2]=39
energy_predict[3]=4
energy_predict[4]=77
energy_predict

energy_predict[1]=(39.22-energy_predict[1])
energy_predict[2]=(energy_predict[2]-mean(z_sc[,2]))/sd(z_sc[,2])
energy_predict[3]=(6.71997-energy_predict[3])
energy_predict[4]=(energy_predict[4]-mean(z_sc[,4]))/sd(z_sc[,4])
energy_predict

#Calculating new Linear Scaling

new_scale<-array(1:4)
new_scale[1] <- (energy_predict[1]-min(data_transformed[,1]))/(max(data_transformed[,1])-min(data_transformed[,1]))
new_scale[2] <- (energy_predict[2]-min(data_transformed[,2]))/(max(data_transformed[,2])-min(data_transformed[,2]))
new_scale[3] <- (energy_predict[3]-min(data_transformed[,3]))/(max(data_transformed[,3])-min(data_transformed[,3]))
new_scale[4] <- (energy_predict[4]-min(data_transformed[,4]))/(max(data_transformed[,4])-min(data_transformed[,4]))
new_scale

#Assigning new values to trained model for prediction

PM(new_scale,energy_predict_weight,0.5)

#choquet(new_scale,energy_predict_weight_choquet)

#Task 5:Linear Regression model
#Applying linear regression model on original transformed data


X1_linear=linear_scale[,1]
X2_linear=linear_scale[,2]
X3_linear=linear_scale[,3]
X4_linear=linear_scale[,4]
Y_linear=linear_scale[,5]
data_linear<-cbind(X1_linear,X2_linear,X3_linear,X4_linear,Y_linear)

fit <- lm(Y_linear~X1_linear+X2_linear+X3_linear+X4_linear,data=data_linear)
summary(fit)

#Applying linear regression model on predicted data

predict(fit,0.7871826,0.4158476,0.4041068,0.3451398)

plot(fit)
data_linear$predicted <- predict(fit)
data_linear$residuals <- residuals(fit)

#Visualizing Y from models with true Y
linear_model<-read.table("PMOutput_05.txt")
Y_model_predict<-linear_model[,5]
Y_transformed<-Y_linear
plot(Y_model_predict,Y_transformed)
true_Y<-Y_subset[,6]
plot(data_linear$residuals,true_Y)
plot(Y_linear,data_linear$predicted)
plot(Y_linear,data_linear$residuals)


