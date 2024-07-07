#Muhammad Kazim
#218200403
#Assignment#1
#******************************************************#

#Question-1:Heron Island Data Analysis

#Data loading

data<-as.matrix(read.csv('AIMSHeronIslandData.csv',header=TRUE,sep = ','))
data<-data[sample(1:366,200),c(1:5)]
write.table(data,'Muhammad_kazim_218200403_HIMyData.txt')

#1.1:Histogram and Box plot for Humidity
colnames(data)<-c("WaterTemp","WindSpeed","AirTemp","AirPressure","Humidity")
humidity<-data[,5]
hist(humidity,main="Humidity Histogram",xlab="Humidity",col="yellow")
boxplot(humidity,main="Humidity Box plot",xlab="Humidity")

#1.2: Five number summary
summary(humidity)

#1.3:Scatterplot B/W Air temp and Water Temp
x<-data[,3]
y<-data[,1]
plot(x,y, main="Scatter Plot",xlab="Air Temperatire ", ylab="Water Temperature ", pch=19)
fit <- lm(y~x)
summary(fit)
abline(lm(y~x))
cor(x,y)
coeOfDet = cor(x,y)^2*100
coeOfDet
#summary(fit)$r.squared

#1.4: Create new variables WaterTBucket, WindSBucket, AirPreBucket
data_mod_1<-as.data.frame(data)
data_mod_1['WaterTBucker'] <- NA
data_mod_1['WindSBucket'] <- NA
data_mod_1['AirPreBucket'] <- NA
data_mod_1$WaterTBucket<-ifelse(data_mod_1$WaterTemp>25,'High','Low')
data_mod_1$WindSBucket<-ifelse(data_mod_1$WindSpeed>30,'High','Low')
data_mod_1$AirPreBucket<-ifelse(data_mod_1$AirPressure>1019,'High','Low')
write.csv(data,'Test.csv')
library(expss)
cro(data_mod_1$WaterTBucket,data_mod_1$WindSBucket,data_mod_1$AirPreBucket)

#*********************************************************#
#Question 4 (c):

library(Bolstad)
y = c(2.5)

#theta values 
mu = seq(1, 5, by = 0.1)

#define the trapezoidal prior
mu.prior = rep(0, length(mu))
mu.prior[mu <= 2] = -1 / 5 + mu[mu <= 2] /5
mu.prior[mu>2 & mu<=3]=-3/5+mu[mu>2 & mu<=3]*(2/5)
mu.prior[mu>3 & mu<=4]=9/5-mu[mu>3 & mu<=4]*(2/5)
mu.prior[mu>4]=1-mu[mu>4]/5


#find posterior
results = normgcp(y,0.2, density = "user", mu = mu, mu.prior = mu.prior)

#plot prior, liklihood and posterior on a single plot
plot(results, overlay = TRUE, which = 1:3)

#plot the above results (prior, liklihood. posterior) in different axes
decomp(results)


#Finding the posterior mean and standard deviation for the above.

## find the posterior mean and std. deviation for the above
mean(results)
var(results)
sd(results)


#***************************************************#

#Question5: Clustering
zz<-read.table('ITdata.txt')
zz<-as.matrix(zz)
cl<-kmeans(zz,5,nstart=25)
cl
plot(zz, col = cl$cluster,main = "K-means Clustering")
points(cl$centers, col = 1:5, pch = 8)

#Spectral Clustering

#install.packages('kernlab')
library(kernlab)
sp_cl<-specc(zz,centers=5)
sp_cl
plot(zz,col=sp_cl,main='Spectral Clustering')

#***************************************************#
#Question6: Guassian modeling and MLE

library(Bolstad)
hi<-read.csv('AIMSHeronIslandData.csv')

#6.1:Plot Histogram

hist(hi$Water.Temperature..1.6m.depth,main='Water Temperate @ 1.6m depth',col='red')

#6.2: Single Guassian Model
mean_est<-mean(hi$Water.Temperature..1.6m.depth)
stand_dev<-sd(hi$Water.Temperature..1.6m.depth)
g_dist<-rnorm(hi$Water.Temperature..1.6m.depth,mean_est,stand_dev)

#Logliklihood
mean_g_dist<-mean(g_dist)
sd_g_dist<-sd(g_dist)
ml_e<-sum(dnorm(g_dist, mean=mean_g_dist, sd=sd_g_dist, log=TRUE))
ml_e
#Plotting Guassian density distribution
library(ggplot2)

data_new<-as.data.frame(g_dist)

ggplot(data=data_new)+
  geom_histogram(aes(x=g_dist,y= ..density..))+
  stat_function(geom ="line",fun=dnorm,args=list(mean=mean_est,sd=stand_dev))+
  ggtitle("Guassian density distribution ")

#6.3: Mixture Guassian Model
#install.packages("mixtools")
library(mixtools)

#Gaussian mixture
mixmdl = normalmixEM(hi$Water.Temperature..1.6m.depth,k=2) # k components
mixmdl
summary(mixmdl)
plot(mixmdl,which=2)
#plot(mixmdl, density = TRUE, w = 1.1)
mixmdl$lambda
mixmdl$mu
mixmdl$sigma
mixmdl$loglik

#plotting the combined curve
x1 <- seq(min(hi$Water.Temperature..1.6m.depth),max(hi$Water.Temperature..1.6m.depth),length=10000)
y = array(0,c(10000,length(mixmdl$lambda)))
for (i in (1:length(mixmdl$lambda)))
{
  y[,i] <- dnorm(x1,mean=mixmdl$mu[i], sd=mixmdl$sigma[i])
}
ycomb=array(0,c(10000,1))
for (j in 1:length(mixmdl$lambda))
{
  ycomb[,1]<-ycomb[,1] + mixmdl$lambda[j]*y[,j]
}
lines(x1,ycomb, col="black", lwd=2, type="l", lty=2)

#Log liklihood values vs iterations
plot(mixmdl$all.loglik)
plot(mixmdl,which=1)

