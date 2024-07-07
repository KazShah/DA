#Assignment-2
#Muhammad Kazim
#218200403

########################################################
#Question:1.7:
#install.packages(" igraph ")
#install.packages("ggm")
library(igraph )
library(ggm )
library(bnlearn)

dag_1<- DAG(S ~ E, J ~ E+G, M ~J+A+S,D~M)
plotGraph (dag_1 , nodesize =20 , tcltk =FALSE , vc=" white ")
#D-seperation
dSep(dag_1,first=c("E"),second=c("A","G"),cond=c("S","M"))
dSep(dag_1,first=c("S","A"),second="G",cond=c("E","J","D"))

#1.8: Markov blanket of S

models="[E][G][A][S|E][J|E:G][M|S:J:A][D|M]"
res2=model2network(models)
res2
plot(res2)
# Markov blanket
mb(res2,"S")
# plot the markov blanket of G in the network
markov=graphviz.plot(res2 , highlight = list ( nodes = mb(res2 , "S"),
                                           col = " blue"))
#Display with different colors
str(nodeRenderInfo(markov))
node.attrs = nodeRenderInfo(markov)
node.attrs$fill[("J")] = "tomato"
node.attrs$fill[("A")] = "blue"
node.attrs$fill[("E")] = "orange"
node.attrs$fill[("M")] = "red"
nodeRenderInfo(markov) = node.attrs
renderGraph(markov)
#######################################################################

#2.1(a):Show obtained belief network for this distribution
#install.packages("BiocManager")
#BiocManager::install()
#BiocManager::install(c("Rgraphviz"))
#BiocManager::install(c("gRain", "RBGL", "gRbase"),force = TRUE)
library(Rgraphviz)
library(gRbase)
library(gRain)
library(RBGL)
library(grid)

dag_2<- DAG(W~H,T~H,R~H+T,S~T)
plotGraph (dag_2 , nodesize =20 , tcltk =FALSE , vc=" white ")

ch<-c("cold","hot")
lhm<-c("low","high","medium")
lh<-c("low","high")

h<-cptable(~humidity,values=c(0.4,0.6),levels=lh)
w.h<-cptable(~wind|humidity,values=c(0.2,0.5,0.3,0.6,0.2,0.2),levels=lhm)
t.h<-cptable(~temp|humidity,values=c(0.9,0.1,0.2,0.8),levels=ch)
s.t<-cptable(~solar|temp,values=c(0.1,0.6,0.3,0.3,0.4,0.3),levels=lhm)
p.h.t<-cptable(~precipitation|humidity:temp,values=c(0.3,0.7,0.3,0.7,0.1,0.9,0.2,0.8),levels =lh)

plist<-compileCPT(list(h,w.h,t.h,s.t,p.h.t))
plist
plist$humidity
plist$wind
plist$temp
plist$solar
plist$precipitation
net1<-grain(plist)
#summary(net1)
plot(net1$dag)

#a)Prob of humidity is high, given temp is cold
net2 <- setEvidence(net1, nodes=c("temp"),states=c("cold"))
querygrain(net2,nodes=c("humidity"))

#b)Joint distribution of temp, humidity and precipitation
querygrain(net1,nodes=c("temp","humidity","precipitation"), type="joint")

#c)Prob of solar is high, given wind speed is medium and precipitation is high
net3 <- setEvidence(net1, nodes=c("wind","precipitation"),states=c("medium","high"))
querygrain(net3,nodes=c("solar"))

#d)Marginal distribution of precipitation
querygrain(net1, nodes=c("precipitation"), type="marginal")
#e)P(R=high|T=cold, H=high)
net4 <- setEvidence(net1, nodes=c("temp","humidity"),states=c("cold","high"))
querygrain(net4,nodes=c('precipitation'))
#f)P(R=high|T=cold, H=high,S=low)
net5 <- setEvidence(net1, nodes=c("temp","solar"),states=c("cold","low"))
querygrain(net5,nodes=c('precipitation'))
#g)P(R=high|T=cold, H=high,W=medium)
net6 <- setEvidence(net1, nodes=c("temp","wind"),states=c("cold","medium"))
querygrain(net6,nodes=c('precipitation'))


#######################################
#Question:4.1:

#install.packages('bnlearn')
library(bnlearn)
library(Rgraphviz)
library(grid)
library(gridExtra)

data(alarm)
summary(alarm)

modelstring = paste0("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF][LVF]",
                     "[STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA][HRSA|ERCA:HR][ANES]",
                     "[APL][TPR|APL][ECO2|ACO2:VLNG][KINK][MINV|INT:VLNG][FIO2][PVS|FIO2:VALV]",
                     "[SAO2|PVS:SHNT][PAP|PMB][PMB][SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC]",
                     "[MVS][VMCH|MVS][VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG]",
                     "[ACO2|VALV][CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]")
dag = model2network(modelstring)
par(mfrow = c(1,1))
graphviz.plot(dag)
#BIC score
#For first 100 data points
bnet_100<-hc(head(alarm,100),score='bic')
bnet_100
bnet_100_sc<-score(bnet_100,alarm,type = 'bic')
bnet_100_sc
graphviz.plot(bnet_100)
#For first 1000 data points
bnet_1000<-hc(head(alarm,1000),score='bic')
bnet_1000
bnet_1000_sc<-score(bnet_1000,alarm,type = 'bic')
bnet_1000_sc
graphviz.plot(bnet_1000)
#For first 1500 data points
bnet_15000<-hc(head(alarm,15000),score='bic')
bnet_15000
bnet_15000_sc<-score(bnet_15000,alarm,type = 'bic')
bnet_15000_sc
graphviz.plot(bnet_15000)

#Bde scores
#For first 100 data points
bnet_bde_100<-hc(head(alarm,100),score='bde')
bnet_bde_100
bde_100_sc<-score(bnet_bde_100,alarm,type = 'bde')
bde_100_sc
graphviz.plot(bnet_bde_100)
#For first 1000 data points
bnet_bde_1000<-hc(head(alarm,1000),score='bde')
bnet_bde_1000
bde_1000_sc<-score(bnet_bde_1000,alarm,type = 'bde')
bde_1000_sc
graphviz.plot(bnet_bde_1000)
#For first 1500 data points
bnet_bde_15000<-hc(head(alarm,15000),score='bde')
bnet_bde_15000
bde_15000_sc<-score(bnet_bde_15000,alarm,type = 'bde')
bde_15000_sc
graphviz.plot(bnet_bde_1500)
############################################################
#Question:4.3(a)
#BIC calculation for entire dataset
bic_all<-hc(alarm,score='bic')
bic_all
graphviz.plot(bic_all)
#BID calculation for entire dataset
bde_all<-hc(alarm,score='bde')
bde_all
graphviz.plot(bde_all)
#Scores
score(bic_all,alarm,type='bic')
score(bde_all,alarm,type='bde')

#Question:4.3(b):
#compare(dag,bic_all)
graphviz.compare(dag,bic_all)
#compare(dag,bde_all)
graphviz.compare(dag,bde_all)
#Question:4.3(c):
df<-data.frame(alarm)
fittedParams=bn.fit (bic_all,df,method="mle")
fittedParams['ECO2']
#Question:4.3(d)
cpquery(fittedParams,event=(BP=="HIGH"),evidence= ((STKV=="LOW") &(HR=="NORMAL") &(SAO2=="NORMAL")))

####################################################################################
#Question5.2
library(data.table)
library(lubridate)
library(dplyr)
#install.packages('dplyr')

#Reading health, weather and air quality files

weather_data<-read.csv('Canberra_climate_data_2012_2019.csv')
air_qual_data<-read.csv('Air_Quality_Monitoring_Data.csv')
health_data<-read.csv('3303_9 Underlying causes of death (Australian Capital Territory).csv',header = FALSE)
######################################################################
#Checking first few rows
head(health_data)
head(weather_data)
head(air_qual_data)
#######################################################################
#Cleaning weather data tidy
weather_data<-weather_data[,c(3,7,8,9,10)]
names(weather_data)[2] <- "Rainfall(mm)"
names(weather_data)[3] <- "Max_Temperature(c)"
names(weather_data)[4] <- "Min_Temperature(c)"
names(weather_data)[5] <- "Solar_Exposure(MJmm)"
#Na value check
colSums(is.na(weather_data))
#Only rainfall column has 9 na values, so removing all na with 0 value as rainfall 
weather_data[is.na(weather_data)] <- 0
#Averagin over years
weather_data_final<-aggregate(.~Year, weather_data, mean)
#########################################################################
#Making air quality data tidy
#Removing extra columns from air quality data
air_qual_data<-air_qual_data[,c(3,4,5,7,8,9,18,19)]
names(air_qual_data)[3] <- "O3"
names(air_qual_data)[7] <- "AQI"
#air_qual_data$Year <- as.Date(air_qual_data$DateTime,"%y")
air_qual_data$Year<- as.Date(air_qual_data$DateTime,format='%d/%m/%Y')
air_qual_data<-air_qual_data[,c(-1)]
air_qual_data$Year <- ymd(air_qual_data$Year)
air_qual_data$Year<-year(air_qual_data$Year)
air_qual_data<-air_qual_data[,c(-7)]
air_qual_data<-aggregate(.~Year, air_qual_data, mean)
############################################################################
#Making health data tidy: removing desease codes as its not required
head(health_data)
health_data<-health_data[-1,-1]
col<-rep(2010:2019, each=3)
colnames(health_data)<-col
df_temp<-health_data[1:2,]
df_n <- transpose(df_temp)
df_n<-cbind(col,df_n)
df_n[is.na(df_n)] <- 0
df_n<-aggregate(.~col, df_n$v2,sum)
Total_deaths<-df_n %>% filter(row_number() %% 3 == 0)
Male_deaths<-df_n %>% filter(row_number() %% 3 == 1)
Female_deaths<-df_n %>% filter(row_number() %% 3 == 2)
############################################################################
#Due to availablity of datasets with different dates, removing extra data entries to sync all datasets
air_qual<-air_qual_data[-c(8,9),]
rownames(air_qual) <- NULL
mortality<-Total_deaths[-c(1,2,3),]
rownames(mortality) <- NULL
weather<-weather_data_final[-c(1),]
rownames(weather) <- NULL
#Now combining all 03 datasets after cleanup
final_data<-data.frame(air_qual,weather[,-1],mortality[,3])
names(final_data)[12] <- "Deaths"
names(final_data)[8] <- "Rainfall(mm)"
names(final_data)[9] <- "Temp_Max(c)"
names(final_data)[10]<-"Temp_Min(c)"
names(final_data)[11] <- "Solar_exp(MJmm)"

final_data$Deaths <- as.numeric(as.character(final_data$Deaths))
summary(final_data)
plot(final_data$Deaths)
x<-final_data[,1]
y<-final_data[,12]
z<-final_data[,7]
plot(x,y, main="Scatter Plot",xlab="Years ", ylab="Deaths ", pch=19)
barplot(y,names.arg=x,xlab="Year",ylab="Deaths",col="blue",
        main="Deaths from 2013-2019 (J00-J99)",border="red")

barplot(z,names.arg=x,xlab="Year",ylab="AQI",col="light green",
        main="AQI",border="red")
##################################################################
#Defining factors
#0:bad,1:Good
#0:low,1:high
#0:high,low
N<-sapply(final_data[,2],function(x)if(x>0.005) 0 else 1)
N <- as.factor(N)
O<-sapply(final_data[,3],function(x)if(x>0.019) 0 else 1)
O <- as.factor(O)
CO<-sapply(final_data[,4],function(x)if(x>0.3) 0 else 1)
CO <- as.factor(CO)
PM10<-sapply(final_data[,5],function(x)if(x>10) 0 else 1)
PM10 <- as.factor(PM10)
PM25<-sapply(final_data[,6],function(x)if(x>8) 0 else 1)
PM25 <- as.factor(PM25)
AQI<-sapply(final_data[,7],function(x)if(x>50) 0 else 1)
AQI <- as.factor(AQI)
Rain<-sapply(final_data[,8],function(x)if(x<1) 0 else 1)
Rain <- as.factor(Rain)
Temp_max<-sapply(final_data[,9],function(x)if(x<22) 0 else 1)
Temp_max <- as.factor(Temp_max)
Temp_min<-sapply(final_data[,10],function(x)if(x<7) 0 else 1)
Temp_min <- as.factor(Temp_min)
Solar_exp<-sapply(final_data[,11],function(x)if(x<17) 0 else 1)
Solar_exp <- as.factor(Solar_exp)
Death<-sapply(final_data[,12],function(x)if(x<150) 1 else 0)
Death <- as.factor(Death)
########################################################################
df<-data.frame(N,O,CO,PM10,PM25,AQI,Rain,Temp_max,Temp_min,Solar_exp,Death)
library(bnlearn)
################################################################
#gs
gsnet<-gs(df)
gsnet
graphviz.plot(gsnet)
#############################################################
#hc with bic
bnet_air<-hc(df,score='bic')
bnet_air
graphviz.plot(bnet_air)
############################################################################
#hc with bic
bde_air<-hc(df,score='bde')
bde_air
graphviz.plot(bde_air)
#########################################
#bde bic comparison
graphviz.compare(bnet_air,bde_air)
##############################################################################

































