##R code for PPP data exercise: ECON 321/343 (Hegerty)
#Testing relative Purchasing Power Parity: That changes in the exchange rate are related to changes in the two countries' price levels

#CHANGE the letter F if you need to; This is now a direct download
setwd("F:/")
data<-read.csv("https://raw.githubusercontent.com/hegerty/PPP_Example/main/PPPData.csv")
#Check your data first!
head(data)
#Remove the date column
data<-data[,-1]
#Add a new variable: The ratio of US to Eurozone price levels
data$Pratio<-data[,1]/data[,2]
head(data)

#Make a time series
tsdata<-ts(data,start = c(1999,1),frequency = 12)

#Rename the columns (You really just need column 2)
colnames(tsdata)<-c("USCPI","EUCPI","USDperEUR","Pratio")
head(tsdata)

#Plot the Time Series
plot(tsdata,xlab="",main="Time Series")

#Make 12-month percentage changes using lags
#US, Eurozone inflation; inflation differential; exchange rate appreciation 
usinf<-100*((tsdata[,1]/lag(tsdata[,1],-12))-1)
euinf<-100*((tsdata[,2]/lag(tsdata[,2],-12))-1)
infdiff<-usinf-euinf
PercChE<-100*((tsdata[,3]/lag(tsdata[,3],-12))-1)

#Calculate correlation and make a regression line
#The regression itself is stored as an object
cor1<-cor(PercChE,infdiff)
reg1<-lm(PercChE~infdiff)
cor1
summary(reg1)

#Make a scatterplot
#Include regression line and a box with the corelation
plot(infdiff,PercChE,pch=16,main = "Correlation between Inflation and Exchange Rates")
abline(reg1,lwd=4,lty=2)
legend("bottomright", title = "Correlation =", legend=round(cor1,3))

#Calculate coefficient of variation for both variables
CV_INFDIFF<-sd(infdiff)/mean(infdiff)
CV_E<-sd(PercChE)/mean(PercChE)

#Examine them; notice E is 5x higher
CV_INFDIFF
CV_E  
CV_E/CV_INFDIFF

#Plot time series together
#I plot 5 x the inflation differential so the variables could be on the same graph
#You could make a separate right-hand-side axis, but here I am keeping it simple

plot(PercChE,lwd=2,xlab="",ylab="",main = "% Change in USD/EUR and 5*Inf. Differential",ylim=c(-25,25))
par(new=TRUE)
plot(5*infdiff,lwd=3,lty=2,xlab="",ylab="",ylim=c(-25,25),col="dark grey")
legend("bottomleft", legend=c("PercChE","infdiff"),lwd=c(2,3),lty=c(1,2),col=c("black","dark grey"))


#plot to high-res .jpg files: Make sure you set your directory

jpeg("TS4.jpg",width=5,height=8, units="in",res=600)
plot(tsdata,xlab="",main="Time Series")
dev.off()

jpeg("scatter.jpg",width=6,height=3, units="in",res=600)
plot(infdiff,PercChE,pch=16,main = "Correlation between Inflation and Exchange Rates")
abline(reg1,lwd=4,lty=2)
legend("bottomright", title = "Correlation =", legend=round(cor1,3))
dev.off()

jpeg("plot.jpg",width=6,height=3, units="in",res=600)
plot(PercChE,lwd=2,xlab="",ylab="",main = "% Change in USD/EUR and 5*Inf. Differential",ylim=c(-25,25))
par(new=TRUE)
plot(5*infdiff,lwd=3,lty=2,xlab="",ylab="",ylim=c(-25,25),col="dark grey")
legend("bottomleft", legend=c("PercChE","infdiff"),lwd=c(2,3),lty=c(1,2),col=c("black","dark grey"))
dev.off()
