library(quantmod)
library(timeSeries)


data<-read.csv("data.csv")



money<-c(1000); #Start with 1000$
i0=247;

for (i in i0:(nrow(data)-1)) {
  if (is.na(data$Remain[i])) {
    money<-c(money, money[length(money)]);
    cat("Stay still.\n")
    next();
  }
  T=nrow(data)-i0;
  nextDay=2*data$Exchange[i]-data$Exchange[i-1]+(i-i0)/T * (data$Remain[i]-0.5)+(data$Exchange[i]-data$Exchange[i-1])
  
  
  
  margin=0;
  if (nextDay>data$Exchange[i]*(1+margin) & data$Exchange[i]*0.01>m) {
    money<-c(money, money[length(money)]*as.numeric(data$Exchange[i+1])/as.numeric(data$Exchange[i]));
    cat("Buy. Money:" , money[length(money)], "\n");
  }
  else if (nextDay<data$Exchange[i]*(1-margin) & data$Exchange[i]*0.01>m) {
  
    money<-c(money, money[length(money)]*as.numeric(data$Exchange[i])/as.numeric(data$Exchange[i+1]));
    cat("Sell. Money:" , money[length(money)], "\n");
  }
  else {
    money<-c(money, money[length(money)]);
    cat("Stay still.\n")
  }
  
}

money.zoo<-zoo(money, as.Date(data$Date[-(1:i0)], "%d/%m/%Y"));
exchange.zoo<-zoo(1000*data$Exchange[(i0):(nrow(data)-1)]/as.numeric(data$Exchange[i0]), as.Date(data$Date[-(1:i0)], "%d/%m/%Y"));
z<-as.zoo(cbind(money.zoo, exchange.zoo))
plot(x = z, ylab = "Retrns", xlab="Date", main = "Cumulative Returns", screens=1, col=c("red", "blue"))
legend(x = "topleft", legend = c("Strategy", "GBP/USD returns"), 
       lty = 1,col = c("red", "blue"))
