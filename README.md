See the whole article in <a href="http://www.tulipquant.com/2016/06/24/using-brexit-polls-to-build-an-algorithm-to-trade-gbpusd/">tulipQuant</a>.

Last 23rd of June a referendum took place in the United Kingdom, in which the voters decided to leave the European Union. After the results were published, the GBP/USD dropped significantly. But, could one have created a trading strategy for the GBP/USD, that was based solely in the polls that were periodically published?

After the referendum, I wanted test if this was possible. The model I wanted to build should have the following features:
<ul>
 	<li>The strategy would only buy/short sell the GBP/USD, if in that day a new poll was published. Then, in the following day the operation would be closed.</li>
 	<li>Positive polls for the remain campaign should have a positive impact for the pound, and viceversa. Moreover, the greater the difference between the percentage of remain and leave votes is, the bigger this impact should be.</li>
 	<li>The closer the referendum day is, the higher the impact of the poll should be.</li>
 	<li>If the polls suggest that the remain and leave campaign will receive the same amount of votes more or less, the GBP/USD should follow the trend it previously had.</li>
</ul>
Taking this features in mind, I tested the following model:
<p style="text-align: center;">R''(t)=t/T(P(t)-0.5)+R'(t)</p>
Here, R(t) denotes the exchange rate of GBP/USD, T indicates the time at which the referendum will take place, and P(t) indicates the percentage of remain votes of the latest poll, for day t. This differential equation satisfies all the features I asked to the model:
<ul>
 	<li>If a poll is published on a day, and the poll is favorable for the remain campaign, then P(t)-0.5 is positive, so that the GBP/USD will tend to go up. And viceversa, if it is favorable for the leave campaign, P(t)-0.5 would be negative, so that the exchange rate will tend to go down.</li>
 	<li>If t is close to T, that is, we are close to the referendum day, t/T would be close to 1, and if t is not close to T, it would be close to 0. Thus, the closer we are to the referendum day, the greater the influence of the poll would be.</li>
 	<li>The term R'(t) makes the GBP/USD to follow the previous trend. The greater the derivative is, the higher the strength of the trend would be.</li>
</ul>
Intuitively, think of the differential equation as a moving bicycle that is speeding up. However, there is wind around the bicycle, that depending on its direction and strength would make the bicycle slower or faster. The bicycle represents the exchange rate GBP/USD, and the wind represents the polls: depending on the direction, it will move the rate in one direction or the other. But in all cases, the bicycle will try continue to move its current direction.

So, how could we solve the differential equation? An easy way consists of using finite differences, in order to numerically approximate the solution.

<h1>Results</h1>
The poll data was obtained from the <a href="http://www.bbc.com/news/uk-politics-eu-referendum-36271589">BBC's site</a>. The information of the exchange rate GBP/USD and the polls are gathered in <a href="https://github.com/bolorsociedad/Brexit-polls-strategy-for-the-GBP-USD/blob/master/data.csv">this spreadsheet</a>. The returns are shown in the following chart:

<img class="size-full wp-image-133 aligncenter" src="https://raw.githubusercontent.com/bolorsociedad/Brexit-polls-strategy-for-the-GBP-USD/master/Returns.png" alt="Returns" width="467" height="356" />

The returns from the 13th of September 2015 and  the 22nd of June 2016 was 11.9%. In the same period of time, the S&amp;P 500 lost -0.38%, and the FTSE 100 returned about 0.9%. Taking a look to the chart, one can see that until March 2016 more or less the returns remained quite flat. However, from March to June the returns increased a lot, probably due to the fact that in this period the polls became more important and relevant, when it comes to the GBP/USD exchange rate.

&nbsp;
<h1>The Code</h1>
The code is shown below, and it is also available in <a href="https://github.com/bolorsociedad/Brexit-polls-strategy-for-the-GBP-USD">GitHub</a>.

<pre lang="PHP">
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
</pre>

<h1>Further improvement</h1>
The model can be generalized a bit, in the following way:
<div style="text-align: center;">R''(t)=alpha  ( t/T)^beta (P(t)-0.5)+gamma R'(t)</div>
Based on this model, the constants alpha,beta,gamma that offer the best fit could be found, in order to improve a bit the performance of the model.
