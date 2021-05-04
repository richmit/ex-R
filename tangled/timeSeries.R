daData          <- data.frame(date=as.POSIXct('2012-01-01')+(1:365)*(60*60*24))
daData$idate    <- as.numeric(daData$date)
daData$x        <- (daData$idate-min(daData$idate))/(60*60*24)
daData$trend    <- daData$x/50
daData$seasonal <- sin(pi*daData$x/3.5)            ######## TRY THIS: equal positive and negative components
#daData$seasonal <- abs(1+sin(pi*daData$x/3.5))    ######## TRY THIS: positive seasonal component
daData$random   <- rnorm(daData$x, sd=.25)
daData$val      <- daData$trend+daData$seasonal+daData$random
daDataSeries <- ts(daData$val, frequency=7)
plot(daDataSeries)
daDataDecomp <- decompose(daDataSeries, type='add')
plot(daDataDecomp)
xyplot(daData$val + daDataDecomp$trend + daDataDecomp$seasonal + daDataDecomp$random ~ daData$date,
       type='l',
       outer=TRUE,
       horizontal=FALSE,
       layout=c(1,4))
daDataDecompDF <- data.frame(date=daData$date,
                             val=daData$val,
                             trend=daDataDecomp$trend,
                             seasonal=daDataDecomp$seasonal,
                             random=daDataDecomp$random)
daDataDecompDF <- melt(daDataDecompDF, id="date")
ggplot(data=daDataDecompDF, aes(x=date)) +
    geom_line(aes(y=value))  +
    facet_grid(variable ~ ., scales = "free")
par(mfcol=c(4,1))
par(mar=c(.5,2.5,.5,.5))
plot(daData$date, daData$val, type='l', ylab='', xaxt='n')
text(mean(par('usr')[1:2]), par('usr')[4], 'Value', pos=1, cex=3, col='blue')
par(mar=c(.5,2.5,0,.5))
plot(as.POSIXct('2012-01-01'), 0,
     xlim=range(daData$date), ylim=range(c(daDataDecomp$trend, daData$trend), na.rm=TRUE),
     col=NA, ylab='', xaxt='n')
points(daData$date, daDataDecomp$trend, type='l', xaxt='n')
points(daData$date, daData$trend,       type='l', col='red')
text(mean(par('usr')[1:2]), par('usr')[4], 'Trend', pos=1, cex=3, col='blue')
plot(as.POSIXct('2012-01-01'), 0,
     xlim=range(daData$date), ylim=2*range(c(daDataDecomp$seasonal, daData$seasonal), na.rm=TRUE),
     col=NA, ylab='', xaxt='n')
points(daData$date, daDataDecomp$seasonal, type='l', xaxt='n')
points(daData$date, daData$seasonal,       type='l', col='red')
text(mean(par('usr')[1:2]), par('usr')[4], 'Seasonal', pos=1, cex=3, col='blue')
par(mar=c(2.5,2.5,0,.5))
plot(as.POSIXct('2012-01-01'), 0,
     xlim=range(daData$date), ylim=range(c(daDataDecomp$random, daData$random), na.rm=TRUE),
     col=NA, xlab='', ylab='')
points(daData$date, daData$random,       type='p', col='red', pch=20)
points(daData$date, daDataDecomp$random, type='l', xaxt='n')
text(mean(par('usr')[1:2]), par('usr')[4], 'Random', pos=1, cex=3, col='blue')
fit <- arima(daDataSeries, order=c(5,0,0), seasonal=list(order=c(2,1,0), period=7))
fit
fore <- predict(fit, n.ahead=7*5)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
par(mfcol=c(1,1))
par(mar=c(5,5,5,5))
ts.plot(daDataSeries, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))
smoothedData <- lowess(daData$idate, daData$val, f=.3)
allDat <- bind_rows(mutate(select(daData, date, val),
                           smoother='actual'),
                    mutate(data.frame(date=as.POSIXct(smoothedData$x, origin='1970-01-01 00:00:00 UTC'),
                                      val=smoothedData$y),
                           smoother='lowess'))
ggplot(allDat, aes(x=date, y=val, col=smoother)) +
  geom_line() +
  labs(title='Smoothing Time Series With A Generic Smoother')
