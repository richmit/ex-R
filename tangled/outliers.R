dataType <- 'hand'
if(dataType=='normzoom')
  daData <- do.call(c, lapply(1:5, function (x) rnorm(2^(7-x), sd=2*x)))      
if(dataType=='hand')
  daData <- c(rep(c(1,-1), 10), c(8,11,15))
if(dataType=='weibull')
  daData <- rweibull(200, .8)                                                      
dataType

cParmSig  <- 3                                                # This is where the "3" comes from in "3 Sigma"
daMean    <- mean(daData)                                     # The "center" of our "non-outlier" interval
daSD      <- sd(daData)                                       # The "radius" of our "non-outlier" interval
cutOffSig <- cParmSig*c(-1, 1)*daSD+daMean                    # Lower and upper limits of our "non-outlier" interval

cParmHem  <- 3                                                # This is the most common value used today
daMAD     <- mad(daData)                                      # The "center" of our "non-outlier" interval
daMedian  <- median(daData)                                   # The "radius" of our "non-outlier" interval
cutOffHem <- cParmHem*c(-1, 1)*daMAD+daMedian                 # Lower and upper limits of our "non-outlier" interval

cParmBox  <- 1.5                                              # This is the most common value used today
daQUAR    <- quantile(daData, c(.25, .75))                    # The first and third quartiles (Q1 & Q3)
cutOffBox <- daQUAR+cParmBox*c(-1,1)*(daQUAR[2]-daQUAR[1])    # Lower and upper limits of our "non-outlier" interval
