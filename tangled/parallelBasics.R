library(parallel)
instancesInMyComputeCluster <- detectCores()
instancesInMyComputeCluster
myComputeCluster <- makeCluster(instancesInMyComputeCluster)
myComputeCluster
unlist(clusterCall(myComputeCluster, 'Sys.getpid'))
unlist(clusterEvalQ(myComputeCluster, 2*Sys.getpid()))
aVar <- 1:10
parSapply(myComputeCluster, aVar, sin)
#parLapply(myComputeCluster, aVar, sin)
bVar <- 10
clusterExport(myComputeCluster, 'bVar')
parSapply(myComputeCluster, aVar, function (x) bVar*x)
cVar <- rnorm(instancesInMyComputeCluster*2^14)
clusterExport(myComputeCluster, "cVar")
system.time(b<-sapply(cVar, function (x) for(i in 1:500) sin(x)))
system.time(b<-parSapply(myComputeCluster, cVar, function (x) for(i in 1:500) sin(x)))
stopCluster(myComputeCluster)
