library(data.table)
mtcars
mtcarsDT <- data.table(mtcars, names=rownames(mtcars))
mtcarsDT
mtcarsDT[2,]
mtcarsDT[2]
mtcarsDT[2:5]
mtcarsDT[cyl==6]
setkey(mtcarsDT,names)
mtcarsDT["Valiant"]
mtcarsDT$cylNames <- paste("cyl", mtcarsDT$cyl, sep='=')
setkey(mtcarsDT,cylNames)
mtcarsDT["cyl=6"]
mtcarsDT["cyl=6",mult="first"]
mtcarsDT$cyl
mtcarsDT[,cyl]
mtcarsDT[,disp/cyl]
mtcarsDT[,cylTimesGear:=cyl*gear]
mtcarsDT
mtcarsDT[,list(cylinder=cyl, weight=wt, gear)]
mtcarsDT[,sum(wt)]
mtcarsDT[,list(daSum=sum(wt), daSd=sd(wt))]
mtcarsDT[,sum(wt),cyl]
mtcarsDT[,list(sumWt=sum(wt)),list(cyl, gear)]
mtcarsDT[,sumWtByCyl:=sum(wt),cyl]
mtcarsDT
crbn <- data.table(carb=c(1,2,3), carbs=c("one", "two", "three"))
setkey(mtcarsDT, carb)
setkey(crbn, carb)
merge(mtcarsDT, crbn, all.x=TRUE)
df1 <- fread("../data/dataTable_f1.csv")
df1
df2 <- fread("dataTable_f2.csv.gz")
df2
df1u <- fread('https://www.mitchr.me/SS/exampleR/rcode/../data/dataTable_f1.csv')
df1u
df2 <- fread("gunzip < dataTable_f2.csv.gz | awk -F, 'NR==1 || $3<100 { print $0 }'")
df2
df3 <- fread('dataTable_f3.txt', sep=':', header=FALSE, strip.white=TRUE, col.names=c('name', 'age', 'weight'))
df3
