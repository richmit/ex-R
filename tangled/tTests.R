popsz  <- 1000
mean1  <- 1
mean2  <- mean1
mean3  <- -1
group1 <- rnorm(popsz, mean=mean1)
group2 <- rnorm(popsz, mean=mean2)
group3 <- rnorm(popsz, mean=mean3)
allDat <- stack(list(group1=group1, group2=group2, group3=group3))
names(allDat) <- c('v', 'group')
ggplot(data=allDat, aes(x=v, col=group)) + geom_density()
t.test(group1, group2)
t.test(group1, group2, var.equal=TRUE)
t.test(group1, group2, paired=TRUE)
t.test(group1, mu=mean1)
t.test(group2, mu=mean2)
t.test(group3, mu=mean1)
t.test(group3, mu=mean1, alternative="greater")
wilcox.test(group1, mu=mean1)
wilcox.test(group1, group2)
wilcox.test(group1, group2, paired=TRUE)
