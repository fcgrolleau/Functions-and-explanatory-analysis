lambda<-0.2
samplesize<-40
n_sim<-1000
df<-matrix(rexp(samplesize*n_sim, lambda), n_sim, samplesize)
means<-apply(df, 1, mean)
hist(means)
abline(v=1/lambda, lwd=10)
variances<-(apply(df, 1, sd))^2
hist(variances)
abline(v=1/lambda^2, lwd=10)

library(ggplot2)
df2<-data.frame(name=factor(rep(c("1000 exp", "1000 means of 40 exps", "1000 variances of 40 exps"), each=1000)) , value=c(df[,1], means, variances))
head(df2)
tail(df2)
summarystat<-data.frame(distribution_mean=1/lambda,
                        mean_of_1000_means=mean(df2$value[which(df2$name=="1000 means of 40 exps")]),
                        mean_of_1000_variances=mean(df2$value[which(df2$name=="1000 variances of 40 exps")]))

summarystat                
g<-ggplot(df2, aes(value, color=name, fill=name)) + geom_histogram(linetype="blank", position="identity", alpha=.6, binwidth = 1)
g
g<-g + geom_vline(xintercept = summarystat$distribution_mean)
g
g<-g + geom_vline(xintercept = summarystat$mean_of_1000_variances)
g



             