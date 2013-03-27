# First column is rank, second column is the actual solar terrestrial variable 
# annual average, and the third column is the number of earthquakes in the year. 
# The last three columns are factors I use for constructing the plots themselves, 
# so ignore those.
dfG <- read.table('ssn-rank.all', col.names=c('rank', 'value', 'neqks', 
                                              'ignore1', 'ignore2', 'ignore3'))
dfV <- read.table('vel-rank.all', col.names=c('rank', 'value', 'neqks', 
                                              'ignore1', 'ignore2', 'ignore3'))
dfAA <- read.table('mag-rank.all', col.names=c('rank', 'value', 'neqks', 
                                               'ignore1', 'ignore2', 'ignore3'))

# We're going to compare samples above the median value and below the median value
gMedian <- median(dfG$value)
vMedian <- median(dfV$value)
aaMedian <- median(dfAA$value)

eqksBelowMedianG <- dfG$neqks[which(dfG$value < gMedian)]
eqksAboveMedianG <- dfG$neqks[which(dfG$value > gMedian)]
eqksAboveMedianG <- dfG$neqks[57:112]
eqksBelowMedianV <- dfV$neqks[which(dfV$value < vMedian)]
eqksAboveMedianV <- dfV$neqks[which(dfV$value > vMedian)]
eqksBelowMedianAA <- dfAA$neqks[which(dfAA$value < aaMedian)]
eqksAboveMedianAA <- dfAA$neqks[which(dfAA$value > aaMedian)]
eqksAboveMedianAA <- dfAA$neqks[57:112]

N <- sum(dfG$neqks) # number of M7.5+ events since 1900
N_63 <- sum(dfV$neqks) # number of M7.5+ events since 1963

# Reproduce Figure 2 from the article
par(mfrow=c(1, 3))

plot(dfG$rank, dfG$neqks, pch = 16, col='green', xlab='Rank Annual Avg G',
     ylab='M7.5+ eqks per year')
points(dfG$rank[which(dfG$value > gMedian)], eqksAboveMedianG, pch = 16, 
       col='brown')
abline(mean(dfG$neqks), 0)

plot(dfV$rank, dfV$neqks, pch = 16, col='green', xlab='Rank Annual Avg V',
     ylab='M7.5+ eqks per year')
points(dfV$rank[which(dfV$value > vMedian)], eqksAboveMedianV, pch = 16, 
       col='brown')
abline(mean(dfV$neqks), 0)

plot(dfAA$rank, dfAA$neqks, pch = 16, col='green', xlab='Rank Annual Avg AA',
     ylab='M7.5+ eqks per year')
points(dfAA$rank[which(dfAA$value > aaMedian)], eqksAboveMedianAA, pch = 16, 
       col='brown')
abline(mean(dfAA$neqks), 0)

# Reproduce Table 3 final row first 6 columns (chi-squared test)
chisquaredG<-sum((eqksBelowMedianG - eqksAboveMedianG)^2 / 
                   (eqksBelowMedianG + eqksAboveMedianG))
pG<-pchisq(chisquaredG, length(eqksBelowMedianG), lower.tail=FALSE)
chisquaredV<-sum((eqksBelowMedianV - eqksAboveMedianV)^2 / 
                   (eqksBelowMedianV + eqksAboveMedianV))
pV<-pchisq(chisquaredV, length(eqksBelowMedianV), lower.tail=FALSE)
chisquaredAA<-sum((eqksBelowMedianAA - eqksAboveMedianAA)^2 / 
                   (eqksBelowMedianAA + eqksAboveMedianAA))
pAA<-pchisq(chisquaredAA, length(eqksBelowMedianAA), lower.tail=FALSE)
print(paste('7.5', N, N_63, pG, pV, pAA ))

# Reproduce Table 4 final row (Welch's T-test)
# Welch's t-test (unpaired, allows unequal variances, only want to see
# if the mean of two samples is the same)
resultsG <- t.test(eqksBelowMedianG, eqksAboveMedianG) 
resultsV <- t.test(eqksBelowMedianV, eqksAboveMedianV) 
resultsAA <- t.test(eqksBelowMedianAA, eqksAboveMedianAA)
print(paste('7.5', resultsG$estimate, resultsG$p.value, resultsV$estimate, 
            resultsV$p.value, resultsAA$estimate, resultsAA$p.value))

# check the assumption behind the t-test: that the values in each sample
# are normally distributed
require(nortest)
par(mfrow=c(1, 2))
lillie.test(eqksBelowMedianG) # recommended by M. Taroni
shapiro.test(eqksBelowMedianG) # recommended by Wikipedia (http://en.wikipedia.org/wiki/Student%27s_t-test#Assumptions)
hist(eqksBelowMedianG) # just to get an idea
lillie.test(eqksAboveMedianG)
shapiro.test(eqksAboveMedianG)
hist(eqksAboveMedianG)

par(mfrow=c(1, 2))
lillie.test(eqksBelowMedianV)
shapiro.test(eqksBelowMedianV)
hist(eqksBelowMedianV)
lillie.test(eqksAboveMedianV)
shapiro.test(eqksAboveMedianV)
hist(eqksAboveMedianV)

par(mfrow=c(1, 2))
lillie.test(eqksBelowMedianAA)
shapiro.test(eqksBelowMedianAA)
hist(eqksBelowMedianAA)
lillie.test(eqksAboveMedianAA)
shapiro.test(eqksAboveMedianAA)
hist(eqksAboveMedianAA)

# Wilcoxon rank-sum aka Mann-Whitney U, unpaired and does not assume normality
# http://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U
wilcox.test(eqksBelowMedianG, eqksAboveMedianG, alternative="greater")
wilcox.test(eqksBelowMedianV, eqksAboveMedianV, alternative="greater")
wilcox.test(eqksBelowMedianAA, eqksAboveMedianAA, alternative="greater")


# Scratch figuring out the chi-squared test
# maxBin <- max(max(eqksBelowMedianAA), max(eqksAboveMedianAA))
# R<-table(factor(eqksBelowMedianAA, 0:maxBin))
# S<-table(factor(eqksAboveMedianAA, 0:maxBin))
# manualChiSquared <- sum((R - S) * (R - S) / (R + S))
# x <- as.table(rbind(R, S))
# chisq.test(x, simulate.p.value=TRUE, B=10000)
# manualPValue<-pchisq(manualChiSquared, maxBin - 1, lower.tail=FALSE)


# ordering doesn't matter for Welch's t-test
# t.test(dfAA$neqks[1:56], dfAA$neqks[58:113])
# t.test(dfAA$neqks[56:1], dfAA$neqks[58:113])


# Reproducing Jeffrey's results:
# "for 1-56, 57-112 I obtain chi**2 = 55.71 and p = 0.4856"
# R<-dfAA$neqks[1:56]
# S<-dfAA$neqks[57:112]
# chisquared<-sum((R-S)^2 / (R+S))
# p<-pchisq(chisquared, 56, lower.tail=FALSE)

# "for 1-56, 58-113 I obtain chi**2 = 62.55 and p = 0.2549"
# R<-dfAA$neqks[1:56]
# S<-dfAA$neqks[58:113]
# chisquared<-sum((R-S)^2 / (R+S))
# p<-pchisq(chisquared, 56, lower.tail=FALSE)

# # Pearson's chi-square test, paired, check if two distributions are "the same"
# maxBinG <- max(max(eqksBelowMedianG), max(eqksAboveMedianG))
# xG <- as.table(rbind(table(factor(eqksBelowMedianG, 0:maxBinG)), 
#                      table(factor(eqksAboveMedianG, 0:maxBinG))))
# maxBinV <- max(max(eqksBelowMedianV), max(eqksAboveMedianV))
# xV <- as.table(rbind(table(factor(eqksBelowMedianV, 0:maxBinV)), 
#                      table(factor(eqksAboveMedianV, 0:maxBinV))))
# maxBinAA <- max(max(eqksBelowMedianAA), max(eqksAboveMedianAA))
# xAA <- as.table(rbind(table(factor(eqksBelowMedianAA, 0:maxBinAA)), 
#                       table(factor(eqksAboveMedianAA, 0:maxBinAA))))
# 
# chisq.test(xG, simulate.p.value=TRUE, B=10000)
# chisq.test(xV, simulate.p.value=TRUE, B=10000)
# chisq.test(xAA, simulate.p.value=TRUE, B=10000)
# 
# chisq.test(eqksBelowMedianG, eqksAboveMedianG, simulate.p.value = TRUE)
# chisq.test(eqksBelowMedianV, eqksAboveMedianV, simulate.p.value = TRUE)
# chisq.test(eqksBelowMedianAA, eqksAboveMedianAA, simulate.p.value = TRUE)
# 
# # Illustrate that there's a difference b/w using the first 56/next 56 and
# # using the first 56 and last 56 (i.e., pairing)
# chisq.test(dfAA$neqks[1:56], dfAA$neqks[57:112], simulate.p.value = TRUE)
# chisq.test(dfAA$neqks[1:56], dfAA$neqks[58:113], simulate.p.value = TRUE)
