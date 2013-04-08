# chi-squared test
#   for correlation between sunspot number (SSN) and earthquake number

# 1. monthly
EQs_mon <- read.table('data/EQs-mon', col.names=c('year', 'month', 'neqks'))
SSN_mon <- read.table('data/SSN-mon-1', col.names=c('year', 'month', 'sunspots'))

x <- EQs_mon$neqks
y <- SSN_mon$sunspots

#chisq.test(x, y)
table(x, y) # show contingency table
chisq.test(table(x, y))

# 2. now yearly
EQs_year <- read.table('year-EQs', col.names=c('year', 'neqks'))
SSN_year <- read.table('SSN-year-1', col.names=c('year', 'sunspots'))

x <- EQs_year$neqks
y <- SSN_year$sunspots

#chisq.test(x, y)
table(x, y)
chisq.test(table(x, y))
