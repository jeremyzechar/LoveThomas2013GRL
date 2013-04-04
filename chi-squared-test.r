# monthly
EQs_mon <- read.table('data/EQs-mon', col.names=c('year', 'month', 'neqks'))
# SSN_mon <- read.table('mon-group-num', col.names=c('year', 'month', 'day', 'sunspots', 'sunspot2'), fill=TRUE)
SSN_mon <- read.table('data/SSN-mon-bin-1', col.names=c('year', 'month', 'sunspots'))

x <- EQs_mon$neqks
y <- SSN_mon$sunspots

chisq.test(x, y)
table(x, y)
chisq.test(table(x, y))

# now yearly

EQs_year <- read.table('year-EQs', col.names=c('year', 'neqks'))
SSN_year <- read.table('SSN-year-bin-1', col.names=c('year', 'sunspots'))

x <- EQs_year$neqks
y <- SSN_year$sunspots

chisq.test(x, y)
table(x, y)
chisq.test(table(x, y))