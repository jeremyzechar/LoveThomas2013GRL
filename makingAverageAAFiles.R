# From the daily AA files, make monthly average and annual average files

d.aaDaily <- read.table('data/AA-day', col.names = c('year', 'month', 'day',
                                                         'value'))
years<-min(d.aaDaily$year):max(d.aaDaily$year)
months <- 1:12

totalNumberOfMonths <- length(months) * length(years)
d.aaMonthly <- data.frame(year = rep(0, totalNumberOfMonths), 
                                month = rep(0, totalNumberOfMonths),
                                value = rep(0, totalNumberOfMonths))
monthCounter <- 0
for (theYear in years){
  for (theMonth in months){
    monthCounter <- monthCounter + 1
    d.aaMonthly$year[monthCounter] <- theYear
    d.aaMonthly$month[monthCounter] <- theMonth
    d.aaMonthly$value[monthCounter] <- mean(d.aaDaily$value[which(
      d.aaDaily$year == theYear & 
        d.aaDaily$month == theMonth)])
  }
}

write.table(d.aaMonthly, 'data/AA-month', row.names = FALSE, 
            col.names = FALSE)

totalNumberOfYears <- length(years)
d.aaYearly <- data.frame(year = years, value = rep(0, totalNumberOfYears))
yearCounter <- 0
for (theYear in years){
  yearCounter <- yearCounter + 1
  d.aaYearly$value[yearCounter] <- mean(d.aaMonthly$value[which(
    d.aaMonthly$year == theYear)])
}

write.table(d.aaYearly, 'data/AA-year', row.names = FALSE, 
            col.names = FALSE)