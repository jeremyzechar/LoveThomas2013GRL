# read in catalog and decide in which size bin to group the eqks -- Marcus did 
# this already w/ EQs-year, EQs-mon, EQs-day
# read in alarm function of choice, must use same binning as binned catalog -- 
# for sunspots, can use SSN-mon or SSN-year; for activity can use AA-day, 
# AA-mon, & AA-year
# 
# binned catalog and alarm function are now arrays of the same length
# from the alarm function array get a unique list of values; these go into a 
# thresholds list
# instantiate arrays for tau and nu; they can be the same length as the number 
# of unique thresholds + 1; the first point is nu = 1, tau = 0.
# now go through each threshold value; 
# tau = sum(which(alarmfunction > threshold)) / length(alarmfunction); 
# nu = 1 - sum(eqks[which(alarmfunction > threshold)]) / N, where N = sum(eqks)
molchanDiagram <- function(catalog, alarmFunction) {
  df.AlarmFunction <- read.table(alarmFunction)
  # the alarm value is stored in the final column
  alarmFunction <- df.AlarmFunction[, ncol(df.AlarmFunction)] 
  cells <- length(alarmFunction)
  
  df.Catalog <- read.table(catalog)
  # the number of eqks is stored in the final column
  eqks <- df.Catalog[, ncol(df.Catalog)] 
  N <- sum(eqks)
#   print(N)

  thresholds <- sort(unique(alarmFunction), decreasing = TRUE)
  
  tau <- rep(0, length(thresholds))
  nu <- rep(0, length(thresholds))
  counter <- 0
  for (threshold in thresholds){
    counter <- counter + 1
    tau[counter] <- length(which(alarmFunction >= threshold)) / cells
    nu[counter] <- 1 - sum(eqks[which(alarmFunction >= threshold)]) / N
    if ((counter - 1) %% 100 == 0){
     print(paste0('(threshold, tau, nu) = (', threshold, ', ', tau[counter], 
                  ', ', nu[counter], ')'))
    }
  }
  plot(tau, nu, xlab = 'alarm fraction', ylab = 'miss rate')
  abline(1, -1)
}