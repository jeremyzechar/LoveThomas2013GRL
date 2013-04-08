# use molchanDiagram function for plotting all Molchans

par(mfrow=c(2, 3))

# SSN
frame() # plot.new()
molchanDiagram('data/EQs-mon','data/SSN-mon')
molchanDiagram('data/EQs-year','data/SSN-year')

# AA
molchanDiagram('data/EQs-day','data/AA-day')
molchanDiagram('data/EQs-mon','data/AA-mon')
molchanDiagram('data/EQs-year','data/AA-year')

# VEL
# molchanDiagram('data/EQs-day','data/VEL-day')
# molchanDiagram('data/EQs-mon','data/VEL-mon')
# molchanDiagram('data/EQs-year','data/VEL-year')