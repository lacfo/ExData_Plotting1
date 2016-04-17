library(dplyr)
library(lubridate)
setwd("D:/coursera/exploratory/ExData_Plotting1")
data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?")
data1 <- tbl_df(data)
data2 <- filter(data1, Date == c("1/2/2007", "2/2/2007"))
data3 <- mutate(data2, Date = dmy(Date), Time = hms(Time))
m <- dim(data3)[1] # calculate the total number of observations
png(file = "plot1.png")
with(data3, hist(Global_active_power, main = "Global Active Power", xlab = 
                     "global active power(kilowatts)", ylab = "Frequency", col = "green"))
dev.off()
png(file = "plot2.png")
with(data3, plot(Global_active_power, type = "n", xaxt = "n", xlab = "", ylab = ""))
lines(data3$Global_active_power)
title(xlab = "date time", ylab = "Global active power")
by_date <- group_by(data3, Date) ## group by the Date
n <- summarize(by_date, n())[[2]][1] ## calculate the number for 2007/02/01
axis(1, tick = TRUE, at = c(0, n, m), labels = c("Thu", "Fri", "Sat"))
dev.off()
#####################################
##for the part 3
sub1 <- as.vector(data3[[7]])
sub2 <- as.vector(data3[[8]])
sub3 <- as.vector(data3[[9]])
sub <- c(sub1, sub2, sub3)
png(file = "plot3.png")
plot(sub, type = "n", xaxt = "n", xlab = "date time", ylab = "Energy Sub Metering", xlim = c(0,1450))
lines(sub1, col = "blue")
lines(sub2, col = "green")
lines(sub3, col = "black")
axis(1, tick = TRUE, at = c(0, n, m), labels = c("Thu", "Fri", "Sat"))
legend("topright", pch = c("_", "_", "_"), col = c("blue", "green", "black"),
       legend = c("sub_metering_1", "sub_metering_2", "sub_metering_3"))
dev.off()
###############################################
##part 4
#par(mfrow = c(2, 2))
png(file = "plot4.png")
par(mfrow = c(2, 2))
with(data3, plot(Global_active_power, type = "n", xaxt = "n", xlab = "", ylab = ""))
lines(data3$Global_active_power)
title(xlab = "date time", ylab = "Global active power")
by_date <- group_by(data3, Date) ## group by the Date
n <- summarize(by_date, n())[[2]][1] ## calculate the number for 2007/02/01
axis(1, tick = TRUE, at = c(0, n, m), labels = c("Thu", "Fri", "Sat"))
plot(data3$Voltage, type = "n", xaxt = "n", xlab = "", ylab = "")
lines(data3$Voltage)
title(xlab = "date time", ylab = "voltage")
plot(sub, type = "n", xaxt = "n", xlab = "date time", ylab = "Energy Sub Metering", xlim = c(0,1450))
lines(sub1, col = "blue")
lines(sub2, col = "green")
lines(sub3, col = "black")
axis(1, tick = TRUE, at = c(0, n, m), labels = c("Thu", "Fri", "Sat"))
legend("topright", pch = c("_", "_", "_"), col = c("blue", "green", "black"),
       legend = c("sub_metering_1", "sub_metering_2", "sub_metering_3"))
plot(data3$Global_reactive_power, xlab = "date time", type = "n", xaxt = "n")
lines(data3$Global_reactive_power)
axis(1, tick = TRUE, at = c(0, n, m), labels = c("Thu", "Fri", "Sat"))
dev.off()

