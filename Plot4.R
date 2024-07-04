library(dplyr)
library(lubridate)

power <- read.table("household_power_consumption.txt",
                    header = TRUE, sep = ';',
                    na.strings = "?",
                    colClasses = c('character', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
) %>%
  mutate(Date = as.Date(Date, format = '%d/%m/%Y'), 
         DateTime = as.POSIXct(paste(Date, Time), format = '%Y-%m-%d %H:%M:%S'))%>%
  filter(between(Date, as.Date("2007-02-01"), as.Date("2007-02-02")))

png("plot4.png", width = 1200, height = 800, res = 150)

# figure
par(mfrow=c(2,2), mar = c(4,4,2,1), oma = c(1,1,2,1))

# plot1
plot(Global_active_power ~ DateTime, power, type = "l",
     ylab = "Global Active Power (kilowatts)",
     cex.lab = 0.5,
     cex.axis = 0.7,
     xlab = NA,
     xaxt = 'n',
     xlim = c(min(power$DateTime), max(power$DateTime) + 100)
)

axis(1, at = c(min(power$DateTime),
               median(power$DateTime),
               max(power$DateTime) + 60),
     labels = c(wday(min(power$DateTime), label = TRUE),
                wday(max(power$DateTime), label = TRUE),
                wday(max(power$DateTime) +60, label = TRUE)),
     cex.axis = 0.7)

#plot2
plot(Voltage ~ DateTime, power, type = "l",
     ylab = "Voltage",
     cex.lab = 0.5,
     cex.axis = 0.7,
     xaxt = 'n',
     xlim = c(min(power$DateTime), max(power$DateTime) + 100)
)

axis(1, at = c(min(power$DateTime),
               median(power$DateTime),
               max(power$DateTime) + 60),
     labels = c(wday(min(power$DateTime), label = TRUE),
                wday(max(power$DateTime), label = TRUE),
                wday(max(power$DateTime) +60, label = TRUE)),
     cex.axis = 0.7)

#plot3
plot(Sub_metering_1 ~ DateTime, power, type = "l",
     ylab = "Energy sub metering",
     cex.lab = 0.5,
     cex.axis = 0.7,
     xlab = NA,
     xaxt = 'n',
     xlim = c(min(power$DateTime), max(power$DateTime) + 100)
)

lines(Sub_metering_2 ~ DateTime, power, type = 'l', col = 'red')
lines(Sub_metering_3 ~ DateTime, power, type = 'l', col = 'blue')


axis(1, at = c(min(power$DateTime),
               median(power$DateTime),
               max(power$DateTime) + 60),
     labels = c(wday(min(power$DateTime), label = TRUE),
                wday(max(power$DateTime), label = TRUE),
                wday(max(power$DateTime) +60, label = TRUE)),
     cex.axis = 0.7)

legend('topright', legend = colnames(power[7:9]),
       col = c('black', 'red', 'blue'),
       lty = 1,
       bty = 'n',
       cex = 0.5
       )

#plot4
plot(Global_reactive_power ~ DateTime, power, type = "l",
     cex.lab = 0.5,
     cex.axis = 0.7,
     xaxt = 'n',
     xlim = c(min(power$DateTime), max(power$DateTime) + 100)
)

axis(1, at = c(min(power$DateTime),
               median(power$DateTime),
               max(power$DateTime) + 60),
     labels = c(wday(min(power$DateTime), label = TRUE),
                wday(max(power$DateTime), label = TRUE),
                wday(max(power$DateTime) +60, label = TRUE)),
     cex.axis = 0.7)

dev.off()
