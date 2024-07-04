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

plot(Sub_metering_1 ~ DateTime, power, type = "l",
     ylab = "Energy sub metering",
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
                wday(max(power$DateTime) +60, label = TRUE)))

legend('topright', legend = colnames(power[7:9]),
       col = c('black', 'red', 'blue'),
       lty = 1)

dev.copy(png, "Plot3.png")
dev.off()
