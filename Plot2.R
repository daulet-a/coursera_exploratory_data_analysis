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

plot(Global_active_power ~ DateTime, power, type = "l",
     ylab = "Global Active Power (kilowatts)",
     xlab = NA,
     xaxt = 'n',
     xlim = c(min(power$DateTime), max(power$DateTime) + 100)
     )

axis(1, at = c(min(power$DateTime),
               median(power$DateTime),
               max(power$DateTime) + 60),
     labels = c(wday(min(power$DateTime), label = TRUE),
                wday(max(power$DateTime), label = TRUE),
                wday(max(power$DateTime) +60, label = TRUE)))

dev.copy(png, 'Plot2.png')
dev.off()
