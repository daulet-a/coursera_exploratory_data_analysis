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

hist(power$Global_active_power,
     col = 'red',
     main = 'Global Active Power',
     xlab = 'Global Active Power (kilowatts)',
     ylab = 'Frequency',
     )

dev.copy(png, 'Plot1.png')
dev.off()
