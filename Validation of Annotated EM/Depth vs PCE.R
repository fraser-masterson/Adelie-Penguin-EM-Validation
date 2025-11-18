
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)


c42 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/EM results classified/C42_25Hz_EM_classifications.csv')
c42 = left_join(c42, C42_PCE, by = 'Timestamp')


c42 = filter(c42, DatesPos > "2018-01-04 00:11:23 UTC" & DatesPos < "2018-01-04 03:41:20 UTC")

dev.new()

time_diff = as.numeric(difftime(
  as.POSIXct("2018-01-04 10:11:06", tz = 'UTC'),
  as.POSIXct("2018-01-04 00:11:23", tz = 'UTC'),
  units = "secs"
))

penguin = c42[c(10000:20000), ]
ggplot() +
  #geom_line(data = penguin, aes(x = DatesPos + time_diff, y = Pitch), col = 'red') +
  geom_line(data = penguin, aes(x = DatesPos + time_diff, y = depth25Hz)) +
  geom_line(data = penguin, aes(x = DatesPos + time_diff, y = VeDBA), col = 'darkgreen') +
  geom_line(data = penguin, aes(x = DatesPos + time_diff, y = Hunting), col = 'red') +
  geom_point(data = filter(penguin, Hunting != 0), aes(x = DatesPos + time_diff, y = depth25Hz)) +
  geom_point(data = filter(penguin, PCE >= 1), aes(x = DatesPos + time_diff, y = depth25Hz, col = as.factor(PCE))) +
  ylab('') +
  xlab('') +
  labs(col = 'PCE') +
  theme_classic()



