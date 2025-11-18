
# just to find quick metrics for results section

library(readr)
library(dplyr)
library(ggplot2)

tf1 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/tf1.csv')
i1 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i1.csv')
tf2 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/tf2.csv')
i2 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i2.csv')
tf1 = filter(tf1, grepl('C61', `Animal ID`) == FALSE)
i1 = filter(i1, grepl('C61', `Animal ID`) == FALSE)
tf2 = filter(tf2, grepl('C61', `Animal ID`) == FALSE)
i2 = filter(i2, grepl('C61', `Animal ID`) == FALSE)



# General stats

dives = tf1  %>%
  group_by(`Animal ID`) %>%
  dplyr::summarise(dive_count = length(unique(`Dive ID`)))


mean(dives$dive_count) # 63.3
sd(dives$dive_count) # 42.3


windows = tf1 %>%
  group_by(`Animal ID`, `Dive ID`) %>%
  dplyr::summarise(window_count = length(unique(`Window ID`)))


mean(windows$window_count) # 26.2
sd(windows$window_count) # 22.6


min(tf1$`Window size (s)`) # 0.04
max(tf1$`Window size (s)`) # 28.36
mean(tf1$`Window size (s)`) # 0.81
sd(tf1$`Window size (s)`) # 1.85


pce = i1 %>%
  group_by(`Animal ID`, `Window ID`) %>%
  dplyr::summarise(prey = sum(`Prey number`))

type = i1 %>%
  group_by(`Prey type`) %>%
  dplyr::summarise(count = sum(`Prey number`))



depth = tf2 %>%
  group_by(`Animal ID`, `Dive ID`) %>%
  dplyr::summarise(depth = first(`Max dive depth`))

mean(depth$depth) # 30.9
sd(depth$depth) # 25.6
max(depth$depth) # 86.5


window_depth = tf1 %>%
  group_by(`Animal ID`, `Window ID`) %>%
  dplyr::summarise(depth = first(`Max window depth`),
                   mean_VeDBA = first(`Mean window VeDBA`),
                   sd_VeDBA = first(`SD window VeDBA`))

mean(window_depth$depth) # 39.9
sd(window_depth$depth) # 17.3

mean(window_depth$mean_VeDBA) # 0.38
sd(window_depth$mean_VeDBA) # 0.15
mean(na.omit(window_depth$sd_VeDBA)) # 0.09
sd(na.omit(window_depth$sd_VeDBA)) # 0.07


sum(tf2$`Prey TRUE/FALSE`) / nrow(tf2)


# EM Performance

sum(tf1$`Prey TRUE/FALSE`)

sum(tf1$`Prey TRUE/FALSE`) / nrow(tf1) # 0.0218

mean(i1$`Total prey in window`) # 2.38
sd(i1$`Total prey in window`) # 2.38

ggplot() +
  geom_boxplot(data = tf1, aes(x = `Prey TRUE/FALSE`, y = `Window size (s)`))

tf1 %>%
  group_by(`Prey TRUE/FALSE`) %>%
  summarise(mean = mean(`Window size (s)`),
            sd = sd(`Window size (s)`))

tf1 %>%
  group_by(`Prey TRUE/FALSE`) %>%
  summarise(mean = mean(`Max window depth`),
            sd = sd(`Max window depth`))

tf1 %>%
  group_by(`Prey TRUE/FALSE`) %>%
  summarise(mean = mean(`Mean window VeDBA`),
            sd = sd(`Mean window VeDBA`))

filter(tf1, is.na(tf1$`SD window VeDBA`) == FALSE) %>%
  group_by(`Prey TRUE/FALSE`) %>%
  summarise(mean = mean(`SD window VeDBA`),
            sd = sd(`SD window VeDBA`))

mean(tf2$`Time spent hunting (s)`) # 17.9
sd(tf2$`Time spent hunting (s)`) # 20.5

sum(i2$`Mixed prey TRUE/FALSE`) / nrow(i2) # 0.376
sum(i2$`Group foraging TRUE/FALSE`) / nrow(i2) # 0.136

mean(i2$`Total prey`) # 10.36
sd(i2$`Total prey`)

tf2 %>%
  group_by(`Prey TRUE/FALSE`) %>%
  summarise('mean' = mean(`Time spent hunting (s)`),
            'sd' = sd(`Time spent hunting (s)`))

tf2 %>% # against dive duration
  group_by(`Prey TRUE/FALSE`) %>%
  summarise('mean' = mean(`Time spent hunting (s)`/`Dive duration (s)`),
            'sd' = sd(`Time spent hunting (s)`/`Dive duration (s)`))
sum(i1$`Prey number`)

# Plots

library(ggplot2)
library(tidyr)


tf1_long = pivot_longer(tf1, cols = c(7, 8, 10, 11), names_to = 'metric', values_to = 'value')

tf1_long$`Prey TRUE/FALSE`[tf1_long$`Prey TRUE/FALSE` == 'TRUE'] = 'True positive window'
tf1_long$`Prey TRUE/FALSE`[tf1_long$`Prey TRUE/FALSE` == 'FALSE'] = 'False positive window'

tf1_long$`metric`[tf1_long$`metric` == 'Mean window VeDBA'] = 'Mean window VeDBA (m/s²)'
tf1_long$`metric`[tf1_long$`metric` == 'SD window VeDBA'] = 'SD window VeDBA (m/s²)'
tf1_long$`metric`[tf1_long$`metric` == 'Max window depth'] = 'Max window depth (m)'


ggplot(tf1_long, aes(x = `Prey TRUE/FALSE`, y = value, fill = `Prey TRUE/FALSE`))+
  scale_fill_manual(values = c('True positive window' = 'grey90', 'False positive window' = 'grey50'), name = NULL) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.5) +
  ylab(NULL) + 
  xlab(NULL) +
  facet_wrap(.~ metric, scales = 'free') +
  theme_classic() +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


ggplot()+
  geom_histogram(data = all_non_validated_pce, aes(x = closest_distance), binwidth = 1, fill = 'lightgrey', col = 'black') +
  geom_vline(xintercept = 11.5, linetype = 'dashed', col = 'darkred') +
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50), limits = c(-0.5,50), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  xlab('Distance from unvalidated PCEs to nearest hunting window') +
  ylab(NULL) +
  theme_classic()


in_out = all_non_validated_pce %>%
  group_by(TagID.x) %>%
  summarise('in' = sum(below_threshold),
            'out' = sum(!below_threshold))

in_out_2 = i1 %>%
  group_by(`Animal ID`) %>%
  summarise('in2' = sum(`Prey number`))

colnames(in_out)[colnames(in_out) == 'TagID.x'] = 'Animal ID'

in_out = left_join(in_out, in_out_2, by = 'Animal ID')
in_out = filter(in_out, grepl('C61', `Animal ID`) == FALSE)

in_out$in_total = in_out$`in` + in_out$in2

in_out_long = pivot_longer(in_out, cols = c(3,5), values_to = 'count', names_to = 'type')

in_out_long$type[in_out_long$type == 'in_total'] = 'Inside of hunting window'
in_out_long$type[in_out_long$type == 'out'] = 'Outside of hunting window'

in_out_long$`Animal ID`[grepl('C42', in_out_long$`Animal ID`) == TRUE] = 'C42'
in_out_long$`Animal ID`[grepl('C52', in_out_long$`Animal ID`) == TRUE] = 'C52'
in_out_long$`Animal ID`[grepl('C56', in_out_long$`Animal ID`) == TRUE] = 'C56'
in_out_long$`Animal ID`[grepl('C65', in_out_long$`Animal ID`) == TRUE] = 'C65'
in_out_long$`Animal ID`[grepl('C67', in_out_long$`Animal ID`) == TRUE] = 'C67'
in_out_long$`Animal ID`[grepl('C68', in_out_long$`Animal ID`) == TRUE] = 'C68'
in_out_long$`Animal ID`[grepl('120', in_out_long$`Animal ID`) == TRUE] = 'C120'

in_out_long$`Animal ID` = as.factor(in_out_long$`Animal ID`)

ggplot(data = in_out_long, aes(y = as.numeric(count), x = '', fill = as.factor(type))) +
  scale_fill_manual(name = NULL, values = c('Inside of hunting window' = 'grey90', 'Outside of hunting window' = 'grey50')) +
  geom_col(col = 'black') +
  coord_polar(theta = 'y') +
  facet_wrap(.~ `Animal ID`, scales = 'free') +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


# pre-buffer pie chart


in_out_pre = all_penguins %>%
  group_by(TagID.x) %>%
  summarise('in' = sum(States_new == 'Hunt' & PCE %in% 1:7, na.rm = TRUE),
            'out' = sum(States_new != 'Hunt' & PCE %in% 1:7, na.rm = TRUE))

in_out_pre = filter(in_out_pre, grepl('C61', TagID.x) == FALSE)

in_out_pre$`TagID.x`[grepl('C42', in_out_pre$`TagID.x`) == TRUE] = 'C42'
in_out_pre$`TagID.x`[grepl('C52', in_out_pre$`TagID.x`) == TRUE] = 'C52'
in_out_pre$`TagID.x`[grepl('C56', in_out_pre$`TagID.x`) == TRUE] = 'C56'
in_out_pre$`TagID.x`[grepl('C65', in_out_pre$`TagID.x`) == TRUE] = 'C65'
in_out_pre$`TagID.x`[grepl('C67', in_out_pre$`TagID.x`) == TRUE] = 'C67'
in_out_pre$`TagID.x`[grepl('C68', in_out_pre$`TagID.x`) == TRUE] = 'C68'
in_out_pre$`TagID.x`[grepl('120', in_out_pre$`TagID.x`) == TRUE] = 'C120'

in_out_pre_long = pivot_longer(in_out_pre, cols = 2:3, values_to = 'count', names_to = 'type')

in_out_pre_long$type[in_out_pre_long$type == 'in'] = 'Inside of hunting window'
in_out_pre_long$type[in_out_pre_long$type == 'out'] = 'Outside of hunting window'

ggplot(data = in_out_pre_long, aes(y = as.numeric(count), x = '', fill = as.factor(type))) +
  scale_fill_manual(name = NULL, values = c('Inside of hunting window' = 'grey90', 'Outside of hunting window' = 'grey50')) +
  geom_col(col = 'black') +
  coord_polar(theta = 'y') +
  facet_wrap(.~ `TagID.x`, scales = 'free') +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        axis.ticks = element_blank())



# Hunting time for positive and negative dives

tf_d2_temp = tf_d2
tf_d2_temp$`Prey TRUE/FALSE`[tf_d2_temp$`Prey TRUE/FALSE` == 'TRUE'] = 'Dive with PCE'
tf_d2_temp$`Prey TRUE/FALSE`[tf_d2_temp$`Prey TRUE/FALSE` == 'FALSE'] = 'Dive without PCE'
ggplot() +
  geom_boxplot(data = tf_d2_temp, aes(x = `Prey TRUE/FALSE`, y = `Time spent hunting (s)`/`Dive duration (s)`, fill = `Prey TRUE/FALSE`), 
               outlier.size = 0.4, outlier.alpha = 0.7) +
  scale_fill_manual(values = c('Dive with PCE' = 'grey90', 'Dive without PCE' = 'grey50'), name = NULL) +
  ylab("Proportion of dive duration labelled as 'Hunt'") +
  xlab(NULL) +
  theme_classic() +
  theme(legend.position = 'FALSE')
  
