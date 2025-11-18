library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(nlme)
library(DHARMa)
library(tidyr)


# load in files from python folder
penguin_files = list.files('/Volumes/FRASER USB/MRes/Data/2022/Python-formatted EM files with PCE')

for (i in 1:length(penguin_files)) {
  current_penguin = str_sub(penguin_files[i], end = -5)
  assign(current_penguin, read_csv(paste('/Volumes/FRASER USB/MRes/Data/2022/Python-formatted EM files with PCE/', penguin_files[i], sep = ''))) 
}



# make stacked penguin file
penguins = c('C42', 'C52', 'C56', 'C61', 'C65', 'C67', 'C68', 'C120')

all_penguins_unfiltered = bind_rows(lapply(penguins, get))


# remove low visibility dives 

all_penguins = filter(all_penguins_unfiltered, low_visibility == 'FALSE')


nrow(filter(all_penguins, PCE %in% 1:7 & States_new != 'Hunt'))



# buffer points by 11 frames and assign closest touching cluster with TRUE (presence/absence) -----------------
# also make matrix of pce labels for each cluster before and after buffering

# make 'closest_before' and 'closest_after' column, which is the distance in rows to nearest prediction window 
# distance calculation

all_penguins$is_hunting = if_else(all_penguins$States_new == 'Hunt', TRUE, FALSE)

all_penguins$row_number = seq(1, nrow(all_penguins), 1)
all_penguins$uniqueID = paste(all_penguins$TagID.x, all_penguins$dive_cluster, all_penguins$hunting_cluster, sep = '_') # prevents mixing in dplyr grouping

all_non_validated_pce = filter(all_penguins, PCE %in% 1:7 & is_hunting == FALSE)
all_non_validated_pce$closest_before = NA
all_non_validated_pce$closest_after = NA

hunting_rows = all_penguins$row_number[all_penguins$is_hunting]

for (i in 1:nrow(all_non_validated_pce)) {
  current_row = all_non_validated_pce$row_number[i]
  
  before_rows = hunting_rows[hunting_rows < current_row]
  after_rows = hunting_rows[hunting_rows > current_row]
  
  all_non_validated_pce$closest_before[i] = before_rows[which.min(current_row - before_rows)]
  all_non_validated_pce$closest_after[i] = after_rows[which.min(after_rows - current_row)]
  
}

all_non_validated_pce$distance_before = abs(all_non_validated_pce$row_number - all_non_validated_pce$closest_before)
all_non_validated_pce$distance_after = abs(all_non_validated_pce$row_number - all_non_validated_pce$closest_after)
all_non_validated_pce$closest_distance = pmin(all_non_validated_pce$distance_before, all_non_validated_pce$distance_after)

all_non_validated_pce_summary = all_non_validated_pce %>%
  dplyr::select(row_number, closest_before, closest_after, distance_before, distance_after)

all_penguins = left_join(all_penguins, all_non_validated_pce_summary, by = 'row_number')


# making a match column based on if a pce occurred in a hunting cluster

match = all_penguins %>%
  group_by(is_hunting, uniqueID) %>%
  summarise(pce_occurred = any(PCE %in% 1:7))
match$pce_occurred[1] = NA

match = match[-1]

all_penguins = left_join(all_penguins, match, by = 'uniqueID')

all_non_validated_pce$before_after_closest = if_else(all_non_validated_pce$distance_before < all_non_validated_pce$distance_after, 'BEFORE', 'AFTER')
all_non_validated_pce$before_after_closest = if_else(all_non_validated_pce$distance_before == all_non_validated_pce$distance_after, 'BOTH', all_non_validated_pce$before_after_closest)

threshold = 11 # 90% quantile in distribution
all_non_validated_pce$below_threshold = if_else(all_non_validated_pce$closest_distance < threshold, TRUE, FALSE)

pce_matrix_pre_buffer = all_penguins %>% # matrix of summed pce labels for each cluster
  group_by(uniqueID) %>%
  summarise(window_size = n(),
            '1' = sum(PCE %in% 1),
            '2' = sum(PCE %in% 2),
            '3' = sum(PCE %in% 3),
            '4' = sum(PCE %in% 4),
            '5' = sum(PCE %in% 5),
            '6' = sum(PCE %in% 6),
            '7' = sum(PCE %in% 7))
pce_matrix_pre_buffer = filter(pce_matrix_pre_buffer, grepl('NA', uniqueID) == FALSE)
pce_matrix_post_buffer = pce_matrix_pre_buffer

all_penguins$pce_occurred_post_buffer = all_penguins$pce_occurred
for (i in 1:nrow(all_non_validated_pce)) {
  
  if (isTRUE(all_non_validated_pce$below_threshold[i] == TRUE &&
             all_non_validated_pce$before_after_closest[i] == 'BEFORE')) {
    
    all_penguins$pce_occurred_post_buffer[all_non_validated_pce$closest_before[i]] = TRUE
    
    current_hunting_cluster = all_penguins$uniqueID[all_non_validated_pce$closest_before[i]]
    matrix_row = which(pce_matrix_post_buffer$uniqueID == current_hunting_cluster)
    matrix_column = which(colnames(pce_matrix_post_buffer) == all_non_validated_pce$PCE[i])
    
    pce_matrix_post_buffer[matrix_row, matrix_column] = pce_matrix_post_buffer[matrix_row, matrix_column] + 1
    
  }
  
  if (isTRUE(all_non_validated_pce$below_threshold[i] == TRUE &&
             all_non_validated_pce$before_after_closest[i] == 'AFTER')) {
    
    all_penguins$pce_occurred_post_buffer[all_non_validated_pce$closest_after[i]] = TRUE
    
    current_hunting_cluster = all_penguins$uniqueID[all_non_validated_pce$closest_after[i]]
    matrix_row = which(pce_matrix_post_buffer$uniqueID == current_hunting_cluster)
    matrix_column = which(colnames(pce_matrix_post_buffer) == all_non_validated_pce$PCE[i])
    
    pce_matrix_post_buffer[matrix_row, matrix_column] = pce_matrix_post_buffer[matrix_row, matrix_column] + 1
    
  }
  if (isTRUE(all_non_validated_pce$below_threshold[i] == TRUE &&
             all_non_validated_pce$before_after_closest[i] == 'BOTH')) {
    
    all_penguins$pce_occurred_post_buffer[all_non_validated_pce$closest_before[i]] = TRUE
    all_penguins$pce_occurred_post_buffer[all_non_validated_pce$closest_after[i]] = TRUE
    
    # choose random between before and after
    current_hunting_cluster = sample(c(all_penguins$uniqueID[all_non_validated_pce$closest_before[i]], 
                                       all_penguins$uniqueID[all_non_validated_pce$closest_after[i]]), size =1)
    matrix_row = which(pce_matrix_post_buffer$uniqueID == current_hunting_cluster)
    matrix_column = which(colnames(pce_matrix_post_buffer) == all_non_validated_pce$PCE[i])
    
    pce_matrix_post_buffer[matrix_row, matrix_column] = pce_matrix_post_buffer[matrix_row, matrix_column] + 1
    
  }
  
}


# adding dive duration and window size columns to all_penguins
all_penguins$uniquediveID = paste(all_penguins$TagID.x, all_penguins$dive_cluster, sep = '_')
dive_dur = all_penguins %>%
  group_by(uniquediveID) %>%
  summarise(dive_duration_s = n()/25)
all_penguins = left_join(all_penguins, dive_dur, by = 'uniquediveID')
all_penguins$window_size_s = all_penguins$window_size / 25

# adding max dive depth column to all_penguins
dive_depth = all_penguins %>%
  group_by(uniquediveID) %>%
  summarise(max_dive_depth = max(depth25Hz))
all_penguins = left_join(all_penguins, dive_depth, by = 'uniquediveID')

pce_matrix_post_buffer$all_pce_sum = pce_matrix_post_buffer$`1` +
  pce_matrix_post_buffer$`2` +
  pce_matrix_post_buffer$`3` +
  pce_matrix_post_buffer$`4` +
  pce_matrix_post_buffer$`5` +
  pce_matrix_post_buffer$`6` +
  pce_matrix_post_buffer$`7`

pce_matrix = pce_matrix_post_buffer
match_buffered = all_penguins %>%
  group_by(uniqueID) %>%
  summarise(pce_occurred_buffer = any(pce_occurred_post_buffer == TRUE))

pce_matrix = filter(pce_matrix, grepl('NA', pce_matrix$uniqueID) == 'FALSE')

# OBJECT LEGEND #######
# all_penguins: main dataset with all variables, unique IDs for each window (penguin_dive_window), and pce lables pre- and post-buffer
# all_penguins$uniqueID: concatenated penguinID, diveID, windowID - to be used at window-level analyses
# all_penguins$uniquediveID: concatenated penguinID, diveID - to be used at dive-level analyses

# pce_matrix: post-buffer matrix with prey type and count for every window and every penguin, 'all_pce_sum' is total pce for that window
# match_buffered: TRUE/FALSE index with every window and every penguin, summarising if any pce happened within a given window post-buffer
#######################






# TRUE/FALSE Models Data ----------------------------------------------------------------------------------


## Model 1 data - presence/absence with energetics at window level

tf_d1 = all_penguins %>%
group_by(TagID.x, dive_cluster, hunting_cluster) %>%
  summarise('Dive duration (s)' = first(dive_duration_s),
            'Max dive depth' = max(depth25Hz),
            'Window size (s)' = first(window_size_s),
            'Max dive depth' = first(max_dive_depth),
            'Max window depth' = max(depth25Hz),
            'Prey TRUE/FALSE' = first(pce_occurred_post_buffer),
            'Mean window VeDBA' = mean(VeDBA),
            'SD window VeDBA' = sd(VeDBA)
            )
colnames(tf_d1)[colnames(tf_d1) == 'TagID.x'] = 'Animal ID'
colnames(tf_d1)[colnames(tf_d1) == 'dive_cluster'] = 'Dive ID'
colnames(tf_d1)[colnames(tf_d1) == 'hunting_cluster'] = 'Window ID'
tf_d1 = filter(tf_d1, is.na(`Dive ID`) == 'FALSE' & is.na(`Window ID`) == 'FALSE')


## Model 2 data - presence/absence at dive level

tf_d2 = all_penguins %>%
  group_by(TagID.x, dive_cluster) %>%
  summarise('Dive duration (s)' = first(dive_duration_s),
            'Max dive depth' = max(depth25Hz),
            'Time spent hunting (s)' = sum(States_new == 'Hunt')/25,
            'Prey TRUE/FALSE' = any(PCE %in% 1:7))
colnames(tf_d2)[colnames(tf_d2) == 'TagID.x'] = 'Animal ID'
colnames(tf_d2)[colnames(tf_d2) == 'dive_cluster'] = 'Dive ID'
tf_d2 = filter(tf_d2, is.na(`Dive ID`) == 'FALSE')


# ----------------------------------------------------------------------------------------------------






# Intensity Models Data ----------------------------------------------------------------------------------

## Model 1 data - prey type and number as a function of window size

pce_matrix_long = pivot_longer(data = pce_matrix, cols = 3:9, values_to = 'count', names_to = 'PCE label')
pce_matrix_long$window_size_s = pce_matrix_long$window_size / 25

pce_matrix_long$PCE_new = NA
pce_matrix_long$PCE_new[pce_matrix_long$`PCE label` == 1] <- 'Potential PCE'
pce_matrix_long$PCE_new[pce_matrix_long$`PCE label` == 2] <- "Confirmed PCE"
pce_matrix_long$PCE_new[pce_matrix_long$`PCE label` == 3] <- "Krill (individual)"
pce_matrix_long$PCE_new[pce_matrix_long$`PCE label` == 4] <- "Krill (swarm)"
pce_matrix_long$PCE_new[pce_matrix_long$`PCE label` == 5] <- "Fish (individual)"
pce_matrix_long$PCE_new[pce_matrix_long$`PCE label` == 6] <- "Jellyfish (individual)"
pce_matrix_long$PCE_new[pce_matrix_long$`PCE label` == 7] <- "PCE (other prey)"

i_d1_prejoin = all_penguins %>%
  group_by(uniqueID, TagID.x, dive_cluster, hunting_cluster) %>%
  summarise('Mean window VeDBA' = mean(VeDBA),
            'SD window VeDBA' = sd(VeDBA),
            'Max window depth' = max(depth25Hz))
i_d1_prejoin = left_join(i_d1_prejoin, pce_matrix_long, by = 'uniqueID')

i_d1 = data.frame('Animal ID' = i_d1_prejoin$TagID.x,
                  'Dive ID' = i_d1_prejoin$dive_cluster,
                  'Window ID' = i_d1_prejoin$hunting_cluster,
                  'Window size (s)' = i_d1_prejoin$window_size_s,
                  'Prey type' = i_d1_prejoin$PCE_new,
                  'Prey number' = i_d1_prejoin$count,
                  'Total prey in window' = i_d1_prejoin$all_pce_sum,
                  'Mean window VeDBA' = i_d1_prejoin$`Mean window VeDBA`,
                  'SD window VeDBA' = i_d1_prejoin$`SD window VeDBA`,
                  'Max window depth' = i_d1_prejoin$`Max window depth`)

colnames(i_d1) = gsub('\\.', ' ', colnames(i_d1))
colnames(i_d1)[colnames(i_d1) == 'Window size  s '] = 'Window size (s)'

i_d1 = filter(i_d1, `Total prey in window` > 0 & `Prey number` > 0) # removing windows with no pce and rows where prey count is 0


## Model 1.1 data - simplified version of Model 1 without prey type

i_d1.1_prejoin = pce_matrix %>%
  group_by(uniqueID) %>%
  summarise(window_size_s = first(window_size) / 25,
            all_pce_sum = first(all_pce_sum))

i_d1.1 = all_penguins %>%
  group_by(uniqueID, TagID.x, dive_cluster, hunting_cluster) %>%
  summarise('Mean window VeDBA' = mean(VeDBA),
            'SD window VeDBA' = sd(VeDBA),
            'Max window depth' = max(depth25Hz))

i_d1.1 = left_join(i_d1.1, i_d1.1_prejoin, by = 'uniqueID')

i_d1.1 = i_d1.1[,-1]

colnames(i_d1.1)[colnames(i_d1.1) == 'TagID.x'] = 'Animal ID'
colnames(i_d1.1)[colnames(i_d1.1) == 'dive_cluster'] = 'Dive ID'
colnames(i_d1.1)[colnames(i_d1.1) == 'hunting_cluster'] = 'Window ID'
colnames(i_d1.1)[colnames(i_d1.1) == 'window_size_s'] = 'Window size (s)'
colnames(i_d1.1)[colnames(i_d1.1) == 'all_pce_sum'] = 'Total prey in window'

i_d1.1 = filter(i_d1.1, `Total prey in window` > 0) # removing windows with no pce


## Model 2 data - prey number and group foraging at dive level

i_d2 = all_penguins %>%
  group_by(TagID.x, dive_cluster) %>%
  summarise('Dive duration (s)' = n() / 25,
            'Max dive depth' = max(depth25Hz),
            'Time spent hunting (s)' = sum(States_new == 'Hunt') / 25,
            'Total prey' = sum(PCE %in% 1:7),
            'Mixed prey TRUE/FALSE' = n_distinct(PCE[PCE %in% 1:7]) > 1,
            'Group foraging TRUE/FALSE' = any(PCE %in% 8)
  )
colnames(i_d2)[colnames(i_d2) == 'TagID.x'] = 'Animal ID'
colnames(i_d2)[colnames(i_d2) == 'dive_cluster'] = 'Dive ID'

i_d2 = filter(i_d2, is.na(`Dive ID`) == 'FALSE')


# ----------------------------------------------------------------------------------------------------


write.csv(tf_d1, '/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/tf1.csv')
write.csv(tf_d2, '/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/tf2.csv')
write.csv(i_d1, '/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i1.csv')
write.csv(i_d1.1, '/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i1_1.csv')
write.csv(i_d2, '/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i2.csv')



# Post-formatting analyses ---------------------------------------------------------------------------


tf_d1 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/tf1.csv')
tf_d2 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/tf2.csv')
i_d1 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i1.csv')
i_d1.1 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i1_1.csv')
i_d2 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i2.csv')


# TRUE/FALSE Models Plots ----------------------------------------------------------------------------

# Model 1 plots

# count of windows with and without prey inside

ggplot() + 
  geom_bar(data = tf_d1, aes(x = `Prey TRUE/FALSE`)) +
  theme_classic()

# distribution of window size

hist(tf_d1$`Window size (s)`, breaks = 100, main = '', xlab = 'Window size (s)')

# prey presence as a function of window size

ggplot() +
  geom_boxplot(data = tf_d1, aes(x = `Prey TRUE/FALSE`, y = `Window size (s)`)) +
  theme_classic()

# prey presence as a function of VeDBA and SD VeDBA

ggplot() +
  geom_violin(data = tf_d1, aes(x = `Prey TRUE/FALSE`, y = `Mean window VeDBA`, fill = 'Mean VeDBA')) +
  geom_violin(data = tf_d1, aes(x = `Prey TRUE/FALSE`, y = `SD window VeDBA`, fill = 'SD VeDBA'), alpha = 0.3) +
  scale_fill_manual(values = c('Mean VeDBA' = 'skyblue', 'SD VeDBA' = 'orange'), name = '') +
  ylab('') +
  theme_classic()

# prey presence as a function of depth

ggplot() +
  geom_boxplot(data = tf_d1, aes(x = `Prey TRUE/FALSE`, y = `Max window depth`, fill = `Animal ID`)) +
  theme_classic() +
  ylab('Max window depth (m)') +
  theme(legend.position = 'FALSE')


# sd vedba as a function of window size

ggplot() +
  geom_point(data = tf_d1, aes(x = `Window size (s)`, y = `SD window VeDBA`), cex = 1, pch = 19)




# Model 2 plots


# count of dives with and without prey inside

ggplot() + 
  geom_bar(data = tf_d2, aes(x = `Prey TRUE/FALSE`)) +
  theme_classic()


# prey presence as a function of time spent hunting - shows model is predicting overall hunting behaviour well
# i.e., when there's prey, the penguin actively hunts, creating large windows
# when there's no prey, the penguin is still using dynamic movement and looking for prey, making short 'hunting' phases

ggplot() +
  geom_boxplot(data = tf_d2, aes(x = `Prey TRUE/FALSE`, y = `Time spent hunting (s)`), fill = 'azure1',colour = 'azure4') +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))
ggsave('/Volumes/FRASER USB/MRes/Data/2022/Validation/TRUE:FALSE/Model 2/Prey presence vs time spent hunting.png', width = 8, height = 6)


# prey presence as a function of max dive depth

ggplot() +
  geom_boxplot(data = tf_d2, aes(x = `Prey TRUE/FALSE`, y = `Max dive depth`)) +
  theme_classic()


# prey presence as a function of dive duration

ggplot() +
  geom_boxplot(data = tf_d2, aes(x = `Prey TRUE/FALSE`, y = `Dive duration (s)`)) +
  theme_classic()


# dive depth as a function of dive duration - two clear clusters

ggplot() +
  geom_point(data = tf_d2, aes(x = `Dive duration (s)`, y = `Max dive depth`, color = `Prey TRUE/FALSE`)) +
  labs(color = 'Prey encountered') +
  theme_classic()


# time spend hunting as a function of dive duration
# this not being linear is good - the model doesn't scale cluster size with dive time (i.e., make clusters at a fixed rate)

ggplot() +
  geom_point(data = tf_d2, aes(x = `Dive duration (s)`, y = `Time spent hunting (s)`, color = `Prey TRUE/FALSE`)) +
  labs(color = 'Prey encountered') +
  theme_classic()




# Intensity Models Plots -----------------------------------------------------------------------------


# Model 1

# total prey as a function of window size

ggplot() +
  geom_point(data = i_d1, aes(x = `Window size (s)`, y = `Total prey in window`), cex = 0.5, pch = 19) +
  theme_classic()

# prey type and number as a function of window size

ggplot() +
  geom_point(data = i_d1, aes(x = `Window size (s)`, y = `Prey number`, color = `Prey type`), cex = 0.5, pch = 19) +
  facet_wrap(~ `Prey type`, scales = 'fixed') +
  theme_classic() +
  theme(legend.position = 'FALSE')


# mean and sd vedba vs total prey bubble plot: this makes a really interesting pattern - optimal sd and mean vedba combination?

ggplot() +
  geom_point(data = i_d1, aes(x = `Mean window VeDBA`, y = `SD window VeDBA`, cex = `Total prey in window`)) +
  theme_classic()



# prey type as a function of mean and sd VeDBA

ggplot() +
  geom_boxplot(data = i_d1, aes(x = `Prey type`, y = `Mean window VeDBA`, fill = 'Mean VeDBA'), cex = 0.5, pch = 19) +
  geom_boxplot(data = i_d1, aes(x = `Prey type`, y = `SD window VeDBA`, fill = 'SD VeDBA'), cex = 0.5, pch = 19) +
  scale_fill_manual(values = c('SD VeDBA' = 'orange', 'Mean VeDBA'= 'skyblue'), name = '') +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# prey type as a function of max window depth

ggplot() +
  geom_boxplot(data = i_d1, aes(x = `Prey type`, y = `Max window depth`), cex = 0.5, pch = 19) +
  xlab('') +
  ylab('Max window depth (m)') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# total prey as a function of mean and sd VeDBA

ggplot(data = i_d1, aes(y = `SD window VeDBA`, x = `Total prey in window`)) +
  geom_point(cex = 0.5, pch = 19) +
  theme_classic()
ggplot(data = i_d1, aes(x = `Total prey in window`, y = `Mean window VeDBA`)) +
  geom_point(cex = 0.5, pch = 19) +
  theme_classic()


# total prey as a function of max window depth

ggplot(data = i_d1, aes(y = `Total prey in window`, x = `Max window depth`)) +
  geom_point(cex = 0.5, pch = 19) +
  theme_classic()






# Model 2 - make sure to filter so that prey count is above 0


# bubble plot of prey count per dive when prey count > 0, with time spent hunting as a function of dive duration
ggplot() +
  geom_point(data = filter(i_d2, `Total prey` > 0), aes(x = `Dive duration (s)`, y = `Time spent hunting (s)`, cex = `Total prey`)) +
  theme_classic()


# same as above but with group foraging separated - more energetically demanding when group foraging / competition present?
ggplot() +
  geom_point(data = filter(i_d2, `Total prey` > 0), aes(x = `Dive duration (s)`, y = `Time spent hunting (s)`, cex = `Total prey`, colour = `Group foraging TRUE/FALSE`)) +
  labs(colour = 'Group foraging') +
  theme_classic()



plot(i_d2$`Total prey`, i_d2$`Group foraging TRUE/FALSE`)
plot(i_d2$`Dive duration (s)`, i_d2$`Group foraging TRUE/FALSE`)
plot(i_d2$`Time spent hunting (s)`, i_d2$`Group foraging TRUE/FALSE`)


# time spent hunting / dive duration (rough time-based energy expenditure) vs group foraging TRUE/FALSE
ggplot() +
  geom_boxplot(data = filter(i_d2, `Total prey` > 0), aes(x = `Group foraging TRUE/FALSE`, y = `Time spent hunting (s)`/`Dive duration (s)`)) +
  ylab('Time spent hunting / dive duration') +
  theme_classic()


plot((filter(i_d2, `Total prey` > 0)$`Time spent hunting (s)`/filter(i_d2, `Total prey` > 0)$`Dive duration (s)`),
     filter(i_d2, `Total prey` > 0)$`Group foraging TRUE/FALSE`)




# looking at vedba signatures for group foraging true/false
# making a quick dataset
group_foraging_vedba = i_d1
group_foraging_vedba$`Group foraging TRUE/FALSE` = 'NA'

for (i in 1:nrow(group_foraging_vedba)) {
  for (j in 1:nrow(i_d2)) {
    if (isTRUE(group_foraging_vedba$`Animal ID`[i] == i_d2$`Animal ID`[j] && group_foraging_vedba$`Dive ID`[i] == i_d2$`Dive ID`[j])) {
      group_foraging_vedba$`Group foraging TRUE/FALSE`[i] = i_d2$`Group foraging TRUE/FALSE`[j]
    }
  }
}


# this is quite interesting - sd vedba stays the same (no more dynamic when group foraging), but mean vedba is slightly higher when group foraging
# suggests that overall energy expenditure is higher when hunting in a group
ggplot() +
  geom_violin(data = group_foraging_vedba, aes(x = `Group foraging TRUE/FALSE`, y = `Mean window VeDBA`, fill = 'Mean VeDBA')) +
  geom_violin(data = group_foraging_vedba, aes(x = `Group foraging TRUE/FALSE`, y = `SD window VeDBA`, fill = 'SD VeDBA'), alpha = 0.3) +
  scale_fill_manual(values = c('Mean VeDBA' = 'skyblue', 'SD VeDBA' = 'orange'), name = '') +
  ylab('') +
  theme_classic()

# comparing just mean vedba
ggplot() +
  geom_boxplot(data = group_foraging_vedba, aes(x = `Group foraging TRUE/FALSE`, y = `Mean window VeDBA`)) +
  ylab('Mean window VeDBA') +
  theme_classic()

t.test(`Mean window VeDBA` ~ `Group foraging TRUE/FALSE`, data = group_foraging_vedba) # significant difference
# about 8% increase in mean vedba when group foraging
t.test(`SD window VeDBA` ~ `Group foraging TRUE/FALSE`, data = group_foraging_vedba) # not significant difference
# actually reduced SD VeDBA when group foraging


# is this because more prey when group foraging?
# surely you would expect sd vedba to increase too if this was the case? it's more correlative with prey count than mean vedba (below)
ggplot() +
  geom_boxplot(data = group_foraging_vedba, aes(x = `Group foraging TRUE/FALSE`, y = `Total prey in window`)) +
  theme_classic()
t.test(`Total prey in window` ~ `Group foraging TRUE/FALSE`, data = group_foraging_vedba) # not as strong but still more prey when group foraging


# VeDBA vs total prey count - really interesting relationship
ggplot() +
  geom_point(data = group_foraging_vedba, aes(x = `Total prey in window`, y = `SD window VeDBA`)) +
  theme_classic()
cor.test(group_foraging_vedba$`Total prey in window`, group_foraging_vedba$`Mean window VeDBA`)
cor.test(group_foraging_vedba$`Total prey in window`, group_foraging_vedba$`SD window VeDBA`) # SD VeDBA actually more correlative than mean and more significant

# prey capture is tied more to behavioral variability / bursts (SD VeDBA), not just elevated baseline activity (mean VeDBA)
# group foraging demands a higher baseline of energy expenditure, like 'overhead costs' of foraging in a group


(2.546406 - 2.240563) / 2.546406






# Continuous response variable distributions ---------------------------------------

# Intensity dataset 1 ----

# Mean VeDBA - bell shaped (Gaussian)
ggplot() +
  geom_density(data = i_d1, aes(x = `Mean window VeDBA`)) +
  theme_classic()

# SD VeDBA - right skewed (gamma)
ggplot() +
  geom_density(data = i_d1, aes(x = `SD window VeDBA`)) +
  theme_classic()





