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
penguin_files = list.files('data/em_files_with_pce')

for (i in 1:length(penguin_files)) {
  current_penguin = str_sub(penguin_files[i], end = -5)
  assign(current_penguin, read_csv(paste('data/em_files_with_pce/', penguin_files[i], sep = ''))) 
}


# make stacked penguin file
penguins = c('C42', 'C52', 'C56', 'C61', 'C65', 'C67', 'C68', 'C120')
all_penguins_unfiltered = bind_rows(lapply(penguins, get))


# remove low visibility dives 
all_penguins = filter(all_penguins_unfiltered, low_visibility == 'FALSE')


# buffer points by 11 frames and assign closest touching cluster with TRUE (presence/absence)
# also make matrix of pce labels for each cluster before and after buffering

# make 'closest_before' and 'closest_after' column, which is the distance in rows to nearest prediction window 
# distance calculation

# flag hunting rows, and give every row a sequential index plus a unique tag-dive-window ID -
# distances for buffering are calculated positionally using row_number, and uniqueID keeps
# grouping/joining operations from mixing rows across different dives or animals
all_penguins$is_hunting = if_else(all_penguins$States_new == 'Hunt', TRUE, FALSE)

all_penguins$row_number = seq(1, nrow(all_penguins), 1)
all_penguins$uniqueID = paste(all_penguins$TagID.x, all_penguins$dive_cluster, all_penguins$hunting_cluster, sep = '_') # prevents mixing in dplyr grouping

# isolate non-validated pce rows that fall outside a hunting cluster - these are the candidates
# for reassignment to a nearby hunting window during buffering
all_non_validated_pce = filter(all_penguins, PCE %in% 1:7 & is_hunting == FALSE)
all_non_validated_pce$closest_before = NA
all_non_validated_pce$closest_after = NA

hunting_rows = all_penguins$row_number[all_penguins$is_hunting]

# for each non-validated pce row, find the nearest hunting row immediately before and after it by row position
for (i in 1:nrow(all_non_validated_pce)) {
  current_row = all_non_validated_pce$row_number[i]
  
  before_rows = hunting_rows[hunting_rows < current_row]
  after_rows = hunting_rows[hunting_rows > current_row]
  
  all_non_validated_pce$closest_before[i] = before_rows[which.min(current_row - before_rows)]
  all_non_validated_pce$closest_after[i] = after_rows[which.min(after_rows - current_row)]
  
}

# convert nearest-row positions into row distances, and take whichever direction is closer
all_non_validated_pce$distance_before = abs(all_non_validated_pce$row_number - all_non_validated_pce$closest_before)
all_non_validated_pce$distance_after = abs(all_non_validated_pce$row_number - all_non_validated_pce$closest_after)
all_non_validated_pce$closest_distance = pmin(all_non_validated_pce$distance_before, all_non_validated_pce$distance_after)

all_non_validated_pce_summary = all_non_validated_pce %>%
  dplyr::select(row_number, closest_before, closest_after, distance_before, distance_after)

all_penguins = left_join(all_penguins, all_non_validated_pce_summary, by = 'row_number')


# flag, for every hunting/non-hunting cluster, whether a pce occurred anywhere within it (pre-buffer)
match = all_penguins %>%
  group_by(is_hunting, uniqueID) %>%
  summarise(pce_occurred = any(PCE %in% 1:7))
match$pce_occurred[1] = NA # first row belongs to the NA uniqueID group - blank it before that row is dropped
match = match[-1] # drop is_hunting now grouping is done, leaving uniqueID + pce_occurred for the join

all_penguins = left_join(all_penguins, match, by = 'uniqueID')

# work out whether each non-validated pce sits closer to its preceding or following hunting window,
# then flag which of those fall within the buffering threshold (11 rows ~ 90th percentile of distances)
all_non_validated_pce$before_after_closest = if_else(all_non_validated_pce$distance_before < all_non_validated_pce$distance_after, 'BEFORE', 'AFTER')
all_non_validated_pce$before_after_closest = if_else(all_non_validated_pce$distance_before == all_non_validated_pce$distance_after, 'BOTH', all_non_validated_pce$before_after_closest)

threshold = 11 # 90% quantile in distribution
all_non_validated_pce$below_threshold = if_else(all_non_validated_pce$closest_distance < threshold, TRUE, FALSE)

# build the pre-buffer matrix of summed pce label counts per cluster, then copy it as the
# starting point for the post-buffer version, which gets incremented in the loop below
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

# reassign each below-threshold, non-validated pce to its nearest hunting cluster: flip that
# cluster's row to TRUE, and add one to the matching pce label count in the matrix. BEFORE/AFTER
# cases go to the single nearest cluster; BOTH cases (equidistant) flag both clusters as TRUE but
# randomly pick one to receive the count, to avoid double-counting a single pce event
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


# add dive duration and window size columns (both in seconds, derived from row counts at 25 Hz)
all_penguins$uniquediveID = paste(all_penguins$TagID.x, all_penguins$dive_cluster, sep = '_')
dive_dur = all_penguins %>%
  group_by(uniquediveID) %>%
  summarise(dive_duration_s = n()/25)
all_penguins = left_join(all_penguins, dive_dur, by = 'uniquediveID')
all_penguins$window_size_s = all_penguins$window_size / 25

# add max dive depth column, taken as the deepest 25 Hz depth reading within each dive
dive_depth = all_penguins %>%
  group_by(uniquediveID) %>%
  summarise(max_dive_depth = max(depth25Hz))
all_penguins = left_join(all_penguins, dive_depth, by = 'uniquediveID')

# sum across all seven pce labels to get total prey capture events per window (post-buffer)
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



# OBJECT LEGEND ================================================================
#
# all_penguins: main dataset with all variables, unique IDs for each window (penguin_dive_window), and pce lables pre- and post-buffer
# all_penguins$uniqueID: concatenated penguinID, diveID, windowID - to be used at window-level analyses
# all_penguins$uniquediveID: concatenated penguinID, diveID - to be used at dive-level analyses
#
# pce_matrix: post-buffer matrix with prey type and count for every window and every penguin, 'all_pce_sum' is total pce for that window
# match_buffered: TRUE/FALSE index with every window and every penguin, summarising if any pce happened within a given window post-buffer
#
# ==============================================================================






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
tf_d1 = filter(tf_d1, is.na(`Dive ID`) == 'FALSE' & is.na(`Window ID`) == 'FALSE') # drop rows falling outside a dive/window cluster


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






# Intensity Models Data ------------------------------------------------------------------------------

## Model 1 data - prey type and number as a function of window size

# reshape the matrix to long format (one row per window per pce label), then translate
# the numeric pce labels into descriptive prey categories for plotting/reporting
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

# bring in window-level energetics and depth, then join to the long-format prey counts
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

colnames(i_d1) = gsub('\\.', ' ', colnames(i_d1)) # data.frame() silently swaps spaces for dots in names - undo that
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

i_d1.1 = i_d1.1[,-1] # drop uniqueID now the join's done

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






# Saving model datasets --------------------------------------------------------------------------------

# export the five model-ready datasets for downstream GLMM/GAM fitting
write.csv(tf_d1, 'output/model_datasets/tf1.csv')
write.csv(tf_d2, 'output/model_datasets/tf2.csv')
write.csv(i_d1, 'output/model_datasets/i1.csv')
write.csv(i_d1.1, 'output/model_datasets/i1_1.csv')
write.csv(i_d2, 'output/model_datasets/i2.csv')
