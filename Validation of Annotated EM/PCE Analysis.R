
library(readr)
library(dplyr)
library(ggplot2)


penguins = 'C42'

all_files = list.files('/Volumes/FRASER USB/MRes/Data/2022/PCE events')

pce = filter(read_csv(paste('/Volumes/FRASER USB/MRes/Data/2022/PCE events/', all_files[1], sep = '')), is.na(PCE) == 'TRUE')
for (i in 1:length(all_files)) {
  if (isTRUE(grepl(penguin, all_files[i]))) {
    current_pce = read_csv(paste('/Volumes/FRASER USB/MRes/Data/2022/PCE events/', all_files[i], sep = ''))
    pce = rbind(pce, current_pce)
  }
  if (isTRUE(i == length(all_files) && nrow(pce) == 0)) {
    print('No PCE files are present for this penguin.')
  }
}

em_file = read_csv(paste('/Volumes/FRASER USB/MRes/Data/2022/EM results classified/', penguin, '_25Hz_EM_classifications.csv', sep = ''))

em_file_pce = left_join(em_file, pce, by = 'Timestamp')
em_file_pce = filter(em_file_pce, DatesPos > "2018-01-04 00:11:23 UTC" & DatesPos < "2018-01-04 03:41:20 UTC")
em_file_pce$PCE_new = if_else(is.na(em_file_pce$PCE) == 'TRUE', 0, em_file_pce$PCE)

em_file_pce$PCE_extended = em_file_pce$PCE_new
for (i in 1:nrow(em_file_pce)) {
  if (isTRUE(em_file_pce$PCE_new[i] > 0)) {
    start_idx = max(1, i - 12)
    end_idx = min(nrow(em_file_pce), i + 12)
    em_file_pce$PCE_extended[start_idx:end_idx] = em_file_pce$PCE_new[i]
  }
}

hunting = if_else(em_file_pce$States_new == 'Hunt', TRUE, FALSE)
em_file_pce$hunting_cluster[hunting] = cumsum(c(0, diff(hunting)) == -1)[hunting] + 1

pce_clusters = na.omit(unique(em_file_pce$hunting_cluster))

match = em_file_pce %>%
  group_by(hunting_cluster) %>%
  summarise(pce_occurred = any(PCE_extended %in% 1:7))

table(match$pce_occurred)

match = na.omit(match)
em_file_pce = left_join(em_file_pce, match, by = 'hunting_cluster')

em_file_pce <- em_file_pce %>%
  mutate(
    validation = if_else(PCE %in% 1:7 & Hunting == 0,
                         "False negative",
                         if_else(pce_occurred,
                                 "Correctly identified",
                                 "False positive"))
  )

true_positives = nrow(filter(em_file_pce, PCE > 0 & PCE < 8 & Hunting == 10))
false_positives = nrow(filter(match, pce_occurred == 'FALSE'))
false_negatives = nrow(filter(em_file_pce, PCE > 0 & PCE < 8 & Hunting == 0))
accuracy = ((true_positives) / (true_positives + false_positives + false_negatives)) * 100

{
  print(paste('PCE predicted:', nrow(match))) 
  print(paste('PCE observed:', nrow(filter(em_file_pce, PCE > 0 & PCE < 8))))
  print(paste('PCE correctly identified:', true_positives))
  print(paste('False positives:', false_positives))
  print(paste('False negatives:', false_negatives))
  print(paste('Accuracy: ', round(accuracy, digits = 2), '%', sep = ''))
}

ggplot() +
  geom_point(data = em_file_pce[1:5000, ], aes(x = DatesPos, y = depth), cex = 2, pch = 19, col = 'grey') +
  geom_point(data = em_file_pce[1:5000, ], aes(x = DatesPos, y = depth, col = as.factor(validation)), cex = 2, pch = 19) +
  labs(col = '') +
  theme_classic()

# time spent hunting - positive relationship in regression?

window_size = em_file_pce %>%
  group_by(hunting_cluster) %>%
  summarise(pce_window_size = n())
window_size = na.omit(window_size)

hist(filter(window_size, pce_window_size < 500)$pce_window_size, breaks = 70) # loads of small PCE windows, remove to improve accuracy?

em_file_pce = left_join(em_file_pce, window_size, by = 'hunting_cluster')
match = left_join(match, window_size, by = 'hunting_cluster')

em_file_pce2 = filter(em_file_pce, pce_window_size > 2 | is.na(pce_window_size) == 'TRUE')
match2 = filter(match, pce_window_size > 2 | is.na(pce_window_size) == 'TRUE')

true_positives2 = nrow(filter(em_file_pce2, PCE > 0 & PCE < 8 & Hunting == 10))
false_positives2 = nrow(filter(match2, pce_occurred == 'FALSE'))
false_negatives2 = nrow(filter(em_file_pce2, PCE > 0 & PCE < 8 & Hunting == 0))
accuracy2 = ((true_positives2) / (true_positives2 + false_positives2 + false_negatives2)) * 100

{
  print(paste('PCE predicted:', nrow(match2))) 
  print(paste('PCE observed:', nrow(filter(em_file_pce2, PCE > 0 & PCE < 8))))
  print(paste('PCE correctly identified:', true_positives2))
  print(paste('False positives:', false_positives2))
  print(paste('False negatives:', false_negatives2))
  print(paste('Accuracy: ', round(accuracy2, digits = 2), '%', sep = ''))
}


EM_parameters_test = data.frame('Minimum_window_size' = NA,
                                'PCE_predicted' = NA,
                                'PCE_observed' = NA,
                                'PCE_correctly_identified' = NA,
                                'False_positives' = NA,
                                'False_negatives' = NA,
                                'Accuracy' = NA)
colnames(EM_parameters_test) = gsub("_", " ", colnames(EM_parameters_test))

for (i in 0:200) {
  
  em_file_pce_current$Hunting_current = if_else(em_file_pce_current$pce_window_size > i, em_file_pce_current$Hunting, 0)
  
  match2 = em_file_pce_current %>%
    group_by(hunting_cluster) %>%
    summarise(pce_occurred = any(PCE_extended %in% 1:7))
  match2 = left_join(match2, window_size, by = 'hunting_cluster')
  
  match_current = filter(match2, pce_window_size > i | is.na(pce_window_size) == 'TRUE')
  
  true_positivesi = nrow(filter(em_file_pce_current, PCE > 0 & PCE < 8 & Hunting_current == 10))
  false_positivesi = nrow(filter(match_current, pce_occurred == 'FALSE'))
  false_negativesi = nrow(filter(em_file_pce_current, PCE > 0 & PCE < 8 & Hunting_current == 0))
  accuracyi = ((true_positivesi) / (true_positivesi + false_positivesi + false_negativesi)) * 100
  
  EM_parameters_current = data.frame('Minimum_window_size' = i,
                                  'PCE_predicted' = nrow(match_current),
                                  'PCE_observed' = nrow(filter(em_file_pce_current, PCE > 0 & PCE < 8)),
                                  'PCE_correctly_identified' = true_positivesi,
                                  'False_positives' = false_positivesi,
                                  'False_negatives' = false_negativesi,
                                  'Accuracy' = round(accuracyi, digits = 2))
  colnames(EM_parameters_current) = gsub("_", " ", colnames(EM_parameters_current))
  
  EM_parameters_test = rbind(EM_parameters_test, EM_parameters_current)
}

EM_parameters_test = na.omit(EM_parameters_test)

write_csv(EM_parameters_test, file = '/Volumes/FRASER USB/MRes/Data/2022/Validation/C42_EM_parameters_test.csv')

ggplot() +
  geom_hline(yintercept = max(EM_parameters_test$Accuracy), col = 'red', linetype = 'dotted') +
  geom_vline(xintercept = EM_parameters_test$`Minimum window size`[which.max(EM_parameters_test$Accuracy)] / 25, col = 'red', linetype = 'dotted') +
  geom_line(data = EM_parameters_test, aes(x = `Minimum window size`/25, y = Accuracy)) +
  geom_text(aes(x = 7, y = max(EM_parameters_test$Accuracy)*0.95, label = paste('Highest accuracy: ', max(EM_parameters_test$Accuracy), '%', sep = ''))) +
  geom_text(aes(x = 7, y = (max(EM_parameters_test$Accuracy)*0.95)-2, label = paste('Optimal window size: ', EM_parameters_test$`Minimum window size`[which.max(EM_parameters_test$Accuracy)] / 25, 's', sep = ''))) +
  labs(y = 'Model Accuracy (%)', x = 'Minimum window size (seconds)') +
  scale_x_continuous(breaks = seq(0, 8, 0.5)) +
  theme_classic()

ggsave(filename = '/Volumes/FRASER USB/MRes/Data/2022/Validation/C42_minimum_window_accuracy.png')

# look into whether correct id occurs in small prediction spikes

table(pce$PCE)


video1 = as.POSIXct('12:24:25', format = '%H:%M:%OS', tz = 'UTC')
logger1 = as.POSIXct('17:38:11', format = '%H:%M:%OS', tz = 'UTC')
video2 = as.POSIXct('07:55:05', format = '%H:%M:%OS', tz = 'UTC')

difference = difftime(video1, video2, units = 'secs')

logger2 = logger1 - difference
print(logger2)

