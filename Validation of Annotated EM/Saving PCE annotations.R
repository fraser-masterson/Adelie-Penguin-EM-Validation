
library(readr)
library(dplyr)

penguin = 'C120'
vid = '8'

out_file = read_csv(paste0('/Volumes/FRASER USB/MRes/Data/2022/EM results classified/', penguin, '_25Hz_EM_classifications_OUT.csv', sep = ''))
em_file = read_csv(paste0('/Volumes/FRASER USB/MRes/Data/2022/EM results classified/', penguin, '_25Hz_EM_classifications.csv', sep = ''))
out_file$Timestamp = em_file$Timestamp

pce_events = filter(out_file, PCE > 0)

write_csv(pce_events, file = paste('/Volumes/FRASER USB/MRes/Data/2022/PCE events/', penguin, '_PCE_', vid, '.csv', sep = ''))

out_file_new = out_file
out_file_new$PCE = 0
out_file_new = out_file_new[-6]

write_csv(out_file_new, file = paste0('/Volumes/FRASER USB/MRes/Data/2022/EM results classified/', penguin, '_25Hz_EM_classifications_OUT.csv', sep = '')) 
