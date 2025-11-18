
library(dplyr)

file = list.files('/Volumes/FRASER USB/MRes/Data/2022/PCE events', full.names = TRUE)

all_pce = read.csv('/Volumes/FRASER USB/MRes/Data/2022/PCE events/C41_PCE_1.csv')
all_pce = filter(all_pce, PCE == 'NA')

for (i in 1:length(file)) {
  
  current_file = read.csv(file[i])
  
  all_pce = rbind(all_pce, current_file)
  
}


table(all_pce$PCE)
