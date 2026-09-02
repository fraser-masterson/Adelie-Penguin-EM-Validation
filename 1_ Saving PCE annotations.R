
library(readr)
library(dplyr)

# Our animal-borne video data files were separated in 30 minute chunks - hence the 'vid' object
# For each video's VANTAGE output (OUT) file, we added the original dataset's timestamp column and filtered out any rows without PCE annotations
# These annotation files all get compiled and added to the EM-labelled accelerometry data in '2_ PCE Analysis.ipynb'

# ID of individual (for indexing VANTAGE 'OUT' file and corresponding EM-labelled accelerometry dataset)
penguin = 'penguinID'

# ID of specific video annotated
vid = '1'

# VANTAGE output (OUT) file of individual
out_file = read_csv(paste0('data/em_labelled_files/EM-classified accelerometry data_OUT.csv', sep = ''))

# Original EM-labelled accelerometry file of individual
em_file = read_csv(paste0('data/em_labelled_files/EM-classified accelerometry data.csv', sep = ''))

# Adding original dataset's timestamp to the output data and filtering to only include annotated timestamps
out_file$Timestamp = em_file$Timestamp
pce_events = filter(out_file, PCE > 0)

# Saving the video's PCE annotations to a unique file
write_csv(pce_events, file = paste('data/pce_files/', penguin, '_PCE_', vid, '.csv', sep = ''))

# Resetting the individual's OUT file for the next video
out_file_new = out_file
out_file_new$PCE = 0
out_file_new = out_file_new[-6]
write_csv(out_file_new, file = paste0('data/em_labelled_files/EM-classified accelerometry data_OUT.csv', sep = '')) 
