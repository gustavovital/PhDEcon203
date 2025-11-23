# Get Working Directory ====
setwd("/Users/gustavovital/Documents/PhD/Seminars")
getwd() # "/Users/gustavovital/Documents/PhD/Seminars" (to change it)

# Check files ====
list.files() # (uncomment to check files in dir)

# Run scripts ====
source('clean_data.R')
source('bib_analysis.R')


biblioshiny()