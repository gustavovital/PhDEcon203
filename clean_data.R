# Packages ====
library(tidyverse)
library(janitor)
library(stringr)
library(readxl)
library(writexl)
library(bibliometrix)

# Get data ====
list.files()
files <- c("savedrecs_1.txt", 
           "savedrecs_2.txt",
           "savedrecs_3.txt",
           "savedrecs_4.txt",
           "savedrecs_5.txt",
           "savedrecs_6.txt",
           "savedrecs_7.txt")

D_raw <- convert2df(files, dbsource = "wos", format = "plaintext")

# Wrangling ====
data <- D_raw
# data %>% glimpse()

data <- data %>%
  filter(
    str_detect(WC, regex("ECONOMIC", ignore_case = TRUE)) |
      str_detect(SC, regex("ECONOMIC", ignore_case = TRUE))
  )
data <- data %>%
  filter(str_detect(DT, regex("ARTICLE", ignore_case = TRUE)))
data <- data %>%
  filter(LA == "ENGLISH")

# remove duplicates
data <- data %>%
  distinct(TI, PY, .keep_all = TRUE)

# create a column to check if the article is relevant in terms of keywords based 
# on the field of interest
data <- data %>% 
  mutate(PY = as.numeric(PY)) %>% 
  filter(PY > 2015) %>% 
  mutate(
    include = if_else(
        str_detect(str_to_lower(DE),
                   "olg|heterogeneous|aiyagari|bewley|krusell") |
        str_detect(str_to_lower(ID),
                   "olg|heterogeneous|aiyagari|bewley|krusell"),
      1, 0
    )
  )

# PRISMA ====
n_identified <- nrow(D_raw)
n_after_filters <- nrow(data)
n_included_auto <- sum(data$include == 1, na.rm = TRUE)
n_excluded_auto <- sum(data$include == 0, na.rm = TRUE)

prisma <- tibble(
  Step = c(
    "Identified (WoS full TXT)",
    "After filtering and deduplication",
    "Included automatically (keywords)",
    "Excluded automatically"
  ),
  Records = c(
    n_identified,
    n_after_filters,
    n_included_auto,
    n_excluded_auto
  )
)

print("===== PRISMA COUNTS =====")
print(prisma)

# Get clean and excluded dataset ====
data_clean <- data %>% filter(include == 1)
# data_excluded <- data %>% filter(include == 0)

save(data_clean, file = "data_clean.RData")

# Save log ====
sink("data_cleaning_log.txt")
cat("===== PRISMA =====\n")
print(prisma)
cat("\n===== Variables kept =====\n")
print(names(data))
cat("\nExecution:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
sink()

cat("\nCleaning completed successfully.",
    nrow(data_clean), "records ready.\n")
