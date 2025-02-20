# load data
data = read.csv('C:/Users/Ben/Desktop/UMD/Research/SequencingData/sequencing_files.csv')

# for the sake of read counts, removing all R2's since they're identical to R1
data = data[data$run == 'R1',]

# combine between dates
# dplyr
library(dplyr)
library(tidyverse)

data = data %>% group_by(site, trimmed) %>% summarize(total_sequences = sum(total_sequences))

# ggplot
library(ggplot2)

# bar plot comparing trimmed and un-trimmed
ggplot(data) +
  geom_bar(mapping = aes(x = site, y = total_sequences, fill = trimmed),
           stat = 'identity', position = 'dodge') +
  coord_flip() +
  theme_bw(base_size = 18)

# calculating the percentage and raw difference between trimmed and untrimmed read counts
trimmed_vs_untrimmed = data %>%
  pivot_wider(names_from = trimmed, values_from = total_sequences) %>%
  rename(untrimmed = `FALSE`, trimmed = `TRUE`) %>%
  mutate(difference = untrimmed - trimmed,
         pct_difference = (difference/untrimmed)*100)

# raw difference
ggplot(trimmed_vs_untrimmed) +
  geom_bar(mapping = aes(x = site, y = difference),
           stat = 'identity') +
  coord_flip() +
  theme_bw(base_size = 18)

# pct difference
ggplot(trimmed_vs_untrimmed) +
  geom_bar(mapping = aes(x = site, y = pct_difference),
           stat = 'identity') +
  coord_flip() +
  theme_bw(base_size = 18)

# load data again
data = read.csv('C:/Users/Ben/Desktop/UMD/Research/SequencingData/sequencing_files.csv')

# for the sake of read counts, removing all R2's since they're identical to R1
data = data[data$run == 'R1',]

# also now judy looking at trimmed data
data = data[data$trimmed == 'TRUE',]

# bar plot comparing runs
ggplot(data) +
  geom_bar(mapping = aes(x = site, y = total_sequences, fill = as.character(date)),
           stat = 'identity', position = 'stack') +
  coord_flip() +
  theme_bw(base_size = 18)
