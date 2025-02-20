# packages
library(dplyr)
library(tidyverse)
library(ggplot2)

# set working directory
setwd('C:/Users/Ben/Desktop/UMD/Research/SequencingData/SequencingData/')

# load data
data = read.csv('sequencing_files.csv')

# for the sake of read counts, removing all R2's since they're identical to R1
data = data[data$run == 'R1',]

# combine between dates
data = data %>% group_by(site, trimmed) %>%
  summarize(total_sequences = sum(total_sequences))

# bar plot comparing trimmed and un-trimmed
p = ggplot(data) +
  geom_bar(mapping = aes(x = site, y = total_sequences, fill = trimmed),
           stat = 'identity', position = 'dodge') +
  coord_flip() +
  theme_bw(base_size = 18)

ggsave(p, filename = 'trimmed_vs_untrimmed_raw_read_counts.png', width = 10, height = 8, units = 'in')

# calculating the percentage and raw difference between trimmed and untrimmed read counts
trimmed_vs_untrimmed = data %>%
  pivot_wider(names_from = trimmed, values_from = total_sequences) %>%
  rename(untrimmed = `FALSE`, trimmed = `TRUE`) %>%
  mutate(difference = untrimmed - trimmed,
         pct_difference = (difference/untrimmed)*100)

# raw difference
p = ggplot(trimmed_vs_untrimmed) +
  geom_bar(mapping = aes(x = site, y = difference),
           stat = 'identity') +
  coord_flip() +
  theme_bw(base_size = 18)

ggsave(p, filename = 'trimmed_vs_untrimmed_raw_difference.png', width = 10, height = 8, units = 'in')


# pct difference
p = ggplot(trimmed_vs_untrimmed) +
  geom_bar(mapping = aes(x = site, y = pct_difference),
           stat = 'identity') +
  coord_flip() +
  theme_bw(base_size = 18)

ggsave(p, filename = 'trimmed_vs_untrimmed_pct_difference.png', width = 10, height = 8, units = 'in')

# load data again
data = read.csv('sequencing_files.csv')

# for the sake of read counts, removing all R2's since they're identical to R1
data = data[data$run == 'R1',]

# also now judy looking at trimmed data
data = data[data$trimmed == 'TRUE',]

# bar plot comparing runs
p = ggplot(data) +
  geom_bar(mapping = aes(x = site, y = total_sequences, fill = as.character(date)),
           stat = 'identity', position = 'stack') +
  coord_flip() +
  theme_bw(base_size = 18)

ggsave(p, filename = 'trimmed_read_counts_across_runs.png', width = 10, height = 8, units = 'in')
