library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# 1️⃣ Set file path
file_path <- "/home/user/Desktop/Morgans struuff/CosmosID/Results/summary_tables.xlsx"

# 2️⃣ Define the sample order you want on the x-axis
sample_order <- c("3MT","2MT1","2MT2","3MM","2MM2","2MM1","1MB","1MNB","2MB1","2MB2","3MB")
#  Read sheet 3 and build stacked bar plot
read_excel(file_path, sheet = 3) %>%
  select(Phylum, all_of(sample_order)) %>%
  pivot_longer(
    cols = -Phylum,
    names_to = "Sample",
    values_to = "Abundance"
  ) %>%
  group_by(Sample, Phylum) %>%
  summarise(Abundance = sum(Abundance), .groups = "drop") %>%
  group_by(Sample) %>%
  mutate(RelAbundance = Abundance / sum(Abundance)) %>%  # relative abundance
  ungroup() %>%
  mutate(Sample = factor(Sample, levels = sample_order)) %>%
  ggplot(aes(x = Sample, y = RelAbundance, fill = Phylum)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    x = "Sample",
    y = "Relative Abundance",
    fill = "Phylum"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
