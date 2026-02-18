library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1️⃣ Set file path
file_path <- "/home/user/Desktop/Morgans struuff/CosmosID/Results/summary_tables.xlsx"

# 2️⃣ Define the sample order you want on the x-axis
sample_order <- c("3MT","2MT1","2MT2","3MM","2MM2","2MM1","1MB","1MNB","2MB1","2MB2","3MB")

# 3️⃣ Read sheet 3, calculate relative abundance, and plot heatmap
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
  mutate(RelAbundance = Abundance / sum(Abundance)) %>%
  ungroup() %>%
  mutate(Sample = factor(Sample, levels = sample_order)) %>%
  ggplot(aes(x = Sample, y = Phylum, fill = RelAbundance)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(
    low = "white",
    high = "darkblue",
    name = "Relative\nAbundance",
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  labs(
    x = "Sample",
    y = "Phylum",
    title = "Phylum Relative Abundance Heatmap"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank()
  )
