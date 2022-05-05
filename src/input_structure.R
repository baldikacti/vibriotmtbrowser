library(tidyverse)
library(readxl)

## Create vibrio_tmt_data.rds ####
txt_ls <- list.files(path = "data/raw", pattern = "*.xlsx", full.names = TRUE)
txt_df <- lapply(txt_ls, function(x) {read_excel(x)})
names(txt_df) <- sub(".xlsx", "", str_split(txt_ls, "_", n = 3, simplify = TRUE)[,3])
txt_df <- lapply(txt_df, function(x) {x %>% select(c("Accession", "Description", "Biological Process", "Cellular Component", "Molecular Function", "KEGG Pathways") | 
                                                     matches("Abundance Ratio:"))})
for (i in 1:length(txt_df)) {
  colnames(txt_df[[i]])[7:length(colnames(txt_df[[i]]))] <- paste0(str_split(colnames(txt_df[[i]])[7:length(colnames(txt_df[[i]]))], ",", simplify = TRUE)[,2], "min")
}
for (i in 1:length(txt_df)) {
  colnames(txt_df[[i]])[7:length(colnames(txt_df[[i]]))] <- sub(" ", "", colnames(txt_df[[i]])[7:length(colnames(txt_df[[i]]))])
}
for (i in 1:length(txt_df)) {
  colnames(txt_df[[i]]) <- sub(" ", "_", colnames(txt_df[[i]]))
}

saveRDS(txt_df, "data/processed/vibrio_tmt_data.rds")

## Create vibrio_tmt_plot_data.rds ####
vibriodata <- readRDS("data/processed/vibrio_tmt_data.rds")
vibriodata <- map2(vibriodata, names(vibriodata), ~cbind(.x, condition = .y)) ## adds list names as a column to the dataframes under column "condition"
strain <- str_split(names(vibriodata), "_", simplify = TRUE)[,1]
vibriodata <- map2(vibriodata, strain, ~cbind(.x, strain = .y)) ## adds the strain information to dataframes

data <- do.call(rbind, vibriodata)
data <- data %>% pivot_longer(cols = contains("min"), names_to = "time", values_to = "abundance") %>%
  mutate(replicate = rep_len(c("A", "B", "C"), nrow(.)))

saveRDS(data, "data/processed/vibrio_tmt_plot_data.rds")
