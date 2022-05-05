library(RColorBrewer)
library(DT)
library(tidyverse)

vibriodata <- readRDS("data/processed/vibrio_tmt_data.rds")
vibriodata <- map2(vibriodata, names(vibriodata), ~cbind(.x, condition = .y)) ## adds list names as a column to the dataframes under column "condition"
strain <- str_split(names(vibriodata), "_", simplify = TRUE)[,1]
vibriodata <- map2(vibriodata, strain, ~cbind(.x, strain = .y)) ## adds the strain information to dataframes

data <- do.call(rbind, vibriodata)
data <- data %>% pivot_longer(cols = contains("min"), names_to = "time", values_to = "abundance") %>%
  mutate(replicate = rep_len(c("A", "B", "C"), nrow(.)))

gene <- "WP_000653994.1"
data_plot <- data %>% filter(Accession == gene)
data_plot <- split(data_plot, data_plot$strain)

ls <- list()
for (i in 1:length(data_plot)) {
  IDs <- names(data_plot)
  p<- ggplot(data_plot[[i]], aes(x=time, y=abundance, color = condition, shape = replicate, group=interaction(replicate,condition))) +
    geom_point(size = 4) +
    geom_line() +
    #geom_hline(yintercept = 100, linetype = "dashed") +
    theme_bw() +
    geom_smooth(aes(group = condition, fill = condition), color = "black", method = "lm") +
    #scale_y_log10() + 
    #facet_wrap(~strain, ncol = 1, scales = "free") + 
    theme(text = element_text(size=15),
          strip.text = element_text(size=15)) +
    ggtitle(paste(gene, IDs[i], sep = " - "))
  ls[[i]] <- p
}
names(ls) <- names(data_plot)
ls[["WT"]]