vibriodata <- readRDS("data/processed/vibrio_tmt_plot_data.rds")
ref <- vibriodata %>% select(c(1:6)) %>% distinct(Accession, .keep_all = TRUE)


#server code
shinyServer(function(input, output, session) {
  
  #====================================================#
  ## Samar 2021 Shutoff TMT ####
  #====================================================#
  goip_vibrio_tmt <- eventReactive(input$action, {
    gene <- as.character(input$text)
    data_plot <- vibriodata %>% filter(Accession %in% gene)
    data_plot <- split(data_plot, data_plot$strain)
    ls <- list()
    for (i in 1:length(data_plot)) {
      IDs <- names(data_plot)
      p<- ggplot(data_plot[[i]], aes(x=time, y=abundance, color = condition, shape = replicate, group=interaction(replicate,condition))) +
        geom_point(size = 4) +
        geom_line() +
        ylab("Abundance Ratio") +
        #geom_hline(yintercept = 100, linetype = "dashed") +
        theme_bw() +
        geom_smooth(aes(group = condition, fill = condition), color = "black", method = "lm") +
        #scale_y_log10() + 
        #facet_wrap(~strain, ncol = 1, scales = "free") + 
        theme(text = element_text(size=20),
              strip.text = element_text(size=15)) +
        ggtitle(IDs[i])
      ls[[i]] <- p
    }
    names(ls) <- names(data_plot)
    return(ls)
  })
  
  output$WT <- renderPlot({
    goip_vibrio_tmt()[["WT"]]
  })
  
  output$DLON <- renderPlot({
    goip_vibrio_tmt()[["Dlon"]]
  })
  
  output$CLPP <- renderPlot({
    goip_vibrio_tmt()[["ClpP"]]
  })
  
  info <- eventReactive(input$action, {
    ref %>% filter(Accession %in% as.character(input$text)) %>% select(-c("Accession")) %>% t() %>% as.data.frame()
  })
  
  output$gene_info <- DT::renderDataTable({
    info()
  }, rownames = T, colnames = "", options = list(
    dom = "ti"
  ))
  
}
)