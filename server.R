##############################################
### COMBINED - lightweight online app
##############################################
### 2026-10-12, Anja - updating COMBINED.X data and adding all gsva scores
### 2026-07-10, Anja - adding transcript level data
### 2026-03-26, Anja - functionality improvements, reactive y axis log transform
### 2026-02-19, Anja - data update to COMBINED_2026-02-19 data.version and using TPM
### 2025-07-21, Anja - updated data for MR49F, PUCA and C42BITX samples
### 2025-07-13, Anja - embellishments
### 2025-07-10, Anja - first version of the combined xeno and culture data app

source("global.R", local = TRUE)

### Define server logic
##############################################
server <- function(input, output, session) {

  
    ### Radio Button observer
    ##############################################
    observeEvent(input$model_group, {
      
      if (input$model_group == "all") {
        updateCheckboxGroupInput(
          session, "model_select",
          selected = models
        )
      }
      
      if (input$model_group == "all.prostate") {
        updateCheckboxGroupInput(
          session, "model_select",
          selected = models[meta.data$tissue == "prostate"]
        )
      }
      
      if (input$model_group == "culture") {
        updateCheckboxGroupInput(
          session, "model_select",
          selected = models[meta.data$source == "in.vitro"]
        )
      }
      
      if (input$model_group == "xeno") {
        updateCheckboxGroupInput(
          session, "model_select",
          selected = models[meta.data$source == "in.vivo"]
        )
      }
      
      if (input$model_group == "nepc") {
        updateCheckboxGroupInput(
          session, "model_select",
          selected = models[meta.data$mcrpc.subtype == "NEPC"]
        )
      }
      
      # "custom" means: do nothing, user selects manually
      if (input$model_group == "custom") {
        updateCheckboxGroupInput(
          session, "model_select",
          selected = c("LNCAP")
        )
      }
      
    })
    
  
   ### Model.Overview
   ##############################################
   output$scatter <- renderPlotly({
      ggplotly(
         ggplot(meta.data, aes(x=`Public-Labrecque.2019.AR-ALL.8`, y=`Public-Labrecque.2019.NEURO1-UP.8`,
                               col=mcrpc.subtype, #shape=source.2,
                               text=paste(model,source.2,cancer.site,mcrpc.subtype,mcrpc.anno,sep='\n'))) +
          geom_vline(xintercept=0, linetype='dashed', col='grey70') +
          geom_hline(yintercept=0, linetype='dashed', col='grey70') +
          geom_point(size=5,alpha=0.5) +
          geom_text(aes(label=model),size=2,col='black') +
          theme_minimal(base_size=11) +
          scale_color_manual(values = labrecque.colors) + #labrecque colors
          labs(title='Labrecque.mCRPC.subtypes - DRAFT'),tooltip="text"
       )
    })

  ### user input
  ##############################################
  observeEvent(input$plot_button, {
    req(input$gene_input)
    goi <- input$gene_input
    entry.g <- colnames(combined.data)[grep(paste0('^',goi,'\\|'),colnames(combined.data))] #symbol to colname
    entry.tx <- colnames(combined.data)[grep(paste0('^',goi,'-[0-9]+'),colnames(combined.data))]
 
    
    ### Bar.Charts
    ##############################################
    # gather gene data
    gn.data <- combined.data[, c(entry.g,"model"), drop = FALSE] #subsetting the goi first generates a small subset
    gn.data <- gn.data[gn.data$model %in% input$model_select, ] #then subset the selected model(s)
    gn.data <- aggregate(. ~ model, data = gn.data, mean)
    gn.data <- reshape2::melt(gn.data, id.vars = 'model', variable.name = 'gene', value.name = "tpm")
    #gn.data$log2.tpm <- log2(gn.data$tpm + 1) #log proportions are deceiving
    subtypes<-unique(combined.data[,c('model','source.2','mcrpc.subtype')])
    gn.data<-merge(gn.data, subtypes)
    gc()
    
    # genes stack chart
    output$stacked_bar.g <- renderPlotly({
      options(scipen=999)
      ggplotly(
        ggplot(gn.data, aes(x=reorder(model, tpm), y=tpm, fill=mcrpc.subtype, text=source.2)) + #on purpose NOT using log
          geom_col() +
          theme_minimal(base_size=8) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = labrecque.colors) + #labrecque colors
          labs(x=NULL, fill=NULL, title=paste0(goi,' - average.GENE.expression - ranked'))
      )
    ### native plotly solution - doesn't reduce mem usage
    })   
    
    # gather transcript data
    tx.data <- combined.data[, c(entry.tx,"model"), drop = FALSE] #subsetting the goi first generates a small subset
    tx.data <- tx.data[tx.data$model %in% input$model_select, ] #then subset the selected model(s)
    tx.data <- aggregate(. ~ model, data = tx.data, mean)
    tx.data <- reshape2::melt(tx.data, id.vars = 'model', variable.name = 'tv', value.name = "tpm")
    #tx.data$log2.tpm <- log2(tx.data$tpm + 1) #log proportions are deceiving
    subtypes<-unique(combined.data[,c('model','source.2','mcrpc.subtype')])
    tx.data<-merge(tx.data, subtypes)
    tx.data$tpm <- round(tx.data$tpm,3)
    tx.data$prop <- tx.data$tpm #on purpose NOT using log
    gc()
    
    # transcripts stack chart
    output$stacked_bar.tx <- renderPlotly({
      options(scipen=999)
      ggplotly(
        ggplot(tx.data, aes(x=reorder(model, tpm), y=tpm, fill=tv, text=source.2)) + #on purpose NOT using log
          geom_bar(stat="identity", position="stack") +
          theme_minimal(base_size=8) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = polychrome) + 
          labs(x=NULL, fill=NULL, title=paste0(goi,' - average.TRANSCRIPT.expression - ranked'))
      )
    })
    
    # transcript proportions - props calculated on log scale are deceiving, best stick to lin values
    output$percentage_stack.tx <- renderPlotly({
      options(scipen=999)
      ggplotly(
        ggplot(tx.data, aes(x=reorder(model, tpm), y=prop, fill=tv, text=paste('tpm:',tpm))) + #on purpose NOT using log
          geom_bar(stat="identity", position="fill") +
          theme_minimal(base_size=8) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = polychrome) + 
          labs(x=NULL, fill=NULL, title=paste0(goi,' - TRANSCRIPT.variant.proportions'))
      )
    }) 
    
    ### Box.Plots
    ##############################################
    bp.data <- combined.data[, c(entry.g,"model","mcrpc.subtype","source.2","Sample"), drop = FALSE] #subsetting the goi first generates a small subset
    bp.data <- bp.data[bp.data$model %in% input$model_select, ]

    ## boxplots by model
    output$boxplot.model1 <- renderPlotly({
      options(scipen=999)
      p<-ggplotly(
         ggplot(bp.data, aes(x=model, y=!!ensym(entry.g),
           text=paste0(Sample,"\ntpm: ",!!ensym(entry.g),"\nmodel: ",model,"\nmcrpc.subtype: ",mcrpc.subtype,"\n",source.2))) +
           geom_boxplot(lwd=0.2, fatten=2, outlier.shape=NA, fill='grey95') +
           geom_jitter(aes(col=mcrpc.subtype), width=0.15, height=0, size=0.6) +
           coord_cartesian(ylim = c(0, NA)) +
           theme_minimal(base_size=8) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           scale_color_manual(values = labrecque.colors) +
           guides(color = guide_legend(override.aes = list(size = 5))) + #not honored by ggplotly :(
           (if (input$yscale_model == "log2") scale_y_continuous(trans = "pseudo_log", breaks = scales::pretty_breaks(n = 7))) +
           labs(x=NULL, y="tpm", title=paste0(entry.g)), tooltip='text'
      )
      p$x$data[[1]]$marker$opacity = 0
      p
    }) 

    ## boxplots by subtype
    output$boxplot.subtype1 <- renderPlotly({
      options(scipen=0)
      q<-ggplotly(
         ggplot(bp.data[bp.data$mcrpc.subtype!='non.PCa',], aes(x=mcrpc.subtype, y=!!ensym(entry.g)))+ #, text=Sample)) + #text confuses stats
            geom_boxplot(lwd=0.2, fatten=2, outlier.shape=NA, fill='grey95') +
            geom_jitter(aes(col=mcrpc.subtype), width=0.15, height=0, size=0.6) +
            coord_cartesian(ylim = c(0, NA)) +
            theme_minimal(base_size=12) +
            theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
            stat_compare_means(label.x=1.5)+
            scale_color_manual(values = labrecque.colors) + 
            (if (input$yscale_subtype == "log2") scale_y_continuous(trans = "pseudo_log", breaks = scales::pretty_breaks(n = 7))) +
            labs(x=NULL, y="tpm", title=paste0(entry.g))
      )
      q$x$data[[1]]$marker$opacity = 0
      q
    })
  })
    
    ## boxplots gsva scores - outside observe event
    output$boxplot.gsva <- renderPlotly({
      req(input$gsva_input)
      gsva.set <- input$gsva_input
      options(scipen=0)
      ggplotly(
        ggplot(gsva.scores[gsva.scores$model %in% input$model_select,], aes(x=model, y=!!ensym(gsva.set),
                              fill=mcrpc.subtype, 
                              text=paste(model,source.2,cancer.site,mcrpc.subtype,mcrpc.anno,sep='\n'))) +
          geom_col()+
          theme_minimal(base_size=8) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = labrecque.colors) + 
          labs(x=NULL, y="gsva.scores", title=paste0(gsva.set))
      )
    })
    
}
##############################################
### END
##############################################
