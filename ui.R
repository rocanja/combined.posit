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

### Define UI
##############################################
ui <- fluidPage(
  
  titlePanel("PCa Model Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      p(em("... please be patient, plots will take a few seconds to render ...")),
      p(em("1 - wait until the 'Model.Overview' plot is showing")),
      p(em("2 - tick the models you want to explore")),
      p(em("3 - enter a gene or  gene.set of interest, then click 'generate.plots'")),
      p(em("4 - select a tab and give it a few seconds for the plots to show")),
      hr(),  
      textInput("gene_input", "Enter one Human Gene Symbol:", value = ""),
      selectizeInput("gsva_input", "... or search for a gene.set:", choices=gene.sets, selected="CTen.Prostate",
                     options=list(placeholder="Search GSVA signatures...", maxOptions=20, allowEmptyOption=TRUE)),
      actionButton("plot_button", "generate.plots"),
      hr(),  
      #h4("Data Set Summary"),
      p("This is a pooled data set of 477 baseline xenograft and in.vitro culture samples from 74 cancer models, mostly PCa ... plus a few BrCa and NeuroBlastoma."),
      hr(), 
      #
      p(strong("Select a model group or tick individual models to display, click 'generate.plots' to refresh:")),
      #
      radioButtons("model_group", "Model Groups",
        choices = c(
          "all models"     = "all",
          "PCa models"     = "all.prostate",
          "in.vitro culture models" = "culture",
          "in.vivo xenografts" = "xeno",
          "neuroendocrine" = "nepc",
          "custom"         = "custom"
        ),
        selected = "custom",
        inline = FALSE
      ),
      #
      checkboxGroupInput("model_select", "Individual Models",
      choiceNames = as.vector(choice_labels),
      choiceValues = as.vector(models),
      #choices = sort(meta.data$model),
      #selected = meta.data$model[meta.data$mcrpc.subtype!='non.PCa']
      selected = c("LNCAP") #reducing the number of default selected models is the only option to push mem <1GB
      ),
      #
      hr(), 
      p("pca - prostate cancer | brca - breast cancer | neuro - neuro blastoma | CL - cell line | CLX - cell line derived xenograft | PDX - patient derived xenograft | ARPC - androgen receptor positive prostate cancer | ARLPC - androgen receptor low prostate cancer | DNPC - double negative prostate cancer | NEPC - neuroendocrine prostate cancer"),
      hr(), 
      p(em("COMBINED.shiny.app.version.2026-10-12 sandpit")),
      p(em("based on gene + transcript level data of:")),
      p(em("COMBINED.C_2026-02-19 + COMBINED.X_2026-07-28")),
      p(em("Gencode.33 | Ensembl.99 gene annotation")),
      hr(), 
      width=3
    ),
    
    mainPanel(
      tabsetPanel(
        #
        #
        tabPanel("Model.Overview", br(), 
                 plotlyOutput("scatter", height="750px", width="920px"), br(),  br(),
                 p(em("Where available, the mcrpc.subtype allocation for the models has been taken from the original Labrecque.et.al.2019 publication.")),
                 p(em("For a few models, the mcrpc.subtype has been extrapolated based on the GSVA.scoring results (see tooltip).")),
                 p(em("GSVA scoring for the plot above is based on linear TPM values (model averages) and matches the Labrecque annotation pretty well.")),
                 p(em("GSVA scoring based on linear FPKM values usually comes closest to the Labrecque annotation."))),
        #
        #
        tabPanel("Expression.Bar.Charts", br(), 
                 h4("Average Expression by Model"),
                 p(em("... please note, TPM values are rounded to integers, thus, expression below 0.5 TPM will be shown as zero")), 
                 plotlyOutput("stacked_bar.g"), br(), 
                 plotlyOutput("stacked_bar.tx"), br(), 
                 plotlyOutput("percentage_stack.tx")),
        #
        #
        tabPanel("Expression.Box.Plots.by.Model", br(), 
                 h4("Gene Expression by Model"), 
                 p(em("... please note, TPM values are rounded to integers, thus, expression below 0.5 TPM will be shown as zero")), 
                 radioButtons("yscale_model","Y.axis scale:",choices = c("Linear" = "linear", "Log2" = "log2"), inline = TRUE), br(), 
                 plotlyOutput("boxplot.model1", height="500px")), 
        #
        #
        tabPanel("Expression.Box.Plots.by.Subtype", br(), 
                 h4("Gene Expression by Labrecque mCRPC Subtype"),
                 p(em("... please note, TPM values are rounded to integers, thus, expression below 0.5 TPM will be shown as zero")), 
                 radioButtons("yscale_subtype","Y.axis scale:",choices = c("Linear" = "linear", "Log2" = "log2"), inline = TRUE), br(), 
                 plotlyOutput("boxplot.subtype1", height="500px", width="700px")),
        #
        #
        tabPanel("Gene.Sets", br(), 
                 h4("GSVA gene set scores"),
                 p(em("... draft ... GSVA scores have been calculated based on average TPM values per model")), 
                 plotlyOutput("boxplot.gsva"))
        #
        #
      ),width=9
    )
  )
)
##############################################
### END
##############################################

