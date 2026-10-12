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

library(shiny)
library(reshape2)
library(ggplot2)
library(plotly)
library(ggpubr)

### setup workspace
##############################################
### shinyapps.io individual files max 100 MB each and total bundle 1GB for free accounts
options(shiny.maxRequestSize = 500 * 1024^2) #increase file upload limit (500 MB, default is 5 MB)
#
### read data files
meta.data<-readRDS("COMBINED.UT-H.REF2-shiny.meta.data.avg_2026-10-12.rds") 
combined.data<-readRDS("COMBINED.UT-H.REF2-shiny.tpm.data.GT_2026-10-12.rds") #genes and transcripts
gsva.scores<-readRDS("COMBINED.UT-H.REF2-shiny.gsva.scores_2026-10-12.rds") #gsva scores 
gene.sets<-colnames(gsva.scores)[10:ncol(gsva.scores)] #extracting gene set names
#
### color schemes
labrecque.colors<-c('AMPC'='#cc0000','ARLPC'='#7d23cc','ARPC'='#458a00','DNPC'='#385ecc','NEPC'='#eeab0c','non.PCa'='grey40') #named.vector is best
polychrome<-rep(c("#5A5156","#F6222E","#FE00FA","#16FF32","#3283FE","#FEAF16","#B00068","#1CFFCE","#90AD1C","#2ED9FF","#DEA0FD","#AA0DFE","#F8A19F","#325A9B","#C4451C","#1C8356","#85660D","#B10DA1","#FBE426","#1CBE4F","#FA0087","#FC1CBF","#F7E1A0","#C075A6","#782AB6","#AAF400","#BDCDFF","#822E1C","#B5EFB5","#7ED7D1","#1C7F93","#D85FF7","#683B79","#66B0FF","#3B00FB","#E4E1E3"),times=2) #36 unique x2 as safety
#
### custom checkbox labels
meta.data <- meta.data[order(meta.data$model),] #sort the whole meta data in alphabetical model order
#models <- sort(meta.data$model) #alphabetical order
models <- meta.data$model #"character"
subtypes <- meta.data$mcrpc.subtype[match(models, meta.data$model)] #"character"
#tissues <- meta.data$tissue[match(models, meta.data$model)] #"character"
#
meta.data$source<-ifelse(grepl('xeno',meta.data$source.2),"in.vivo","in.vitro")
#
meta.data$tissue.labels<-meta.data$source.2
meta.data$tissue.labels<-gsub("patient.derived.xenograft","PDX",meta.data$tissue.labels)
meta.data$tissue.labels<-gsub("cell.line.xenograft","CLX",meta.data$tissue.labels)
meta.data$tissue.labels<-gsub("cell.line","CL",meta.data$tissue.labels)
#
tissues <- meta.data$tissue.labels[match(models, meta.data$model)] #"character"
#
### simple labels
# choice_labels <- mapply(function(model, tissue, subtype) {
#     tags$span(paste0(model, " | ", tissue, " - ", subtype))
#   },
#   models,
#   tissues,
#   subtypes,
#   SIMPLIFY = FALSE,
#   USE.NAMES = FALSE #needs to be un.named to work in ui
# )
#
### colored labels
choice_labels <- mapply(
  function(model, tissue, subtype) {
    
    subtype_color <- labrecque.colors[subtype]
    if (is.na(subtype_color)) subtype_color <- "black"
    
    tags$span(
      style = sprintf("color:%s;", subtype_color),
      sprintf("%s | %s - %s", model, tissue, subtype)
      # tried bold labels but doesn't work
    )
  },
  models,
  tissues,
  subtypes,
  SIMPLIFY = FALSE,
  USE.NAMES = FALSE
)
##############################################
### END
##############################################



