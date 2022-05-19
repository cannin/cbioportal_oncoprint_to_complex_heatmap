# LOAD FUNCTIONS ----
# devtools::install_github("jokergoo/ComplexHeatmap")
# Works with ComplexHeatmap_2.2.0 from BioConductor; DOES NOT work on 2.13.1 from GitHub
library(ComplexHeatmap)

source("transform_cbioportal_to_complexheatmap_oncoprint.R")

# LOAD DATA ----
dat <- read.table("ccle_oncoprint_sample.tsv", sep="\t", header=TRUE, stringsAsFactors=FALSE)
tmp <- transform_cbioportal_to_complexheatmap_oncoprint(dat, verbose=TRUE)
dat_oncoprint <- tmp$mat
col <- tmp$col
alter_fun_list <- tmp$alter_fun_list

selected_cell_lines <- colnames(dat_oncoprint)

o1 <- oncoPrint(dat_oncoprint,
                col = col,
                alter_fun = alter_fun_list, 
                alter_fun_is_vectorized = FALSE,
                top_annotation = columnAnnotation(line_name = anno_text(selected_cell_lines, gp = gpar(fontsize = 10))),
                right_annotation = NULL,
                show_row_names = TRUE,
                #row_names_side = "left",
                show_pct = FALSE,
                #pct_side = "right"
                #show_heatmap_legend = FALSE
)

draw(o1)

# CREATE DATASET ----
# dat <- read.table("ccle_2019_onco500_PATIENT_DATA_oncoprint.tsv", sep="\t", header=TRUE, stringsAsFactors=FALSE)
# selected_cell_lines <- c("A2058_SKIN", "A2780_OVARY", "BT549_BREAST", "CAL62_THYROID", "HCC15_LUNG", "KMRC20_KIDNEY", "SNU1_STOMACH", "SNU449_LIVER", "U2OS_BONE", "YAPC_PANCREAS")
# selected_genes <- c("CDK4", "PTEN", "NOTCH1", "TP53", "JAK1", "EGFR", "BRAF", "MYC", "CTNNB1", "YAP1")
# 
# dat <- dat[, c("track_name", "track_type", selected_cell_lines)]
# filtered_dat <- dat[dat$track_name %in% selected_genes, ]
# write.table(filtered_dat, "ccle_oncoprint_sample.tsv", sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)

