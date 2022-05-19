library(ComplexHeatmap)

# WORKS ----
mat = read.table(textConnection(
  "s1,s2,s3
g1,snv;indel,indel;snv,snv;indel
g2,snv;indel,snv;indel,indel;snv
g3,indel,indel,snv"), row.names = 1, header = TRUE, sep = ",", stringsAsFactors = FALSE)
mat = as.matrix(mat)
mat

# DOES NOT WORK ----
mat = read.table(textConnection(
  "s1,s2,s3
g1,snv;indel,indel;snv,snv;indel
g2,snv;indel,snv;indel,indel;snv
g3,indel,indel,indel;snv"), row.names = 1, header = TRUE, sep = ",", stringsAsFactors = FALSE)
mat = as.matrix(mat)
mat

col <- c(snv ="red", indel="blue")
alter_fun_list <- list(
  snv = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9,gp = gpar(fill = col["snv"], col = NA)),
  indel = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.4, gp = gpar(fill = col["indel"], col = NA))
)

h1 <- oncoPrint(mat, alter_fun = alter_fun_list, col = col, 
                alter_fun_is_vectorized = FALSE,
                top_annotation = NULL,
                right_annotation = NULL,
                show_row_names = TRUE,
                show_column_names = TRUE,
                row_names_side = "left",
                show_pct = FALSE)
draw(h1)
