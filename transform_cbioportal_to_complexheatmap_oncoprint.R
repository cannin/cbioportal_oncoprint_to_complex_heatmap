#' Transform cBioPortal Oncoprint Download for ComplexHeatmap Oncoprint Visualization
#' 
#' @param df data.frame read in from cBioPortal oncoprint download; the first 2 columns should be track_name and track_type
#' @param return_type a string (mat: processed data matrix, col: colors, alter_fun_list: alteration function list); 
#'   this function has provide inputs needed for ComplexHeatmap oncoPrint(); (Default: mat)
#' @param verbose Show debugging information (Default: FALSE)
#' 
#' @export
transform_cbioportal_to_complexheatmap_oncoprint <- function(df, return_type="mat", verbose=FALSE) {
  # Parameter 
  annotation_ncol <- 2 
    
  # Color Setup  
  col <- c(mrna_hi="#FF9999", mrna_lo="#6699CC",
           prot_hi = "#FF3DF7", prot_lo = "#00E0FF",
           mut_mis_put_pass="#A68029", mut_mis_put_driv="#008000",
           mut_inframe_put_pass="#54D400", mut_inframe_put_driv="#993305",
           mut_trun_put_pass="#70808F", mut_trun_put_driv="#000000",
           amp="#FF0000", deep_del="#0000FF",
           homdel_rec="#0000FF", splice_rec="#E5802B", amp_rec="#FF0000",
           splice="#f0b87b",
           sv="#ce92e8", sv_rec="#8b00c9")
  
  # Shape Setup
  alter_fun_list <- list(
    background = function(x, y, w, h) {
      grid.rect(x, y, w, h, 
                gp = gpar(fill = "#DDDDDD", col = "white"))
    },
    amp = function(x, y, w, h) {
      grid.rect(x, y, w, h, 
                gp = gpar(fill = col["amp"], col = "white"))
    },
    deep_del = function(x, y, w, h) {
      grid.rect(x, y, w, h, 
                gp = gpar(fill = col["deep_del"], col = "white"))
    },
    homdel_rec = function(x, y, w, h) {
      grid.rect(x, y, w, h, 
                gp = gpar(fill = col["homdel_rec"], col = "white"))
    },
    amp_rec = function(x, y, w, h) {
      grid.rect(x, y, w, h, 
                gp = gpar(fill = col["amp_rec"], col = "white"))
    },
    mrna_hi = function(x, y, w, h) {
      grid.rect(x, y, w*0.66, h*0.90, 
                gp = gpar(fill = "#DDDDDD", col = col["mrna_hi"], lwd=4, linejoin='mitre'))
    },
    mrna_lo = function(x, y, w, h, v) {
      grid.rect(x, y, w*0.66, h*0.90, 
                gp = gpar(fill = "#DDDDDD", col = col["mrna_lo"], lwd=4, linejoin='mitre'))
    },
    prot_hi = function(x, y, w, h) {
      grid.polygon(
        unit.c(x + 0.43*w, x - 0.43*w, x - 0.43*w, x + 0.43*w), 
        unit.c(y + 0.48*h, y + 0.48*h, y + 0.25*h, y + 0.25*h),
        gp = gpar(fill = col["prot_hi"], col = col["prot_hi"], linejoin='mitre'))
    },
    prot_lo = function(x, y, w, h) {
      grid.polygon(
        unit.c(x + 0.43*w, x - 0.43*w, x - 0.43*w, x + 0.43*w), 
        unit.c(y - 0.48*h, y - 0.48*h, y - 0.25*h, y - 0.25*h),
        gp = gpar(fill = col["prot_lo"], col = col["prot_lo"], linejoin='mitre'))
    },
    mut_mis_put_pass = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.4, 
                gp = gpar(fill = col["mut_mis_put_pass"], col = col["mut_mis_put_pass"], linejoin='mitre'))
    },
    mut_mis_put_driv = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.4, 
                gp = gpar(fill = col["mut_mis_put_driv"], col = col["mut_mis_put_driv"], linejoin='mitre'))
    },
    mut_inframe_put_pass = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.4, 
                gp = gpar(fill = col["mut_inframe_put_pass"], col = col["mut_inframe_put_pass"], linejoin='mitre'))
    },
    mut_inframe_put_driv = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.4, 
                gp = gpar(fill = col["mut_inframe_put_driv"], col = col["mut_inframe_put_driv"], linejoin='mitre'))
    },  
    mut_trun_put_pass = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.4, 
                gp = gpar(fill = col["mut_trun_put_pass"], col = col["mut_trun_put_pass"], linejoin='mitre'))
    },
    mut_trun_put_driv = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.4, 
                gp = gpar(fill = col["mut_trun_put_driv"], col = col["mut_trun_put_driv"], linejoin='mitre'))
    },
    splice = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.4, 
                gp = gpar(fill = col["splice"], col = col["splice"], linejoin='mitre'))
    },
    splice_rec = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.4, 
                gp = gpar(fill = col["splice_rec"], col = col["splice_rec"], linejoin='mitre'))
    },
    sv = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.6, 
                gp = gpar(fill = col["sv"], col = col["splice"], linejoin='mitre'))
    },
    sv_rec = function(x, y, w, h) {
      grid.rect(x, y, w*0.69, h*0.6, 
                gp = gpar(fill = col["sv_rec"], col = col["sv_rec"], linejoin='mitre'))
    }
  )
  
  if(return_type == "col") {
    return(col)    
  } 
  
  if(return_type == "alter_fun_list") {
    return(alter_fun_list)
  }
  
  # Remove any NAs mixed with ""
  df[is.na(df)] <- ""
  
  df[df == "mRNA High"] <- "mrna_hi"
  df[df == "mRNA Low"] <- "mrna_lo"
  df[df == "Protein High"] <- "prot_hi"
  df[df == "Protein Low"] <- "prot_lo"
  df[df == "Missense Mutation (putative passenger)"] <- "mut_mis_put_pass"
  df[df == "Missense Mutation (putative driver)"] <- "mut_mis_put_driv"
  df[df == "Inframe Mutation (putative passenger)"] <- "mut_inframe_put_pass"
  df[df == "Inframe Mutation (putative driver)"] <- "mut_inframe_put_driv"
  df[df == "Truncating mutation (putative passenger)"] <- "mut_trun_put_pass"
  df[df == "Truncating mutation (putative driver)"] <- "mut_trun_put_driv"
  df[df == "Amplification"] <- "amp"
  df[df == "Deep Deletion"] <- "deep_del"
  df[df == "homdel_rec"] <- "homdel_rec"
  df[df == "amp_rec"] <- "amp_rec"
  df[df == "sv"] <- "sv"
  df[df == "sv_rec"] <- "sv_rec"
  df[df == "splice"] <- "splice"
  
  entry_types <- sort(unique(as.vector(as.matrix(df[, 3:ncol(df),]))))
  
  if(verbose) { cat("ENT: ", paste(entry_types, collapse="|")) }
  
  genes <- unique(df$track_name)
  tracks <- unique(df$track_type)
  
  if(verbose) { 
    cat("GENES: ", paste(genes, collapse="|"), "\n")
    cat("TRACKS: ", paste(tracks, collapse="|"), "\n")
  } 
  
  mat <- matrix("", nrow=length(genes), ncol=(ncol(df)-annotation_ncol))
  rownames(mat) <- genes 
  colnames(mat) <- colnames(df)[-c(1:annotation_ncol)]
  
  if(verbose) { cat("FINAL DIM: ROWS: ", nrow(mat), " COLS: ", ncol(mat), "\n") }
  
  for(k in 1:length(tracks)) {
    track <- tracks[k]
    
    if(verbose) {
      cat("CUR TRACK: ", track, "\n")
    }
    
    for(i in 1:nrow(mat)) {
      for(j in 1:ncol(mat)) {
        gene <- rownames(mat)[i]
        
        tmp_df <- df[df$track_type == track,]
        cur_entry <- tmp_df[i, j+annotation_ncol]
        
        if(!is.na(cur_entry) && cur_entry != "") {
          if(mat[gene, j] != "") {
            mat[gene, j] <- paste0(mat[gene, j], ";", cur_entry)             
          } else {
            mat[gene,j] <- cur_entry 
          }
        }
      }
    }
  }
  
  return(mat)  
}
