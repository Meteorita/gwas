## Helpers for PCA
require(ggplot2)

PlotPCA <- function(evec, groups, tlt, output) {
  
  # Plot Principal Components
  pc <- list(c("PC1", "PC2"), c("PC1", "PC3"), c("PC3", "PC2"))
  
  p <- lapply(pc, function(x) {
    ggplot(evec) + 
      geom_point(aes(x = evec[, x[1]], y = evec[, x[2]], 
                     col = groups), size = 1) + 
      labs(x = x[1], y = x[2]) +
      scale_colour_brewer(palette = "Paired") + 
      theme(legend.position = "none", 
            panel.background = element_rect(fill = "white", color= "grey"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  })
  
 fg <- ggpubr::ggarrange(plotlist = p, ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom",
                   labels = c("A", "B", "C"))
 
 ggpubr::annotate_figure(fg, top = tlt)
 ggsave(output, device = "png", width = 8, height = 8)
  
}

LoadData <- function(fam, eigenvec){
  # Args:
  #  fam: path/to/filename.fam 
  #  eigenvec: path/to/filename.eigenvec file 
  
  # Initiate output
  out <- vector("list")
  
  # Load PCA results
  evec <- data.frame(read.table(eigenvec, header = FALSE, skip = 0, sep = " "))
  rownames(evec) <- evec[, 2]
  evec <- evec[, 3:ncol(evec)]
  colnames(evec) <- paste("PC", c(1:20), sep="")
  
  # Load fam file of data used for PCA
  status <- read.table(fam)[, c("V2", "V6")]
  
  # Compile the output
  out[["evec"]] <- evec
  t <- sapply(rownames(evec), function(x) status$V6[which(status$V2 == x)])
  out[["groups"]] <- as.factor(t)
  
  out
  
}

FindOutliers <- function(data) {
  # Subset the eigenvectors
  eigenvec <- data[["evec"]]
  
  # Find out the indexes of the outliers applying the rule 
  # "more than 6 standard deviations away from the mean"
  ind <- apply(eigenvec, 2, function(x) {
    which(abs(x - mean(x)) > 6 * sd(x))
  }) %>% Reduce(union, .)
  
  # Show outlier ids
  rownames(eigenvec)[ind]
}





