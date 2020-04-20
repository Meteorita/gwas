require(dplyr)
setwd("/home/mrog/data/stroke/geno/R_scripts")
source("pca_helpers.R")
system("ls")

## Load data from exp 1
data <- LoadData("/home/mrog/data/stroke/geno/exp1/pruneddata.fam",
                 "/home/mrog/data/stroke/geno/exp1/plink.eigenvec")

FindOutliers(data)

# Load data from exp 2
data <- LoadData("/home/mrog/data/stroke/geno/exp2/pruneddata.fam",
                 "/home/mrog/data/stroke/geno/exp2/plink.eigenvec")

FindOutliers(data)

require(dplyr)
source("pca_helpers.R")

## Plot PCA for exp 1
data <- LoadData("/home/mrog/data/stroke/geno/exp1/pruneddata.fam",
                 "/home/mrog/data/stroke/geno/exp1/plink.eigenvec")

# Plot PCA
PlotPCA(data[["evec"]], data[["groups"]],  
        "PCA of exp1", "out/exp1_pca.png")

# Determine the proportion of variance of each component
eigenvec <- data[["evec"]]
pvar <- ((apply(eigenvec, 1, sd)^2) / (sum(apply(eigenvec, 1, sd)^2)))*100

plot(pvar)
pvar[pvar > 4]


## Plot PCA for exp 2
data <- LoadData("/home/mrog/data/stroke/geno/exp2/pruneddata.fam",
                 "/home/mrog/data/stroke/geno/exp2/plink.eigenvec")

# Plot PCA
PlotPCA(data[["evec"]], data[["groups"]],  
        "PCA of exp2", "out/exp2_pca.png")

# Determine the proportion of variance of each component
eigenvec <- data[["evec"]]
pvar <- ((apply(eigenvec, 1, sd)^2) / (sum(apply(eigenvec, 1, sd)^2)))*100

plot(pvar)
pvar[pvar > 4]
