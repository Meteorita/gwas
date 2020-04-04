## Import ped/map into db

# Initiate
pref <- "/home/gennady/data/stroke/proc/filt/340full_stroke1"
tmp <- tempdir()
dir.create(tmp)
fn <- paste0(tmp, "/tmp")

conc <- function(t1, t2) {
  colnames(t1) <- "snp" 
  colnames(t2) <- "snp"
  paste0(t1$snp, t2$snp)
}

# Convert to ped/map
system2("/home/gennady/tools/plink_5.2/plink", 
        c("--bfile", pref, "--recode", "--out", fn))

# Load ped file
ped <- data.table::fread(paste0(fn, ".ped"))
# Split table
ped1 <- ped[, 1:6]
ped2 <- ped[, !c(1:6)]

# Combine the alleles
ind <- seq(1, by = 2, ncol(ped2))
l <- plyr::llply(ind[1:100], function(i) {
  t1 <- ped2[, .SD, .SDcols = i]
  t2 <- ped2[, .SD, .SDcols = i + 1]
  g <- conc(t1, t2)
})

m <- do.call("cbind", l)
m <- cbind(ped1$V2, m)
m <- as.data.frame(m, stringsAsFactors = F)

# Melt
out <- reshape2::melt(m , id.vars = c("V1"), 
                      variable.name = "snp",
                      value.name = "geno")
out$snp <- as.character(out$snp)

# Load map
map <- data.table::fread(paste0(fn, ".map"))
map$id <- paste0("V", 2:(nrow(map)+1))

# Merge two tables
res <- merge(out, map, by.x = "snp", by.y = "id", all.x = T)

# Drop unneccessary columns
res$snp <- NULL
res$V3 <- NULL

# Name colums
colnames(res) <- c("name", "geno", "chr", "snp", "pos")

# Save 
output <- paste0(tmp, "/data.txt")
data.table::fwrite(res, output, sep = "\t", col.names = F)

require(RMySQL)
# Initiate db connection
mydb <- dbConnect(MySQL(), user='mrog', password='ysrfkkxx', 
                  dbname='gwas', host='79.120.74.41')






# Clean
unlink(tmp, recursive = T)
