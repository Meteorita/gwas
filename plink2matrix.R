## Convert Plink files into genotype matrix with rs dictionary
require(dplyr)

args <- commandArgs(T)

# input <- "/home/gennady/data/stroke/proc/merge2/merged2" 
input <- args[1]

# Convert to raw format
args <- c("--bfile", input, "--recodeAD", "--out", input)
system2("/home/gennady/tools/plink_5.2/plink", args = args)

#load raw file
out <- data.table::fread(paste0(input, ".raw")) %>% 
  select(IID, PHENOTYPE, starts_with("rs"), -ends_with("HET"))

# Save genotype table
data.table::fwrite(out, paste0(input, ".txt"), sep=" ", quote = F, na="-1")

# Load the list of markers
snps <- data.table::fread(paste0(input, ".bim"))
# For each chromosome find the first and last rs
dt <- snps %>% group_by(chr = V1) %>% summarize(rs1 = snps$V2[snps$V4 == min(V4)],
                                                pos1 = min(V4), 
                                                rs2 = snps$V2[snps$V4 == max(V4)],
                                                pos2 = max(V4))
# Save 
data.table::fwrite(dt[, c("rs1", "rs2", "chr")], paste0(input, ".rs.txt"), 
       sep=" ", quote = F, col.names = F)
