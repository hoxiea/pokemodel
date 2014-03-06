

# Read in the Pokemon information
DATAPATH <- "/Users/hha/Dropbox/pokemodel/game_data/base_stats.csv"
p <- read.csv(DATAPATH, header=TRUE)
pairs(p[, 3:ncol(p)])
boxplot(p[, 3:ncol(p)])