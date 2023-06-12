setwd
library(plyr)
library(tidyverse)
library(ggplot2)



###############################

## Overlap between mega and bud fulldataset 

# this comes from https://stackoverflow.com/questions/64882496/how-to-calculate-the-overlap-between-2-dataset-distribution

est_dfe_beta <- est_dfe_bud_mega_fulldataset[, c('Tissue',
                                                 'b')]


d_bud <- with(est_dfe_beta, density(b[est_dfe_beta == "Bud"], 
                           from = min(b), 
                           to = max(b)))


d_mega <- with(est_dfe_beta, density(b[est_dfe_beta == "Mega"], 
                           from = min(b),
                           to = max(b)))


joint <- pmin(d_bud$y, d_mega$y)

bud_mega_overlap <- data.frame(x = rep(d_bud$x, 3),
                  y = c(d_bud$y, d_mega$y, joint),
                  Data = rep(c("Bud", "Mega", "overlap"), each = length(d_bud$x)))

ggplot(bud_mega_overlap, aes(x, y, fill = Data)) + 
  geom_area(position = position_identity(), color = "black") +
  scale_fill_brewer(palette = "Pastel2") +
  theme_bw()


sum(joint) / sum(d_bud$y, d_mega$y)






##############
# overlap between mega and but but only for the median to low subset

# median low expression values

est_dfe_beta <- median_low_est_dfe_all_tissues[, c('Tissue',
                                                            'b')]

melt_est_dfe_beta_all <-
  reshape2::melt(est_dfe_beta_all, id.var = c('Tissue'))


ggplot(data = melt_est_dfe_beta_all, mapping = aes(x = value, fill = Tissue)) +
  geom_density(alpha=0.4) +
  theme_classic()




d_bud <- with(est_dfe_beta, density(b[est_dfe_beta == "bud_low"], 
                                    from = min(b), 
                                    to = max(b)))


d_mega <- with(est_dfe_beta, density(b[est_dfe_beta == "mega_low"], 
                                     from = min(b),
                                     to = max(b)))


joint <- pmin(d_bud$y, d_mega$y)

bud_mega_overlap <- data.frame(x = rep(d_bud$x, 3), #I do not understand what the rep does in here, what is the 3 for?
                               y = c(d_bud$y, d_mega$y, joint),
                               Data = rep(c("Bud", "Mega", "overlap"), each = length(d_bud$x)))

ggplot(bud_mega_overlap, aes(x, y, fill = Data)) + 
  geom_area(position = position_identity(), color = "black") +
  scale_fill_brewer(palette = "Pastel2") +
  theme_bw()


sum(joint) / sum(d_bud$y, d_mega$y)





