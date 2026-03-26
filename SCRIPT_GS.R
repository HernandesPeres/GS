remove(list = ls())

# install.packages(c("bWGR", "dplyr", "plotly", "ggplot2"))

library(bWGR)
library(dplyr)
library(plotly)
library(ggplot2)

load("dataGS.RData") # Loading data

head(Pheno)
tail(Pheno)

dim(Geno)
Geno[1:10, 1:10]
hist(Pheno$Trait1) # Continuous distribution

# Population structure
pca <- prcomp(Geno)

plot_ly(data = as.data.frame(pca$x),
        x = ~PC1,
        y = ~PC2,
        z = ~PC3,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5))


###>>>--- 1. Organinzing training population
geno_trn <- Geno[1:300,]   # Traning pop
geno_tst <- Geno[301:500,] # Unseen pop

pheno_trn <- Pheno[1:300,]   # Training pop
pheno_tst <- Pheno[301:500,] # Unseen pop


###>>>--- 2. Model building
###>>>--- RR-BLUP

Model = wgr(y = pheno_trn$Trait1,   
            X = as.matrix(geno_trn))

###>>>--- 3. Prediction
plot(Model$hat, pheno_trn$Trait1) # Blups

cor(Model$hat, pheno_trn$Trait1) # Model accuracy

All_effect <- Model$b # Allele substitution effects for all markers
head(All_effect)

GEBV <- geno_tst %*% All_effect # GEBV of non-evaluated genotypes

###>>>--- 4. Selection
GEBV <- as.data.frame(GEBV)
df <- data.frame(id = rownames(GEBV),
                 GEBV = GEBV$V1)

df <- df %>% arrange(desc(GEBV)); 
gen.sel <- df[1:20,]; gen.sel

df$group <- "Non-selected"
gen.sel$group <- "Selected"

df_all <- rbind(df, gen.sel)

ggplot(df_all, aes(x = GEBV, color = group, fill = group)) +
  geom_histogram(alpha = 0.4, bins = 20, position = "identity") +
  geom_density(aes(y = ..count..), size = 1, alpha = 0.4) +
  labs(x = "GEBV", y = "Density") +
  scale_color_manual(values = c("Non-selected" = "red", "Selected" = "blue")) +
  scale_fill_manual(values = c("Non-selected" = "red", "Selected" = "blue"))+
  theme(legend.title = element_blank())
