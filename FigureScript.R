# Load libraries
library(R.devices)
library(FactoMineR)
library(factoextra)
#devtools::install_github("clauswilke/ggplot2@issue-2363-ggsave")
setwd("Downloads/Azhar")

# Load the data
data <- read.csv("allData.txt", sep = "\t")
head(data)
dim(data)
dataPCA <- data[,5:14]

# analysis the PCA
pca <- PCA(dataPCA, graph = F)


# Create a scree plot
fviz_eig(pca)

# creating the plot
p1 <- fviz_pca_biplot(pca,
        palette = "Accent2",
        geom.ind = "point",
        #col.ind = data$Type,
        pointshape = 21,
        pointsize = data$Concentration,
        fill.ind = data$Type,
        geom.var = c("text","arrow"),
        habillage = "none",
        col.var = "black",
        legend.title = list(fill = "Types", shape = "Group", size = "Concentration (μM)"),
        legend.key.size = 5,
        repel = TRUE,
        mean.point = FALSE,
        labelsize = 6,
        title = ""
)
# annotation and cleaning up
p2 <- p1 + geom_point(aes(shape=data$Group, col=data$Types)) +
          theme(text = element_text(family = "Times New Roman", face = "bold", size = 18), axis.title = element_text(size = 16),
          axis.text = element_text(size = 16), axis.line = element_line(size = 1),
          panel.grid.major =  element_blank(), panel.grid.minor = element_blank(),
      ) + annotate("text", x= 6.5, y = 4, label = "Positive Control") +
            guides(fill = guide_legend(override.aes = list(size = 5))) +
            scale_fill_discrete(breaks=c("3 nm", "6 nm", "8 nm", "12 nm", "Neg")) +
            guides(shape = guide_legend(override.aes = list(size = 5))) +
            labs(x = "PC1 (62.3%)", y = "PC2 (16%)"
            )
p2
# save the file
ggsave(p2, file="test.png", height=5.51, width = 5.24)
