library(tidyverse)
library(patchwork)
library(pheatmap)

df <- data.frame(nb_cluster = c("8 → 7", "7 → 6", "6 → 5", "5 → 4",
                       "4 → 3", "3 → 2", "2 → 1"),
                 delta = c(0.5, 0.6, 0.7, 0.8, 1.4, 1.5, 3))

df$nb_cluster <- factor(
  df$nb_cluster,
  levels = df$nb_cluster
)

g <- df %>% 
  ggplot(aes(x = nb_cluster, y = delta))+
  geom_bar(stat = "identity", col = "black", fill = "blue")+
  theme_minimal()+
  labs(x = "number of cluster",
       y = "delta T", 
       title = "Variation of criterion T")+
  theme(axis.text = element_text(size = 15, color = "black"), 
        plot.title = element_text(size = 15, hjust = 0.5),
        axis.title = element_text(size = 15))


# ggsave("C:/Users/g.martet/Documents/Agrostat Dijon/figures/var_crit_T.png", g, dpi = 400,
#        width = 7, height = 5)

#-------------------------------------------------------------------------------
# figure variable fonctionnelle et variable smoothée
#-------------------------------------------------------------------------------

dat <- openxlsx::read.xlsx("C:/Users/g.martet/Documents/updatedata/dat.xlsx")
dim(dat)

dfPC1.AMR_ville_FQ <- openxlsx::read.xlsx("C:/Users/g.martet/Documents/functional_study/df_fPC1/sum.var_AMR_ville_FQ_R.xlsx")
rownames(dfPC1.AMR_ville_FQ) <- dfPC1.AMR_ville_FQ[,1]
dfPC1.AMR_ville_FQ <- dfPC1.AMR_ville_FQ[,-1]

data <- dat[, which(colnames(dat) %in% c("region", "annee", v))]             
ncomp <- 2 
Nb.Base <- 5

func_data <- function(data, Nb.Base, ncomp){
  
  data <- data %>% 
    dplyr::mutate(annee = annee %>% factor, region = region %>%  factor)
  
  # Paramètres
  Temps      <- levels(data$annee)
  NbTemps    <- length(Temps)
  VarName    <- colnames(data)[-c(1:2)]
  NbVar      <- length(VarName)
  Regions    <- levels(data$region)
  NbRegions  <- length(Regions)
  my_palette <- colorRampPalette(c("blue", "grey", "red"))
  
  # Transformation des variables en matrice
  mat_tmps_reg <- lapply(1:NbVar, function(p) as.matrix(reshape2:::dcast(data, annee  ~ region, value.var = VarName[p])[, -1]))
  
  # transformation en objet fonctionnel
  # Base B-spline
  spline.basis <- fda::create.bspline.basis(rangeval = c(0, 1), nbasis = Nb.Base, norder = 3)
  # plot(spline.basis, lty = 1, lwd = 2, main = "Base de B-splines", xlab = 'Temps')
  # Projection des données dans la base
  data.amc.fd <- lapply(1:NbVar, function(p) {
    mat <- mat_tmps_reg[[p]]
    mat <- cbind(mat, annee = 2012:2024)
    mat <- na.omit(mat)
    Temps <- levels(as.factor(mat[, "annee"]))
    NbTemps    <- length(Temps)
    mat <- mat[,-which(colnames(mat) == "annee")]
    Time.01      <- seq(0, 1, length.out = NbTemps)
    fda::smooth.basis(Time.01, mat, spline.basis)$fd})
  
  # fcpa
  fPCA.amc <- lapply(1:NbVar, function(p) fda::pca.fd(data.amc.fd[[p]], nharm = ncomp, centerfns = TRUE))
  names(fPCA.amc) <- VarName
  df <- data.frame(col1 = rep(NA,12))
  for (j in 1:NbVar){
    d <- as.data.frame(fPCA.amc[[j]]$scores)
    colnames(d) <- paste0(VarName[j], "_fPC", 1:ncol(d))
    df <- cbind.data.frame(df, d)
  }
  df <- df[,-1]
  rownames(df) <- c("Auvergne-Rhone-Alpes", "Bourgogne-Franche-Comte", "Bretagne",               
                    "Centre-Val de Loire", "Grand Est", "Hauts-de-France", "IDF", "Normandie",               "Nouvelle-Aquitaine", "Occitanie", "PACA", "Pays de la Loire")
  return(list(df, data.amc.fd, fPCA.amc))
}

v <- intersect(colnames(dat), colnames(dfPC1.AMR_ville_FQ))
dat.func_ville_FQ <- func_data(data = dat[, which(colnames(dat) %in% c("region", "annee", v))],              
                               ncomp = 2, Nb.Base = 5)

# 1) Plot de la variable fonctionelle

cols <- c("IDF" = "#FFD700", 
          "Auvergne-Rhone-Alpes" = "#FB6A4A", "PACA" = "#CB181D", 
          "Nouvelle-Aquitaine" = "#FD8D3C", "Occitanie" = "#D94701",
          "Centre-Val de Loire" = "#9ECAE1", "Pays de la Loire" = "#6BAED6",
          "Normandie" = "#3182BD", "Bretagne" = "#08519C", 
          "Bourgogne-Franche-Comte" = "#74C476", "Grand Est" = "#31A354", 
          "Hauts-de-France" = "#006D2C")

g1 <- dat %>% 
  dplyr::filter(annee != 2024) %>% 
  dplyr::select(region, annee, log_AMC_ville_FQ) %>% 
  tidyr::pivot_longer(!c("region", "annee"), names_to = "Var",
                      values_to = "value") %>%
  ggplot(aes(x = annee, y = value, col = region))+
  geom_point()+
  geom_line(show.legend = FALSE)+
  theme_bw()+
  labs(color = "Région",
       title = "Functional variable X")+
  scale_x_continuous(breaks = seq(2012, 2024, by = 1),
                     minor_breaks = seq(2012, 2024, by = 1))+
  theme(axis.text.y = element_text(size = 6.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 7.5),
        axis.text.x = element_text(angle = 45, vjust = 0.6, colour = "black", size = 6.5), 
        legend.text = element_text(size = 6.5), 
        legend.title = element_text(size = 6.5),
        legend.position = "bottom",
        legend.justification = "left",
        legend.spacing = unit(0.5, "cm"),        
        legend.key.height = unit(0.1, "cm"),   
        legend.key.width = unit(0.1, "cm"),    
        legend.key.spacing.y = unit(0.1, "cm") 
  )+
  scale_color_manual(values = cols)

g1

ggsave("C:/Users/g.martet/Documents/Agrostat Dijon/figures/var_AMC_FQ_ville.png", g1, dpi = 400,
       width = 5, height = 4)

# 2) plot de la variable smoothée

data <- dat %>% 
  dplyr::mutate(annee = annee %>% factor, region = region %>%  factor)

# Paramètres
Temps      <- levels(data$annee)
NbTemps    <- length(Temps)
#VarName    <- colnames(data)[-c(1:2)]
#NbVar      <- length(VarName)
Regions    <- levels(data$region)
NbRegions  <- length(Regions)
my_palette <- colorRampPalette(c("blue", "grey", "red"))

# Transformation des variables en matrice
M <- data %>% 
  dplyr::select(region, annee, log_AMC_ville_FQ) %>% 
  tidyr::pivot_wider(names_from = region, values_from = log_AMC_ville_FQ)

mat_tmps_reg <- as.matrix(M[, -1])

# transformation en objet fonctionnel
# Base B-spline
spline.basis <- fda::create.bspline.basis(rangeval = c(0, 1), nbasis = Nb.Base, norder = 3)

mat <- mat_tmps_reg
mat <- cbind(mat, annee = 2012:2024)
mat <- na.omit(mat)
Temps <- levels(as.factor(mat[, "annee"]))
NbTemps    <- length(Temps)
mat <- mat[,-which(colnames(mat) == "annee")]
Time.01      <- seq(0, 1, length.out = NbTemps)
Time.02      <- seq(0, 1, length.out = 1000)
out.smooth <- fda::smooth.basis(Time.01, mat, spline.basis)
plot(out.smooth$fd)
ysmooth <- fda::eval.fd(Time.01, out.smooth$fd)
ysmooth <- as.data.frame(ysmooth)
ysmooth$annee <-2012:2023

g2 <- ysmooth %>% 
  tidyr::pivot_longer(!c("annee"), names_to = "region", values_to = "value") %>% 
  ggplot(aes(x = annee, y = value, col = region))+
  geom_point()+
  geom_line(show.legend = FALSE)+
  theme_bw()+
  labs(color = "Région",
       title = "Smoothed functional variable X in B-spline basis")+
  scale_x_continuous(breaks = seq(2012, 2024, by = 1),
                     minor_breaks = seq(2012, 2024, by = 1))+
  theme(axis.text.y = element_text(size = 6.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 7.5),
        axis.text.x = element_text(angle = 45, vjust = 0.6, colour = "black", size = 6.5), 
        legend.text = element_text(size = 6.5), 
        legend.title = element_text(size = 6.5),
        legend.position = "bottom",
        legend.justification = "left",
        legend.spacing = unit(0.5, "cm"),        
        legend.key.height = unit(0.1, "cm"),   
        legend.key.width = unit(0.1, "cm"),    
        legend.key.spacing.y = unit(0.1, "cm") 
  )+
  scale_color_manual(values = cols)

g2

ggsave("C:/Users/g.martet/Documents/Agrostat Dijon/figures/smoothed_AMC_FQ_ville.png", g2, dpi = 400,
       width = 5, height = 4)

ggpubr::ggarrange(g1, g2, common.legend = TRUE)

# Heatmap fPCA

df.heatmap.fPCA <- dfPC1.AMR_ville_FQ %>% 
  dplyr::select(log_AMC_ville_FQ) %>% 
  dplyr::arrange(log_AMC_ville_FQ)

?pheatmap::pheatmap
df.plot <- t(df.heatmap.fPCA)
rownames(df.plot) <- "fPC1 (98.9%) \nFunctional variable X"
  
ph <- pheatmap::pheatmap(df.plot, 
                    cluster_rows = FALSE, 
                    cluster_cols = FALSE,
                    color = colorRampPalette(c("blue", "white", "red"))(100),
                    cellwidth = 20,
                    cellheight = 20,
                    fontsize = 10,
                    border_color = "white",
                    angle_col = 45, 
                    display_numbers = FALSE)

ph

ggsave("C:/Users/g.martet/Documents/Agrostat Dijon/figures/heatmap_fPC1.png", ph, dpi = 400,
       width = 10, height = 3, bg = "white")

#-------------------------------------------------------------------------------
# graphe des % de variance expliquée des fPC1
#-------------------------------------------------------------------------------

dfPC1.AMR_ville_FQ <- dfPC1.AMR_ville_FQ %>% 
  dplyr::select(- AMR_ville_FQ_R.1, - AMR_ville_FQ_R.2, - AMR_ville_FQ_R.3)

dat.func_ville_FQ[[3]]$log_UGB_density_volailles$varprop

df.varprop.fPC1 <- data.frame(variable = rep(NA, ncol(dfPC1.AMR_ville_FQ)),
                              block = rep(NA, ncol(dfPC1.AMR_ville_FQ)),
                              varprop_fPC1 = rep(NA, ncol(dfPC1.AMR_ville_FQ)))
j <- 6
for (j in 1:ncol(dfPC1.AMR_ville_FQ)){
  var <- colnames(dfPC1.AMR_ville_FQ)[j]
  df.varprop.fPC1[j, "variable"] <- var
  df.varprop.fPC1[j, "varprop_fPC1"] <- dat.func_ville_FQ[[3]][[var]]$varprop[1]
}

df.varprop.fPC1$block <- c("Y", 
                           rep("AMC", 25-2+1),
                           rep("Animals", 44-26+1),
                           rep("Environment", 51-45+1),
                           rep("Humans", 62-52+1))

# df.varprop.fPC1$x <- as.factor(c("fPC1 Y",
#                        paste0("fPC1 var", 1:24),
#                        paste0("fPC1 var", 1:(44-26+1)),
#                        paste0("fPC1 var", 1:(51-45+1)),
#                        paste0("fPC1 var", 1:(62-52+1))))

g <- df.varprop.fPC1 %>% 
  ggplot(aes(x = variable, y = varprop_fPC1, fill = block))+
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+
  theme_bw()+
  facet_wrap(~ block, scales = "free")+
  geom_hline(yintercept = 0.70, color = "red3")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     minor_breaks = seq(0, 1, by = 0.1),
                     labels = paste0(seq(0, 100, by = 10), "%"),
                     expand = c(0, 0.01))+
  labs(title = "Percentage of explained variance of each functional variable by its fPC1 score")+
  theme(axis.text.x = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8, color = "black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 8))+
  scale_fill_manual(values = c("Y" = "#F8766D", "Animals" = "#00B0F6", 
                               "Humans" = "#E76BF3", 
                    "Environment" = "#00BF7D", "AMC" = "#A3A500"))

g

ggsave("C:/Users/g.martet/Documents/Agrostat Dijon/figures/barplot_fPC1.png", g, dpi = 400,
       width = 6, height = 4, bg = "white")
