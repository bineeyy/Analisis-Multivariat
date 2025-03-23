# Load library yang diperlukan
library(dplyr)
library(readr)
library(tidyr)
library(psych)

# Baca dataset
data <- read_csv("C:/Users/Devina/Documents/Letter Recognition/letter_recognition.csv")

# Cek struktur data
str(data)

# Cek missing values
sum(is.na(data))

# Hapus semua kolom non-numerik (kategorikal)
data <- data %>% select(where(is.numeric))

# Handling outlier: Menghapus outlier berdasarkan IQR
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x <- ifelse(x < lower_bound | x > upper_bound, NA, x)  # Ganti outlier dengan NA
  return(x)
}

data <- data %>% mutate(across(where(is.numeric), remove_outliers))

# Isi NA akibat outlier dengan median tiap kolom
data <- data %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))

# Cek hasil preprocessing
summary(data)

# Uji KMO (Kaiser-Meyer-Olkin) dan Bartlett Test
library(psych)
r <- cor(data)
KMO(r)

#Bartlett Test
bartlett.test(data)

# Simpan hasil preprocessing
write_csv(data, "C:/Users/Devina/Documents/Letter Recognition/letter_recognition pre.csv")


#---------Principal Component Analysis-----------
data <- read_csv("C:/Users/Devina/Documents/Letter Recognition/letter_recognition pre.csv")

scale_data = scale(data)
r = cov(scale_data)

library(corrplot)
cor_matrix <- cor(data)
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", tl.srt = 45)

##Menghitung eigenvalue dan eigenvector
pc <- eigen(r)
pc$values

library(dplyr)
##Menghitung proporsi varians dan kumulatif
sumvar <- sum(pc$values)
propvar <- sapply(pc$values, function(x) x/sumvar)*100
cumvar <- data.frame(cbind(pc$values, propvar)) %>% mutate(cum = cumsum(propvar))
colnames(cumvar)[1] <- "value"
rownames(cumvar) <- paste0("PC", 1:nrow(cumvar))
print(cumvar)

#hasilPCA
pc$vectors
scores <- as.matrix(scale_data) %*% pc$vectors
head(scores)

#Visualisasi
library(factoextra)
# membuat scree plot
fviz_eig(pca_result, 
         addlabels = TRUE, 
         ncp = 5, 
         barfill = "skyblue", 
         barcolor = "darkblue", 
         linecolor = "red")

#Biplot
fviz_pca_biplot(pca_result, 
                geom.ind = "point", 
                #col.ind = status.ipm, 
                #palette = c("#FC4E07","#E7B800", "#00AFBB"), 
                addEllipses = TRUE, 
                #legend.title = "Kategori"
)

# correlation circle
contrib_circle <- fviz_pca_var(pca_result, col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                               repel = TRUE) + 
  ggtitle("Kontribusi Variabel")
plot(contrib_circle)

#contribution
contrib_v_PC1 <- fviz_contrib(pca_result, choice = "var", axes = 1, top = 5) + ggtitle("PC1")
contrib_v_PC2 <- fviz_contrib(pca_result, choice = "var", axes = 2, top = 5) + ggtitle("PC2")
contrib_v_PC3 <- fviz_contrib(pca_result, choice = "var", axes = 3, top = 5) + ggtitle("PC3")
plot(contrib_v_PC1)

#---------Factor Analysis-----------
varcov = cov(scale_data)
pc = eigen(varcov)
pc$values
pc$vectors
sp = sum(pc$values[1:5])

L1 = sqrt(pc$values[1])*pc$vectors[,1]
L2 = sqrt(pc$values[2])*pc$vectors[,2]
L3 = sqrt(pc$values[3])*pc$vectors[,3]
L4 = sqrt(pc$values[4])*pc$vectors[,4]
L5 = sqrt(pc$values[5])*pc$vectors[,5]

L = cbind(L1,L2,L3,L4,L5)
L

# Perform factor analysis 
library(psych)
fa <- fa(r = scale_data, 
         covar = TRUE,
         nfactors = 5, 
         rotate = "varimax") 
#summary(fa) 

load <- fa$loadings

plot(load[,c(1 ,5)],type="n") # set up plot 
text(load[,c(1,5)],labels=names(data),cex=.7)

fa.diagram(load)

