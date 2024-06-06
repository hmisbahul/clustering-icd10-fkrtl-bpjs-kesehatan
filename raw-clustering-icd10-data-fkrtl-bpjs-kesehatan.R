---
  title: "Klasterisasi ICD-10 Data Klaim FKRTL BPJS Kesehatan Menggunakan Metode Density-Based
  Spatial Clustering of Application with Noise (DBSCAN)"
author: "Misbahul Huda"
date: "May 22, 2024"
output:
  pdf_document: default
html_document: default
---
  
  # Pre-Processing Data
  
  ## Input Library
  ```{r}
library(haven)
library(dplyr)
library(tidyverse)
```

## Import Dataset
```{r}
## data utama
datamentah <- read_dta('C:/Users/Yurnalis/Documents/Misbah/Buat Kerja/Portofolio/clustering-icd10-data-fkrtl-bpjs-kesehatan/2019202003_fkrtl.dta')
peserta <- read_dta('C:/Users/Yurnalis/Documents/Misbah/Buat Kerja/Portofolio/clustering-icd10-data-fkrtl-bpjs-kesehatan/2015202001_kepesertaan.dta')

## data tambahan
icd10 <- read.csv('C:/Users/Yurnalis/Documents/Misbah/Buat Kerja/Portofolio/clustering-icd10-data-fkrtl-bpjs-kesehatan/icd-10.csv')
provinsi <- read.csv('C:/Users/Yurnalis/Documents/Misbah/Buat Kerja/Portofolio/clustering-icd10-data-fkrtl-bpjs-kesehatan/provinsi.csv')

## cuplikan data
head(datamentah)
head(peserta)
head(icd10)
head(provinsi)
```

## Memilah dan menggabungkan data yang akan digunakan
```{r}
## data klaim dari tahun 2019 hingga 2020
data1 = datamentah %>% 
  select(PSTV01,PSTV15,FKL03,FKL05,FKL15A)
data1$FKL03 = as.Date(data1$FKL03)
data2 = with(data1, data1[(FKL03 >= "2019-01-01" & FKL03 <= "2020-12-31"),])

## merge data klaim dan kategori yang akan menjadi variabel model clustering
data3 = merge(data2,icd10,by="FKL15A")
data4 = merge(data3, provinsi, by="FKL05")
head(data4)

## select data kepesertaan dan merge provinsi 
peserta1 = peserta %>%
  select(PSTV01,PSTV13,PSTV15,PSTV16)
peserta2 = filter(peserta1, PSTV16 >= "2019" & PSTV16 <= "2020")
peserta3 = merge(peserta2,provinsi,by="PSTV13")
head(peserta3)
```

## Checking blank row data klaim
```{r}
cat("Summary data yang digunakan:\n\n")
summary(data4)
cat("Informasi data:\n\n")
str(data4)
cat("total NA values:", sum(is.na(data4)))
```

## Checking blank row data kepesertaan
```{r}
cat("Summary data yang digunakan:\n\n")
summary(peserta3)
cat("Informasi data:\n\n")
str(peserta3)
cat("total NA values:", sum(is.na(peserta3)))
```

## Pengelompokkan data berdasarkan provinsi dan menghitung jumlah klaim tiap kategori per provinsi
```{r}
## variabel kepesertaan merupakan nilai sampel peserta bpjs kesehatan yang digunakan untuk transformasi data
## nilai sampel(kepesertaan) = jumlah bobot peserta per provinsi X 1%
kepesertaan = peserta3 %>%
  group_by(Provinsi) %>%
  summarise(bobot = sum(PSTV15)*0.01)
head(kepesertaan)
```

## Pengelompokkan data berdasarkan provinsi dan menghitung jumlah klaim tiap kategori per provinsi
```{r}
data5 = data4 %>% group_by(Provinsi) %>%
  summarise(persalinan = sum(Kategori == 'Persalinan'),
            kecelakaan = sum(Kategori == 'Kecelakaan'),
            katastropik = sum(Kategori == 'Katastropik'),
            penyakit_lainnya = sum(Kategori == 'Penyakit Lainnya'))
head(data5)
```

## Transformasi Data
```{r}
## transformasi data dilakukan dengan membagi jumlah klaim dengan nilai sampel peserta dikali 100%
## Transformasi(datafinal) = Jumlah Klaim(data5) / Nilai Sampel(kepesertaan) X 100%
transformasi = merge(data5,kepesertaan,by="Provinsi")
head(transformasi)
datafinal = transformasi %>% group_by(Provinsi) %>% 
  summarise(Klaim_Persalinan = (persalinan/bobot),
            Klaim_Kecelakaan = (kecelakaan/bobot),
            Klaim_Katastropik = (katastropik/bobot),
            Klaim_Penyakit_Lainnya = (penyakit_lainnya/bobot))
head(as.data.frame(datafinal))
```

## Normalisasi Data
```{r}
## normalisasi data dilakukan agar skala antar variabel tidak mendominasi variabel lain dan mengganggu model Cluster yang dihasilkan
## normalisasi data yang dilakukan penulis menggunakan fungsi scale
normdata = as.data.frame(as.data.frame(datafinal[,-c(1,1)]))
row.names(normdata) = paste(datafinal$Provinsi)
datanormal = scale(normdata)
cat("Summary Data yang telah dinormalisasi:\n\n")
summary(datanormal)
cat("Informasi data:\n\n")
str(datanormal)
cat("total NA values:", sum(is.na(datanormal)))
cat("Cuplikan data yang telah dinormalisasi dan siap untuk diolah:\n\n")
head(datanormal)
```


# Data Visualization and Modelling Data

## Input Library
```{r}
library(ggplot2)
library(scales)
library(factoextra)
library(dbscan)
library(clusterSim)
library(fpc)
```

## Statistika Daskriptif
```{r}
## filter data per variabel dari datafinal
persalinan1 = datafinal %>% dplyr::select(Provinsi, Klaim_Persalinan)
kecelakaan1 = datafinal %>% dplyr::select(Provinsi, Klaim_Kecelakaan)
katastropik1 = datafinal %>% dplyr::select(Provinsi, Klaim_Katastropik)
lainnya1 = datafinal %>% dplyr::select(Provinsi, Klaim_Penyakit_Lainnya)
```

```{r, fig.height=7, fig.width=10}
## Barplot Klaim Persalinan
persalinan1$Provinsi <- factor(persalinan1$Provinsi, levels = persalinan1$Provinsi[order(persalinan1$Klaim_Persalinan)])
Persalinan <- ggplot(persalinan1, aes(Provinsi, Klaim_Persalinan, fill = Klaim_Persalinan)) +
  labs (title = "Persentase Klaim Persalinan per Provinsi", x = "Provinsi", y = "Klaim Persalinan", fill = "Klaim\nPersalinan") +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(position = "right", low = "cadetblue4", mid = muted("cadetblue3"), high = "cadetblue2", midpoint = median(persalinan1$Klaim_Persalinan)) +
  geom_text(aes(label = paste0(round(Klaim_Persalinan,3),"%"), vjust = 0.3 ,hjust = -0.3), size = 3.5) +
  theme(plot.title = element_text(size = 20, color = "Black", face = "bold", hjust = 0.55, vjust = 0.7),
        axis.title = element_text(size = 15, color = "Black"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  ylim(0,0.55)
Persalinan
```
```{r, fig.height=7, fig.width=10}
## Barplot Klaim Kecelakaan
kecelakaan1$Provinsi <- factor(kecelakaan1$Provinsi, levels = kecelakaan1$Provinsi[order(kecelakaan1$Klaim_Kecelakaan)])
Kecelakaan <- ggplot(kecelakaan1, aes(Provinsi, Klaim_Kecelakaan, fill = Klaim_Kecelakaan)) +
  labs (title = "Persentase Klaim Kecelakaan per Provinsi", x = "Provinsi", y = "Klaim Kecelakaan", fill = "Klaim\nKecelakaan") + 
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(position = "right", low = "brown4", mid = muted("brown3"), high = "brown1", midpoint = median(kecelakaan1$Klaim_Kecelakaan)) +
  geom_text(aes(label = paste0(round(Klaim_Kecelakaan,5),"%"), vjust = 0.3 ,hjust = -0.3), size = 3.5) +
  theme(plot.title = element_text(size = 20, color = "Black", face = "bold", hjust = 0.55, vjust = 0.7),
        axis.title = element_text(size = 15, color = "Black"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  ylim(0,0.0115)
Kecelakaan
```
```{r, fig.height=7, fig.width=10}
## Barplot Klaim Katastropik
katastropik1$Provinsi <- factor(katastropik1$Provinsi, levels = katastropik1$Provinsi[order(katastropik1$Klaim_Katastropik)])
Katastropik <- ggplot(katastropik1, aes(Provinsi, Klaim_Katastropik, fill = Klaim_Katastropik)) +
  labs (title = "Persentase Klaim Katastropik per Provinsi", x = "Provinsi", y = "Klaim Katastropik", fill = "Klaim\nKatastropik") + 
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(position = "right", low = "seagreen4", mid = muted("seagreen3"), high = "seagreen2", midpoint = median(katastropik1$Klaim_Katastropik)) +
  geom_text(aes(label = paste0(round(Klaim_Katastropik,2),"%"), vjust = 0.3 ,hjust = -0.3), size = 3.5) +
  theme(plot.title = element_text(size = 20, color = "Black", face = "bold", hjust = 0.55, vjust = 0.7),
        axis.title = element_text(size = 15, color = "Black"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  ylim(0,8)
Katastropik
```
```{r, fig.height=7, fig.width=10}
## Barplot Klaim Penyakit Lainnya
lainnya1$Provinsi <- factor(lainnya1$Provinsi, levels = lainnya1$Provinsi[order(lainnya1$Klaim_Penyakit_Lainnya)])
Lainnya <- ggplot(lainnya1, aes(Provinsi, Klaim_Penyakit_Lainnya, fill = Klaim_Penyakit_Lainnya)) +
  labs (title = "Persentase Klaim Penyakit Lainnya per Provinsi", x = "Provinsi", y = "Klaim Penyakit Lainnya", fill = "Klaim\nPenyakit Lainnya") + 
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(position = "right", low = "goldenrod4", mid = muted("goldenrod3"), high = "goldenrod1", midpoint = median(lainnya1$Klaim_Penyakit_Lainnya)) +
  geom_text(aes(label = paste0(round(Klaim_Penyakit_Lainnya,3),"%"), vjust = 0.3 ,hjust = -0.3), size = 3.5) +
  theme(plot.title = element_text(size = 20, color = "Black", face = "bold", hjust = 0.55, vjust = 0.7),
        axis.title = element_text(size = 15, color = "Black"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  ylim(0,20)
Lainnya
```

## Plot Matriks Jarak
```{r, fig.height=7, fig.width=10}
## menghitung jarak antar provinsi menggunakan metode euclidean distance
hitungjarak = round(dist(datanormal, method = "euclidean"),1)

## plot matriks jarak
matriksjarak = fviz_dist(hitungjarak) +
  labs(title = "Matriks Jarak", fill = "Nilai Jarak") +
  theme(plot.title = element_text(size = 14, color = "Black", face = "bold", hjust = 0.5, vjust = 0.8),
        axis.text.x = element_text(size = 10, vjust = 0.2, angle = 90),
        axis.text.y = element_text(size = 10, vjust = 0.25),
        legend.title = element_text(size = 14))
matriksjarak
```

## kNNdisplot
```{r}
## kNN displot digunakan untuk menentukan parameter epsilon yang nantinya digunakan untuk membentuk model Cluster DBSCAN
kNNdistplot(datanormal, k = 2)
abline(h=1.9, col="red", lty=2)
abline(h=1.3, col="blue", lty=2)
abline(h=0.7, col="orange", lty=2)
title("kNN Distance Plot")
```
```{r}
cat("Cara membaca kNNdisplot ialah dengan melihat lengkungan curam yang ada pada plot\nDidapati dari plot diatas bahwa terdapat tiga lengkungan curam, yakni pada h1 = 0.7, h2 = 1.3, h3 = 1.9\nNilai h yang didapat dari kNNdisplot akan digunakan sebagai parameter epsilon pada model DBSCAN dan nilai k pada kNNdisplot akan digunakan sebagai MinPts pada model DBSCAN\nterdapat tiga model, yakni:\nModel 1 (MinPts = 2, Eps = 0.7)\nModel 2 (MinPts = 2, Eps = 1.3)\nModel 3 (MinPts = 2, Eps = 1.9)")
```

## DBSCAN
```{r, fig.height=9, fig.width=9}
## Plot Model 1
db_clust1 = dbscan(datanormal, eps = 0.7, MinPts = 2)
fviz_cluster(db_clust1, datanormal, palette="jco", ggtheme = theme_minimal()) +
  labs(title = "Plot Cluster Model 1") +
  theme(plot.title = element_text(size = 20, color = "Black", face = "bold", hjust = 0.5, vjust = 0.8),
        legend.title = element_text(size = 13))

## Dunn Index, DB Index, Average Silhouette Index
db_index1 = index.DB(datanormal, as.integer(db_clust1$cluster) + 1)
clustering_indices1 = cluster.stats(dist(datanormal),as.integer(db_clust1$cluster) + 1)
dunn_index1 = clustering_indices1$dunn
sil1 = silhouette(as.integer(db_clust1$cluster) + 1, dist(datanormal))
avg_silhouette1 = mean(sil1[, "sil_width"])
```
```{r, fig.height=9, fig.width=9}
## Plot Model 2
db_clust2 = dbscan(datanormal, eps = 1.3, MinPts = 2)
fviz_cluster(db_clust2, datanormal, palette="jco", ggtheme = theme_minimal()) +
  labs(title = "Plot Cluster Model 2") +
  theme(plot.title = element_text(size = 20, color = "Black", face = "bold", hjust = 0.5, vjust = 0.8),
        legend.title = element_text(size = 13))

## Dunn Index, DB Index, Average Silhouette Index
db_index2 = index.DB(datanormal, as.integer(db_clust2$cluster) + 1)
clustering_indices2 = cluster.stats(dist(datanormal),as.integer(db_clust2$cluster) + 1)
dunn_index2 = clustering_indices2$dunn
sil2 = silhouette(as.integer(db_clust2$cluster) + 1, dist(datanormal))
avg_silhouette2 = mean(sil2[, "sil_width"])
```
```{r, fig.height=9, fig.width=9}
## Plot Model 3
db_clust3 = dbscan(datanormal, eps = 1.9, MinPts = 2)
fviz_cluster(db_clust3, datanormal, palette="jco", ggtheme = theme_minimal()) +
  labs(title = "Plot Cluster Model 3") +
  theme(plot.title = element_text(size = 20, color = "Black", face = "bold", hjust = 0.5, vjust = 0.8),
        legend.title = element_text(size = 13))

## Dunn Index, DB Index, Average Silhouette Index
db_index3 = index.DB(datanormal, as.integer(db_clust3$cluster) + 1)
clustering_indices3 = cluster.stats(dist(datanormal),as.integer(db_clust3$cluster) + 1)
dunn_index3 = clustering_indices3$dunn
sil3 = silhouette(as.integer(db_clust3$cluster) + 1, dist(datanormal))
avg_silhouette3 = mean(sil3[, "sil_width"])
```

```{r}
## Menentukan model terbaik
cat("Cara untuk menentukan model terbaik untuk metode DBSCAN ialah dengan melihat nilai DB Index, Dunn Index, dan Average Silhouette Width nya.\n\nParameter DB Index: Semakin rendah nilai DB Index, semakin baik pula pengelompokkannya.\nParameter Dunn Index: Semakin tinggi nilai Dunn Index, semakin baik pula pengelompokkannya.\nParameter Average Silhouette Width: Semakin nilai Silhouette mendekati 1, maka semakin kuat model Cluster yang dihasilkan.\n\nBerikut ringkasan nilai DB Index, Dunn Index, dan Average Silhouette width pada masing-masing model yang telah didapat:\n\nModel 1 :\nDB Index:", db_index1$DB, "\nDunn Index:", dunn_index1, "\nAverage Silhouette Width:" ,avg_silhouette1, "\n\nModel 2 :\nDB Index:", db_index2$DB, "\nDunn Index:", dunn_index2, "\nAverage Silhouette Width:" ,avg_silhouette2, "\n\nModel 3 :\nDB Index:", db_index3$DB, "\nDunn Index:", dunn_index3, "\nAverage Silhouette Width:" ,avg_silhouette3, "\n\nBerdasarkan nilai DB Index, Dunn Index, dan Average Silhouette Width pada masing-masing model, dapat ditarik kesimpulan bahwa model terbaik untuk perhitungan ini ialah:\nModel 3")
```

## Hasil Cluster 
```{r, fig.height=9, fig.width=9}
## plot Cluster
fviz_cluster(db_clust3, datanormal, palette="jco", ggtheme = theme_minimal()) +
  labs(title = "Plot Cluster") +
  theme(plot.title = element_text(size = 20, color = "Black", face = "bold", hjust = 0.5, vjust = 0.8),
        legend.title = element_text(size = 13))
```
```{r}
## Hasil
Cluster = db_clust3$cluster
hasilklaster = datafinal %>% mutate(Cluster)
klaster0 = hasilklaster %>% filter(Cluster == 0)
klaster1 = hasilklaster %>% filter(Cluster == 1)
klaster2 = hasilklaster %>% filter(Cluster == 2)
klaster3 = hasilklaster %>% filter(Cluster == 3)
klaster00 = paste(klaster0$Provinsi)
klaster11 = paste(klaster1$Provinsi)
klaster22 = paste(klaster2$Provinsi)
klaster33 = paste(klaster3$Provinsi)
cat("Anggota Cluster 1:\n")
klaster11
cat("Anggota Cluster 2:\n")
klaster22
cat("Anggota Cluster 3:\n")
klaster33
cat("Anggota Cluster Noise: \n")
klaster00
```