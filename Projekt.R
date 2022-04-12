#załadnowanie danych i poprawa nazw zmiennych
df0 <- readxl::read_excel("./dane_finalne.xlsx",sheet = 1)
df <- janitor::clean_names(df0)

#wczytanie bibliotek
library(tidyverse)
library(psych)
library(FactoMineR)
library(factoextra)
library(GGally)
library(fpc)
library(DataExplorer)
#sprawdzenie obserwacji brakujących
plot_intro(df) + 
  theme_minimal() +
  ylab("") + 
  xlab("") +
  ggtitle("Charakterystyka zmiennych")

#sprawdzenie zmiennych
df %>% glimpse()
#opis zmiennych
#country: Kraj
#gdp_pc:	Produkt krajowy brutto per capita w euro
#high_tech_trade_pc: Handel wysokimi technologiami per capita w euro
#r_d_gdp_pct: Wydatki na badania i rozwój jako procent PKB
#r_d_bud_pct: Wydatki na badania i rozwój jako procent budżetu
#use_cloud_pct: Procent ludzi korzystających z chmury
#weeknd_work_pct:	Procent ludzi pracujących  w weekendy (15-64)
#emp_deadline_pct:	Procent ludzi pracujących intensywnie i z deadlinem(15-64)
#working_pop_pct:	Procent populacji w wieku produkcyjnym (15-64)
#sprawdzenie pod kątem PCA
#korelacja pomiędzy zmiennymi
corrplot::corrplot(cor(df[,-1]),
                   method = "color",
                   type="lower",
                   addCoef.col = "black")
#Wyraźnie skorelowane ze sobą:
 #PKB na mieszkańca oraz wydatki na badania i rozwój jako    procent PKB: 74%
 #PKB na mieszkańca oraz procent ludzi korzystających z chmury: 73%
 #PKB na mieszkańca oraz handel wysokimi technologiami: 66%

#Brak korelacji pomiędzy handlem wysokimi technologiami per capita w euro
 #a procentem populacji w wieku produkcyjnym (15-64),
  #procentem ludzi pracujących intensywnie i z deadlinem(15-64),
   #a także procentem ludzi pracujących  w weekendy (15-64)

#test sferycznosci Bartletta
cortest.bartlett(cor(df[, -1]), n = nrow(df))
# p-value bardzo male, mozna odrzucic hipoteze, ze macierz korelacji jest macierza jednostkowa

#kryterium KMO
KMO(cor(df[, -1]))
# KMO > 0.5, wiec PCA dopuszczalna
#weeknd_work_pct ma niską wartość, także korelogram na to wskazywał
#PCA jest jak najbardziej dopuszczalna

#standaryzacja
df_standarized <-  scale(df[,-1]) %>% as.data.frame()
rownames(df_standarized) <- df$country

#PCA

dane_pca0 <- PCA(df_standarized, graph = F, ncp = 8)

#Pomiar rozbieżności pomiędzy modelem a rzeczywistymi danymi
fviz_screeplot(dane_pca0, addlabels = TRUE)
#Zbadanie wymiarów
dane_pca0$var$coord
#Pierwszy wymiar - PKB, wydatki na research & development, korzystanie z chmury
#Drugi wymiar - praca w weekendy i deadliny

#wybranie dwóch wymiarów ze względu na jakość zmiennych
dane_pca <- PCA(df_standarized, graph = F, ncp = 2)
#biplot
fviz_pca_var(dane_pca, repel = TRUE,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             labelsize = 5, col.circle = "grey40")
# wykres obserwacji
fviz_pca_ind(dane_pca, repel = TRUE,  pointsize = "contrib",
             pointshape = 21, fill = "#4b86b4", col.point = "white")

#Analiza skupień

#wykresy punktowe danych
ggpairs(df_standarized) +
  theme_light() +
  theme(strip.text = element_text(size=10, color = "black"))#niewiele tu widać
#macierz dystansu
d <- dist(df_standarized, method = "euclidean")
#wizualizacja macierzy dystansu
fviz_dist(d)
#grupowanie hclust
hc1 <- hclust(d, method = "ward.D2")
#wizualizacja dendogramu
fviz_dend(hc1)
#tniemy w 3 miejscach
#liczebność grup
cutree(hc1, k = 3) %>% table() # mniej wiecej rowne
#na wykresie
fviz_dend(hc1,
          k = 3, rect = TRUE, rect_border = "gray80",                 # label size
          k_colors = c("#000a14", "#0052a3", "#9a0707"),
          color_labels_by_k = TRUE, lwd = 1.1) + ylab("")
fviz_dend()
?fviz_dend
#dokładam podział do danych
df_cluster <- df
df_cluster$cluster.w <- cutree(hc1, k = 3) %>% as_factor()
#do boxplota
df_cluster_tidy <-  df_cluster %>% 
  pivot_longer(gdp_pc:working_pop_pct,
               values_to = "value",
               names_to = "zmienna")
#boxplot
ggplot(df_cluster_tidy,
       aes(value,cluster.w, col = cluster.w, fill = cluster.w))+
  geom_boxplot(alpha = .5)+
  facet_wrap(vars(zmienna),
             scales = "free",
             ncol = 4) +
  ylab("") +
  xlab("") +
  theme_light()+ 
  theme(strip.text = element_text(size=10, color = "black")) +
  ggtitle("Charakterystyka grup według hclust")
#interpetacja
#grupowanie kmeans
x <- rep(0, 10) # wektor z wss, poczatkowo zerowy
# petla:
for(i in 1:10)
  x[i] <- kmeans(df_standarized, centers = i, nstart = 10)$tot.withinss
# wykres osypiska - 2 lub 3 grupu
ggplot(x %>% as.data.frame(),
       aes(row_number(-x),x)) +
  geom_line(size = 1.5, alpha = 0.5, col = "gray60") +
  geom_point(col = "gray30", size = 4) +
  theme_minimal() + xlab("") + ylab("") +
  ggtitle("Wykres osypiska")

#Kryterium Average Silhouette i Calinskiego - Harabasza
km.ch <-  kmeansruns(df_standarized, criterion = "ch", runs = 10)
km.asw <-  kmeansruns(df_standarized, criterion = "asw", runs = 10)

kryteria <-  cbind.data.frame(`Caliński - Harabasz` = km.ch$crit,
                 `Average Silhouette` = km.asw$crit)
kryteria_pivot <- kryteria %>%
  mutate(index = 1:10) %>% 
  pivot_longer(`Caliński - Harabasz`:`Average Silhouette`,
               names_to = "kryterium", values_to = "Wartość")
kryteria_pivot
ggplot(kryteria_pivot,
       aes(index, `Wartość`)) +
  geom_line(size = 1.5, alpha = 0.5, col = "gray60" ) +
  geom_point(col = "gray30", size = 4) +
  theme_minimal() + xlab("") + ylab("") + 
  facet_wrap(vars(kryterium), scales = "free") +
  theme(strip.text = element_text(size=15))


#kmeans
df_standarized %>% glimpse()
kmeans_df <-  kmeans(df_standarized, centers = 2, nstart = 10)
df_cluster$cluster.km <- kmeans_df$cluster %>% as.factor()
#
plot_boxplot(df_cluster, by = "cluster.km")
df_cluster_tidy <- df_cluster %>% 
  pivot_longer(gdp_pc:working_pop_pct,
               values_to = "value",
               names_to = "zmienna") %>% 
  pivot_longer(cluster.w:cluster.km,
               values_to = "cluster_value",
               names_to = "cluster")
#wykres
ggplot(df_cluster_tidy %>% filter(cluster == "cluster.km"),
       aes(value,cluster_value, col = cluster_value, fill = cluster_value))+
  geom_boxplot(alpha = .5)+
  facet_wrap(vars(zmienna),
             scales = "free",
             ncol = 4) +
  ylab("") +
  xlab("") +
  theme_light()+ 
  theme(strip.text = element_text(size=10, color = "black"))
#Estonia_ porównanie z hclust dla gdp_pc
ggplot(df_cluster_tidy %>% filter(zmienna == "gdp_pc"),
       aes(value,cluster_value, col = cluster_value, fill = cluster_value))+
  geom_boxplot(alpha = .5)+
  facet_wrap(vars(cluster),
             scales = "free",
             ncol = 4)+
  ylab("") +
  xlab("") +
  theme_light()+ 
  theme(strip.text = element_text(size=10, color = "black")) +
  geom_curve(data = data.frame(x = 10494.2543210168, y = 2.26430671098796, xend = 12638.3521709584, yend = 2.01562917599984, cluster = "cluster.km"),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(30L, unit(0.1, "inches"),
                           "last", "closed"),
             inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = 7153.40377558588, y = 2.31956838542976, label = "Estonia", cluster = "cluster.km"),
            mapping = aes(x = x, y = y, label = label),
            colour = "#04cc82", inherit.aes = FALSE) +
  ggtitle("GDP per capita kmeans vs hclust")
#kmeans do wykresu
kmeans_do_wykresu <-  dane_pca$ind$coord %>%
  as.data.frame() %>%
  mutate(cluster_km = df_cluster$cluster.km )
#kmeans wykres
ggplot(kmeans_do_wykresu,
       aes(Dim.1,Dim.2, col = cluster_km)) + 
  geom_point() + theme_minimal() + ggrepel::geom_text_repel(label = rownames(kmeans_do_wykresu))

#kmeans do wykresu 2
kmeans_do_wykresu <- kmeans_do_wykresu %>%
  mutate(cluster_hcl = df_cluster$cluster.w)
# na koniec porównać cluster km = 3 vs cluster km  = 2 vs hclust
kmeans_df1 <-  kmeans(df_standarized, centers = 3, nstart = 10)
kmeans_do_wykresu$cluster_km_3 <- kmeans_df1$cluster %>% as.factor()
names(kmeans_do_wykresu)[3] <- "cluster_km_2"

kmeans_do_wykresu_ostatni_wykres <-  kmeans_do_wykresu %>% 
  mutate(country = rownames(kmeans_do_wykresu)) %>% 
  pivot_longer(cluster_km_2:cluster_km_3,
               names_to = "cluster_type",
               values_to = "cluster_value")
#ostatni wykres
ggplot(kmeans_do_wykresu_ostatni_wykres,
       aes(Dim.1,Dim.2, col = cluster_value)) +
  geom_point() + 
  facet_wrap(vars(cluster_type)) + 
  ggrepel::geom_text_repel(
    label = kmeans_do_wykresu_ostatni_wykres$country) + 
  theme_light() + 
  theme(strip.text = element_text(size=10, color = "black"))
#
df0 %>% glimpse()
df %>% glimpse()
dane_pca %>% glimpse()
dane_pca0 %>% glimpse()
df_cluster %>% glimpse()
df_cluster_tidy %>% glimpse()
df_standarized %>% glimpse()
hc1 %>% glimpse()
km.asw %>% glimpse()    
km.ch %>% glimpse()
kmeans_df %>% glimpse()
kmeans_df1 %>% glimpse()
kmeans_do_wykresu %>% glimpse()
kmeans_do_wykresu_ostatni_wykres %>% glimpse()
kryteria %>% glimpse()
kryteria_pivot %>% glimpse()
