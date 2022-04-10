#załadnowanie danych i poprawa nazw zmiennych
df0 <- readxl::read_excel("./dane_finalne.xlsx",sheet = 1)
df <- janitor::clean_names(df0)

#wczytanie bibliotek
library(tidyverse)
library(psych)
library(FactoMiner)
library(factoextra)
library(GGally)
library(fpc)

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

#zróżnicowanie zmiennych, współczynnik zmienności:
sapply(df[,-1],
       function(x) sd(x) / mean(x) * 100) %>%
  as_tibble(rownames = colnames(df[,1])) # working_pop_pct quasi stała, ale zbadamy, czy coś z nią można osiągnąć

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
#Percentage of explained variances
fviz_screeplot(dane_pca0, addlabels = TRUE)
#Zbadanie wymiarów
dane_pca0$var$coord
#Pierwszy wymiar - PKB, wydatki na research & development
#Drugi wymiar - praca w weekendy i deadliny

#wybranie dwóch wymiarów
dane_pca <- PCA(df_standarized, graph = F, ncp = 2)
#biplot
fviz_pca_var(dane_pca, repel = TRUE)
# wykres obserwacji wg wspolczynnikow skladowych
fviz_pca_ind(dane_pca, repel = TRUE, 
             addEllipses = T)

#Analiza skupień

#wykresy punktowe danych
ggpairs(df_standarized) #niewiele tu widać
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
fviz_dend(hc1, k = 3, rect = TRUE,
          color_labels_by_k = TRUE) + ylab("")
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
  theme_light()
#interpetacja
#grupowanie kmeans
x <- rep(0, 10) # wektor z wss, poczatkowo zerowy
# petla:
for(i in 1:10)
  x[i] <- kmeans(df_standarized, centers = i, nstart = 10)$tot.withinss
# wykres osypiska - 2 lub 3 grupu
ggplot(x %>% as.data.frame(),
       aes(row_number(-x),x)) +
  geom_line() +
  geom_point(col = "gray", size = 4) +
  theme_light() + xlab("") + ylab("") +
  ggtitle("Wykres osypiska")

#### kryterium ch (Calinskiego-Harabasza)
km.ch <-  kmeansruns(df_standarized, criterion = "ch", runs = 10)
plot(km.ch$crit, type = "b")
#wykres
km_crit <- km.ch$crit
ggplot(km_crit %>% as.data.frame(),
       aes(1:10, km_crit)) +
  geom_line() +
  geom_point(col = "gray", size = 4) +
  theme_light() + xlab("") + ylab("") +
  ggtitle("Kryterium Calinskiego-Harabasza")
#kryterium asw
km.asw <-  kmeansruns(df_standarized, criterion = "asw", runs = 10)
plot(km.asw$crit, type = "b")
km_crit1 <- km.asw$crit
ggplot(km_crit1 %>% as.data.frame(),
       aes(1:10, km_crit1)) +
  geom_line() +
  geom_point(col = "gray", size = 4) +
  theme_light() + xlab("") + ylab("") +
  ggtitle("Kryterium Average Silhouette")
