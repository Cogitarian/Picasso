library(tidyverse)
library(jpeg)
library(reshape2)

prepare_kmeans_palete <- function(title, year, path, picture_scale = 5, n_clusters = 8)
{
   # wczytanie obrazka
   picture <- readJPEG(path)

   # przygotowanie data frames dla każdej ze składowych kolorów
   image.R <- melt(picture[,,1])
   image.G <- melt(picture[,,2])
   image.B <- melt(picture[,,3])

   # złączenie składowych w jedną dużą tabelę
   image.df <- cbind(image.R, image.G[3], image.B[3])
   colnames(image.df) <- c("x","y","R","G","B")

   # dla oszczędzenia pamięci usuwamy co zbędne
   rm(image.R, image.G, image.B, picture)

   image.df <- image.df %>%
      # kolory do HEX)
      mutate(RGB_hex = sprintf("#%02X%02X%02X", round(R * 255), round(G * 255), round(B * 255))) %>%
      # przeskalowanie dla szybszych późniejszych obliczeń
      filter(x %% picture_scale == 0) %>%
      filter(y %% picture_scale == 0) %>%
      mutate(x = x %/% picture_scale, y = y %/% picture_scale)

   # rozmiary przeskalowanego obrazu
   dimx <- max(image.df$x)
   dimy <- max(image.df$y)

   # kmeans dla składowych koloru
   image_df_kmeans <- kmeans(image.df[, c("R", "G", "B")], n_clusters)

   # centra
   image.df_centers <- image_df_kmeans$centers %>%
      as_data_frame() %>%
      mutate(cluster = as.numeric(rownames(.))) %>%
      rename(R_kmeans = R, G_kmeans = G, B_kmeans = B) %>%
      # kolory RGB
      mutate(RGB_kmeans_hex = sprintf("#%02X%02X%02X", round(255 * R_kmeans), round(255 * G_kmeans), round(255 * B_kmeans)))

   # łączymy dane oryginalne przeskalowanego obrazu z wynikiem kmeans
   image.df_all <- image.df %>%
      mutate(cluster = image_df_kmeans$cluster) %>%
      left_join(image.df_centers, by = "cluster")

   # znowu usuwamy śmieci z pamięci
   rm(image.df, image.df_centers, image_df_kmeans)

   # rozkład kolorów po kmeans
   popular_colors_kmeans <- image.df_all %>%
      count(RGB_kmeans_hex) %>%
      ungroup() %>%
      arrange(desc(n)) %>%
      mutate(color_n = row_number())

   # dodajemy dane o obrazie
   popular_colors_kmeans$title <- title
   popular_colors_kmeans$year <- year
   popular_colors_kmeans$path <- path


   return(popular_colors_kmeans)
}
