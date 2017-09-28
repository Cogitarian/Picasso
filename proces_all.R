library(tidyverse)
library(jpeg)
library(reshape2)

setwd("~/RProjects/Picasso")


# przygotowanie funkcji
source("analiza_funkcja.R")

# lista wszystkich obrazów
paintings <- readRDS("paintings.RDS")



# test funkcji
i <- sample(1:nrow(paintings), 1)

cat(paste0("\rObraz (", i, "/", nrow(paintings), "): \"",
           as.character(paintings[i, "title"]),
           "\"  plik: ", as.character(paintings[i, "path"])))

df <- prepare_kmeans_palete(title = as.character(paintings[i, "title"]),
                            year = as.numeric(paintings[i, "year"]),
                            path = as.character(paintings[i, "path"]),
                            picture_scale = 5,
                            n_clusters = 8)

palete <- as.character(df$RGB_kmeans_hex)
names(palete) <- as.character(df$RGB_kmeans_hex)

df %>%
   arrange(color_n) %>%
   mutate(RGB_kmeans_hex = factor(RGB_kmeans_hex, levels = RGB_kmeans_hex)) %>%
   ggplot() +
   geom_col(aes(RGB_kmeans_hex, n, fill = RGB_kmeans_hex)) +
   scale_fill_manual(values = palete)

rm(df, palete, i)



# dla każdego obrazu:
kmeans_colors <- data_frame()

for(i in 1:nrow(paintings)) {

   cat(paste0("\nObraz (", i, "/", nrow(paintings), "): \"",
              as.character(paintings[i, "title"]),
              "\"  plik: ", as.character(paintings[i, "path"])))

   tmp_df <- prepare_kmeans_palete(title = as.character(paintings[i, "title"]),
                                   year = as.numeric(paintings[i, "year"]),
                                   path = as.character(paintings[i, "path"]),
                                   picture_scale = 5,
                                   n_clusters = 8)

   # numer obrazu, na wszelki wypadek
   tmp_df$i <- i

   kmeans_colors <- kmeans_colors %>% bind_rows(tmp_df)

   # zapisanie co 20 obrazów
   if(i %% 10 == 0) saveRDS(kmeans_colors, file = "kmeans_colors.RDS")
}


saveRDS(kmeans_colors, file = "kmeans_colors.RDS")




plot_data <- kmeans_colors %>%
#   filter(color_n == 1) %>%
   group_by(year, color_n) %>%
   mutate(p = n/sum(n)) %>%
   ungroup()

palete <- as.character(plot_data$RGB_kmeans_hex)
names(palete) <- as.character(plot_data$RGB_kmeans_hex)


plot_data %>%
   ggplot() +
   geom_bar(aes(year, p, fill = RGB_kmeans_hex), stat = "identity", show.legend = FALSE) +
   scale_fill_manual(values = palete) +
   facet_wrap(~color_n)

