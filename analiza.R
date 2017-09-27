library(tidyverse)
library(jpeg)
library(reshape2)

file <- "pics/the-girls-of-avignon-1907.jpg"

file <- "pics/guernica-by-pablo-picasso.jpg"
picture <- readJPEG(file)

dim(picture)
dims <- dim(picture)

plot.new()
rasterImage(picture, 0, 0, 1, 1)


image.R <- melt(picture[,,1])
image.G <- melt(picture[,,2])
image.B <- melt(picture[,,3])

image.df <- cbind(image.R, image.G[3], image.B[3])
colnames(image.df) <- c("x","y","R","G","B")

rm(image.R, image.G, image.B, picture)

image.df <- image.df %>%
   mutate(R = round(R * 255),
          G = round(G * 255),
          B = round(B * 255)) %>%
   mutate(RGB_hex = sprintf("#%02X%02X%02X", R, G, B)) %>%
   # przeskalowanie dla szybszych późniejszych obliczeń
   filter(x %% 5 == 0) %>%
   filter(y %% 5 == 0) %>%
   mutate(x = x %/% 5, y = y %/% 5)

dimx <- max(image.df$x)
dimy <- max(image.df$y)

image_df_kmeans <- kmeans(image.df[, c("R", "G", "B")], 8)

image.df_centers <- image_df_kmeans$centers %>%
   round(0) %>%
   as_data_frame() %>%
   mutate(cluster = as.numeric(rownames(.))) %>%
   rename(R_kmeans = R, G_kmeans = G, B_kmeans = B) %>%
   mutate(RGB_kmeans_hex = sprintf("#%02X%02X%02X", R_kmeans, G_kmeans, B_kmeans))

image.df_all <- image.df %>%
   mutate(cluster = image_df_kmeans$cluster) %>%
   left_join(image.df_centers, by = "cluster")


head(image.df_all)

popular_colors <- image.df_all %>%
   count(RGB_hex) %>%
   ungroup() %>%
   top_n(32, n) %>%
   arrange(n)

popular_colors_palete <- as.character(popular_colors$RGB_hex)
names(popular_colors_palete) <- as.character(popular_colors$RGB_hex)


popular_colors %>%
   mutate(RGB_hex = factor(RGB_hex, levels = RGB_hex)) %>%
   ggplot() +
   geom_col(aes(RGB_hex, n, fill = RGB_hex), show.legend = TRUE) +
   coord_flip() +
   scale_fill_manual(values = popular_colors_palete)


image.df_all %>%
   count(RGB_kmeans_hex) %>%
   ungroup() %>%
   top_n(32) %>%
   arrange(desc(n))


# to view image after color quantization - unncomment below lines
# # reconstitute the segmented image in the same shape as the input image
image.segmented <- array(dim=c(dimx, dimy, 3))
image.segmented[,,1] <- matrix(image.df_all$R_kmeans, nrow=dimx)
image.segmented[,,2] <- matrix(image.df_all$G_kmeans, nrow=dimx)
image.segmented[,,3] <- matrix(image.df_all$B_kmeans, nrow=dimx)
image.segmented <- image.segmented/255

# View the result
plot.new()
rasterImage(image.segmented, 0, 0, 1, 1)


pca <- princomp(image.df_all[, c("R", "G", "B")])
pca <- pca$scores
colnames(pca) <- c("PC1", "PC2", "PC3")

normalize <- function(x) {
   return((x-min(x))/(max(x)-min(x)))
}

pca <- normalize(round(pca))

<<<<<<< HEAD
image.pca <- array(dim=c(dimx, dimy, 3))
=======
image.pca <- array(dim==c(dimx, dimy, 3))
>>>>>>> 0bae33bb4007bbd5a4fdcf9b36ec492795ae6d88
image.pca[,,1] <- matrix(pca[, 1], nrow=dimx)
image.pca[,,2] <- matrix(pca[, 2], nrow=dimx)
image.pca[,,3] <- matrix(pca[, 3], nrow=dimx)

# View the result
plot.new()
rasterImage(image.pca, 0, 0, 1, 1)



# wez kazdy obraz po kolei
# przeksztalc bitmape na ramke x, y, r, g, b
# scal rgb do kolorow w hex
# zrob kmeans() do 32 kolorow z r, g, b
# policz najbardziej dominujacy kolor - wg hex i wg kmeans
# zapisz w ramce rok, dominujacy kolor kmeans i kolor hex
# narysuj barplot rok, 1, fill=kolor w wersji z hex i wersji z kmeans
# w wiki sprawdz kiedy byl jaki okres, dodaj obpowiednie linie na wykresach
# wez jakis charakretystyczny obraz i zrob mu rozne filtry :)
# na przyklad PCA na kolorach :)
