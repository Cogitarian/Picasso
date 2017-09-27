library(tidyverse)
library(jsonlite)


setwd("~/RProjects/Picasso")


# folder na pliki graficzne - tworzymy jesli nie istnieje
if(!file.exists("pics")) dir.create("pics")


### Lista obrazów

url <- "https://www.wikiart.org/en/pablo-picasso/mode/all-paintings?json=2&resultType=text&page="

paintings_list <- tibble()

# 1129 obrazów, po 200 na stronie = 6 stron
for(page_no in 1:6) {
   # zbuduj urla do JSONa
   page_url <- paste0(url, page_no)

   # pobierz JSONa
   json <- fromJSON(page_url)

   # weź tylko interesujące dane
   paintings_tmp <- json$Paintings

   # dodaj do pełnej listy
   paintings_list <- paintings_list %>% bind_rows(paintings_tmp)
}

# bierzemy tylko potrzebne nam dane i zmieniamy rok na wartość liczbową
# usuwamy to co nie udało się zamienić (9 sztuk)
paintings <- paintings_list %>%
   select(id, title, year, image) %>%
   mutate(year = as.numeric(year)) %>%
   filter(!is.na(year))


# pobieramy wszystkie obrazy
download_paint <- function(paint) {
   # url do obrazu
   paint_url <- URLencode(as.character(paint$image))

   # ścieżka do pliku lokalnego
   # () zamienione na _
   dest_path <- paste0("pics/", basename(gsub("[\\(\\)]", "_", paint_url)))

   # pobierz plik i zapisz lokalnie
   download.file(paint_url, destfile = dest_path)

   # oddaj ścieżkę do pliku lokalnego
   return(dest_path)
}

# dla każdego wiersza wywołaj funkcję i wynik jej działania umieść w kolumnie "path"
paintings <- paintings %>%
   by_row(download_paint, .collate = "cols", .to = "path")

# zapisz listę obrazów na dysku
saveRDS(paintings, file = "paintings.RDS")
