dataset <- read.csv("anime-filtered.csv")
summary(dataset)

# Mamy 25 kolumn, w których mamy zarówno kolumny które można potraktować 
# jako factor, jak i dane ilościowe oraz jakoŚciowe. 

str(dataset)

install.packages("mice")
library(mice)
md.pattern(dataset)

# Dataset ze strony Kaggle zawierał dwa warianty: raw i filtrowany. Zdecydowałem się operować na filtrowanym, ponieważ raw posiadał
# ponad 24000 rekordów, a blisko połowa z nich to tytuły które nigdy nie wyszły lub zostały zapomniane przez społecznoŚć
# Obecny posiada jedynie śladowe braki w kolumnie "Rating" co pozostawiam do późniejszego rozpatrzenia w ciągu poniższego EDA.

# Przechodzę do konwersji datasetu do postaci, w której będę mógł nim swobodnie manipulować
# Mam kolumny, które proszą się o konwersję na factor.

dataset$Type <- as.factor(dataset$Type)
dataset$Studios <- as.factor(dataset$Studios)

# Ale mam też takie, które choć bym chciał, nie jestem w stanie przekształcić w nieinwazyjny sposób. Kolumny takie jak:

dataset$Producers
dataset$Licensors

# I szczególnie:

dataset$Genres

# Posiadają wiele rekordów rodzielonych przecinkami, więc zdecydowałem się podczas badań na nich, zbudować funkcję, która zadziała
# na zasadzie "LIKE % %" w SQL. To pozwoli mi bez większych problemów mimo wszystko odszukać rekordy, które zawierają nie tylko tą
# kategorię, po której filtruję

like <- function(text, searched_string){
  words <- strsplit(text, ",")[[1]]
  words <- trimws(words)
  return(searched_string %in% words)
}

# Funkcja zakłada, że po przecinkach będzie znajdować się dokładnie takie słowo, jakiego szukamy, nie przyjmuje niestety wycinków nazw podobnie
# jak LIKE w SQL, jednak dobrze, że w ogóle mogę się z tym w ten sposób uporać


install.packages("ggplot")
