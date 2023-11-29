library("quanteda")
library("readtext")
library("stopwords")
library("quanteda.tidy")
library("quanteda.textplots")
library("quanteda.textstats")
library("dplyr")

#load data.frame from file and add variables
data_dir <- "/Users/paulkeydel/Documents/coding projects/telegram"
rawdata <- readtext(file = paste0(data_dir, "/telegram_data.tsv"),
                    text_field = "text",
                    docid_field = "creat_time"
)
rawdata <- rawdata %>% mutate(region = sapply(state, switch,
                              TH = 'ost', 
                              NRW = 'west', 
                              Bund = 'bund')
)
rawdata <- mutate(rawdata, react_rate = rawdata$likes / rawdata$views)

#create corpus
corp <- corpus(rawdata)

#number of collected messages in east, west, federal
rawdata %>% group_by(region) %>% summarise(n = n(), r = n() / nrow(rawdata))

#calc wordcloud for biggest reaction rates
corp_sort <- arrange(corp, desc(react_rate))
summary(corp_sort)
dfmat <- corp_sort[1:10] |>
    tokens(remove_punct = TRUE, remove_symbols = TRUE) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german"))
set.seed(100)
textplot_wordcloud(dfmat, min_count = 3, random_order = FALSE, rotation = 0.25, color = RColorBrewer::brewer.pal(8, "Dark2"))

#topfeatures between east, west, federal
dfmat <- corp |>
    tokens(remove_punct = TRUE, remove_symbols = TRUE) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german")) |>
    dfm_group(groups = region)
topfeatures(dfmat, groups = region, 20)

#show textual context
kwic(tokens(corp), pattern = "deutsch*", valuetype = "regex")

#determine probability of defined dictionaries
dict <- dictionary(list(speech = c("volk", "elit*", "undemokrat*", "betrug", "verrat*", "*lüge*", "wahrheit"),
                        migration = c("migrat*", "ausländer", "grenzen", "abschieben"),
                        names = c("weidel", "chrupalla", "höcke", "krah"))
)
dfmat <- corp |>
    tokens(remove_punct = TRUE, remove_symbols = TRUE) |>
    tokens_lookup(dictionary = dict) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german")) |>
    dfm_group(groups = region) |>
    dfm_weight(scheme = "prop")
print(dfmat)