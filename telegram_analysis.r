library("quanteda")
library("readtext")
library("stopwords")
library("quanteda.tidy")
library("quanteda.textplots")
library("quanteda.textstats")

#load corpus from files
data_dir <- "/Users/paulkeydel/Documents/coding projects/telegram"
rawdata <- readtext(file = paste0(data_dir, "/telegram_data.tsv"),
                    text_field = "text",
                    docid_field = "creat_time"
)
corp <- corpus(rawdata)

docvars(corp, "region") <- sapply(docvars(corp, "state"), switch,
                                  TH = 'ost', 
                                  NRW = 'west', 
                                  Bund = 'bund', 
)
docvars(corp, "react_rate") <- corp$likes / corp$views

corp_sort <- arrange(corp, desc(react_rate))
summary(corp_sort)

dfmat <- corp_sort[1:10] |>
    tokens(remove_punct = TRUE) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german"))
topfeatures(dfmat, 40)
set.seed(100)
textplot_wordcloud(dfmat, min_count = 3, random_order = FALSE, rotation = 0.25, color = RColorBrewer::brewer.pal(8, "Dark2"))

data_tokens <- tokens(corp)
kwic(data_tokens, pattern = "deutsch*", valuetype = "regex")

dict <- dictionary(list(speech = c("volk", "elit*", "undemokrat*", "betrug", "verrat*", "*lüge*", "wahrheit"),
                        migration = c("migrat*", "ausländer", "grenzen", "abschieben"),
                        names = c("weidel", "chrupalla", "höcke", "krah"))
)
dfmat <- corp |>
    tokens(remove_punct = TRUE) |>
    tokens_lookup(dictionary = dict) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german")) |>
    dfm_group(groups = region) |>
    dfm_weight(scheme = "prop")
print(dfmat)