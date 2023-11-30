library("quanteda")
library("readtext")
library("stopwords")
library("quanteda.tidy")
library("quanteda.textplots")
library("quanteda.textstats")
library("dplyr")
library("tidyr")

#load data.frame from file and add variables
data_dir <- "/Users/paulkeydel/Documents/coding projects/telegram"
rawdata <- readtext(file = paste0(data_dir, "/telegram_data.tsv"),
                    text_field = "text",
                    docid_field = "creat_time"
)
stopifnot(length(unique(rawdata$doc_id)) == nrow(rawdata))
rawdata <- rawdata %>% mutate(region = sapply(state, switch,
                              TH = "ost",
                              SN = "ost",
                              BB = "ost",
                              NRW = "west",
                              BY = "west",
                              SH = "west",
                              SL = "west",
                              RLP = "west",
                              Bund = "bund")
)
rawdata <- mutate(rawdata, react_rate = rawdata$likes / rawdata$views)

#number of collected messages in east, west, federal
rawdata %>% group_by(region) %>% summarise(n = n(), r = n() / nrow(rawdata))

#distribution of messages in states
messages_per_state <- rawdata %>%
    group_by(state) %>%
    summarise(num = n())
barplot(height = messages_per_state$num,
        names = messages_per_state$state,
        main = "Textkorpus nach Bundesländern",
        xlab = "Bundesland",
        ylab = "Nachrichten",
        ylim = c(0, 80),
        col = "#8EBBE4"
)

#create corpus
corp <- corpus(rawdata)

#calc wordcloud for biggest reaction rates
corp_sort <- arrange(corp, desc(react_rate))
summary(corp_sort)
dfmat <- corp_sort[1:10] |>
    tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german"))
set.seed(100)
textplot_wordcloud(dfmat, min_count = 3, random_order = FALSE, rotation = 0.25, color = RColorBrewer::brewer.pal(8, "Dark2"))

#topfeatures and wordclouds between east, west, federal
dfmat <- corp |>
    tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german"))
topfeatures(dfmat, groups = region, 20)
set.seed(100)
textplot_wordcloud(dfm_subset(dfmat, region == "ost"),
                   min_count = 4,
                   max_words = 40,
                   random_order = FALSE,
                   rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2")
)
set.seed(100)
textplot_wordcloud(dfm_subset(dfmat, region == "west"),
                   min_count = 4,
                   max_words = 40,
                   random_order = FALSE,
                   rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2")
)

#show textual context
head(kwic(tokens(corp), pattern = "deutsch*", valuetype = "regex"))

#determine probability of defined dictionaries and print them in grouped barplots
dict <- dictionary(list(ideology = c("volk", "elit*", "undemokrat*", "referend*", "betrug", "verrat*", "*lüge*", "wahrheit", "establishm*", "*herrsch*", "politiker*"),
                        movement = c("weidel", "chrupalla", "höcke", "storch", "gauland", "zusammen", "gemeinsam", "protest", "mehrheit", "jagen", "gegenbewegung"))
)
#sources dictionaries:
#Matthijs Rooduijn & Teun Pauwels: Measuring Populism
#RCP-Lex: https://osf.io/s48cj/?view_only=
dfmat <- corp |>
    tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE) |>
    tokens_lookup(dictionary = dict) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german")) |>
    dfm_group(groups = region) |>
    dfm_weight(scheme = "prop")
print(dfmat)
text_freq <- textstat_frequency(dfmat, groups = region) %>%
    select(feature, frequency, group) %>%
    spread(key = group, value = frequency)
t <- as.matrix(select(text_freq, bund, ost, west))
colnames(t) <- c("Bund", "Ost", "West")
rownames(t) <- text_freq$feature
t
barplot(t,
    col = c("lightblue", "#bae4ba"),
    legend.text = text_freq$feature,
    ylim = c(0, 1),
    beside = TRUE)