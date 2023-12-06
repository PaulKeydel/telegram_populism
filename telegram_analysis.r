library("quanteda")
library("readtext")
library("stopwords")
library("quanteda.tidy")
library("quanteda.textplots")
library("quanteda.textstats")
library("dplyr")
library("tidyr")
library("readr")

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

#load and construct dictionaries
#for analysis we use two different dicts from
#*Matthijs Rooduijn & Teun Pauwels: Measuring Populism
#*RPC-Lex: https://osf.io/s48cj/?view_only=
dict_RTideo <- unlist(c(
    "elit*", "konsens*", "undemokrat*", "referend*", "korrupt*", "propagand*", "politiker*", "täusch*", "betrug*", "betrüg*", "*verrat*", "scham*", "schäm*", "skandal*", "wahrheit*", "unfair*", "establishm*", "*herrsch*", "unehrlich*")
)
rooduijn_dict <- dictionary(list(ideology_RT = dict_RTideo))
#load RPC dictionary
rpc_dict_df <- read_delim(paste0(data_dir, "/rpc_lex.csv"),
                          delim = ";", locale = locale(decimal_mark = ",")
)
unique(rpc_dict_df$category_en)
dict_protest <- unlist(rpc_dict_df %>%
    filter(category_en == "Protest/rebellion") %>%
    select(term))
dict_nationalism <- unlist(rpc_dict_df %>%
    filter(category_en == "Nationalism") %>%
    select(term))
dict_antielitism <- unlist(rpc_dict_df %>%
    filter(category_en == "Anti-elitism") %>%
    select(term))
dict_antiimmigrant <- unlist(rpc_dict_df %>%
    filter(category_en == "Anti-immigration/islamophobia") %>%
    select(term))
dict_conspiracy <- unlist(rpc_dict_df %>%
    filter(category_en == "Conspiracy") %>%
    select(term))
dict_movement <- base::setdiff(dict_protest, dict_conspiracy)
dict_ideology <- base::union(base::setdiff(dict_nationalism, dict_antiimmigrant), dict_RTideo)
#dict_movement <- base::intersect(dict_protest, dict_antielitism)
#dict_ideology <- base::intersect(dict_nationalism, dict_antielitism)
intrsct <- base::intersect(dict_movement, dict_ideology)
dict_movement <- base::setdiff(dict_movement, intrsct)
dict_ideology <- base::setdiff(dict_ideology, intrsct)
rpc_dict_df <- data.frame(word = c(dict_movement, dict_ideology),
                          sentiment = c(rep("movement", length(dict_movement)), rep("ideology", length(dict_ideology))))
rpc_dict <- as.dictionary(rpc_dict_df, tolower = TRUE)

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

#create corpus and the corpus that is tokenized
corp <- corpus(rawdata)
corp_tkns <- corp |> tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE)

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
dfmat <- corp_tkns |>
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
dict <- rpc_dict
#dict <- c(rpc_dict["movement"], rooduijn_dict["ideology_RT"])
dfmat_0 <- corp_tkns |>
    tokens_lookup(dictionary = dict) |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german"))
par(mfrow = c(1, 2))
#group the feature matrix by c(ost, west, bund)
dfmat <- dfmat_0 |> dfm_group(groups = region) |>
    dfm_weight(scheme = "prop")
print(dfmat)
text_freq <- textstat_frequency(dfmat, groups = region) %>%
    select(feature, frequency, group) %>%
    spread(key = group, value = frequency)
t <- as.matrix(select(text_freq, bund, ost, west))
colnames(t) <- c("Bund", "Ost", "West")
rownames(t) <- text_freq$feature
barplot(t,
    col = c("lightblue", "#bae4ba"),
    legend.text = text_freq$feature,
    ylim = c(0, 1),
    beside = TRUE)
#group the feature matrix by states
dfmat <- dfmat_0 |> dfm_group(groups = state) |>
    dfm_weight(scheme = "prop")
print(dfmat)
text_freq <- textstat_frequency(dfmat, groups = state) %>%
    select(feature, frequency, group) %>%
    spread(key = group, value = frequency)
t <- as.matrix(select(text_freq, -feature, -Bund))
rownames(t) <- text_freq$feature
barplot(t,
    col = c("lightblue", "#bae4ba"),
    legend.text = text_freq$feature,
    ylim = c(0, 1),
    beside = TRUE)