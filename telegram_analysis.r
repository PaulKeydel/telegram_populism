library("quanteda")
library("readtext")
library("stopwords")
library("quanteda.tidy")
library("quanteda.textplots")
library("quanteda.textstats")
library("dplyr")
library("tidyr")
library("readr")

#working directory
data_dir <- "/Users/paulkeydel/Documents/coding projects/telegram/"

#load main data.frame, add variables and load texts for dict validation
rawdata <- readtext(file = paste0(data_dir, "telegram_data.tsv"),
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
rawdata <<- mutate(rawdata, react_rate = rawdata$likes / rawdata$views)


#load right-wing dictionariy from literature
#for detecting different categories of the populist dicourse we use the RPC-Lex
#RPC-Lex: https://osf.io/s48cj/?view_only= from Oliver Puschmann
RPC_subdicts <- list(
    c("Scandalization", 0),
    c("Suspicion/manipulation ", 0),
    c("Exposure/revelation", 0),
    c("Markers of distance", 0),
    c("Conspiracy", 0),
    c("Antisemitism", 1),
    c("Anti-elitism", 1),
    c("Apocalypse/downfall ", 0),
    c("Protest/rebellion", 0),
    c("Nationalism", 0),
    c("Anti-immigration/islamophobia", 1),
    c("Anti-gender/anti-feminism", 1),
    c("Esotericism", 0)
)
selected_subdicts <- list()
for (i in c(1:length(RPC_subdicts))) {
    if (RPC_subdicts[[i]][[2]] == "1") {
        selected_subdicts <- c(selected_subdicts, RPC_subdicts[[i]][[1]])
    }
}
#load RPC dictionary
rpc_dict_df <- read_delim(paste0(data_dir, "/rpc_lex.csv"),
                          delim = ";", locale = locale(decimal_mark = ",")
)
rpc_dict_df <- mutate(rpc_dict_df,
    perspective = case_when(category_en == "Scandalization" ~ "style",
                            category_en == "Suspicion/manipulation " ~ "style",
                            category_en == "Exposure/revelation" ~ "style",
                            category_en == "Markers of distance" ~ "style",
                            category_en == "Conspiracy" ~ "topoi",
                            category_en == "Antisemitism" ~ "antagonisms",
                            category_en == "Anti-elitism" ~ "antagonisms",
                            category_en == "Apocalypse/downfall " ~ "topoi",
                            category_en == "Protest/rebellion" ~ "topoi",
                            category_en == "Nationalism" ~ "topoi",
                            category_en == "Anti-immigration/islamophobia" ~ "antagonisms",
                            category_en == "Anti-gender/anti-feminism" ~ "antagonisms",
                            category_en == "Esotericism" ~ "topoi")
    )
rpc_dict_df_0 <- select(rpc_dict_df, term, category_en)
rpc_dict_df_0 <- filter(rpc_dict_df_0, category_en %in% selected_subdicts)
colnames(rpc_dict_df_0) <- c("word", "sentiment")
rpc_dict <<- as.dictionary(rpc_dict_df_0, tolower = TRUE)

rpc_dict_df_1 <- select(rpc_dict_df, term, perspective)
colnames(rpc_dict_df_1) <- c("word", "sentiment")
rpc_dict_grouped <<- as.dictionary(rpc_dict_df_1, tolower = TRUE)

#create corpus and the corpus that is tokenized
corp <<- corpus(rawdata)
corp_tkns <<- corp |> tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE)

#distribution of messages in states
plot_msgs_state_dist <- function() {
    messages_per_state <- rawdata %>%
        group_by(state) %>%
        summarise(num = n())
    barplot(height = messages_per_state$num,
            names = messages_per_state$state,
            main = "Telegram: text corpus by state",
            xlab = "federate state/Bundesland",
            ylab = "messages",
            ylim = c(0, 50 + max(messages_per_state$num)),
            col = "#8EBBE4"
    )
}

#calc wordcloud for biggest reaction rates
wordcloud_react_rate <- function(max_words) {
    corp_sort <- arrange(corp, desc(react_rate))
    summary(corp_sort)
    dfmat <- corp_sort[1:50] |>
        tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE) |>
        dfm(tolower = TRUE) |>
        dfm_remove(pattern = stopwords("german"))
    set.seed(100)
    textplot_wordcloud(dfmat,
                    min_count = 3,
                    max_words = max_words,
                    random_order = FALSE,
                    rotation = 0.25,
                    color = RColorBrewer::brewer.pal(8, "Dark2")
    )
    title(main = "Telegram: wordcloud of highest feedbacked messages")
}

#wordclouds between east and west
wordcloud_east_west <- function(max_words) {
    dfmat <- corp_tkns |>
        dfm(tolower = TRUE) |>
        dfm_remove(pattern = stopwords("german"))
    topfeatures(dfmat, groups = region, 20)
    set.seed(100)
    textplot_wordcloud(dfm_subset(dfmat, region == "ost"),
                    min_count = 4,
                    max_words = max_words,
                    random_order = FALSE,
                    rotation = 0.25,
                    color = RColorBrewer::brewer.pal(8, "Dark2")
    )
    title(main = "Telegram: significant words in East Germany")
    set.seed(100)
    textplot_wordcloud(dfm_subset(dfmat, region == "west"),
                    min_count = 4,
                    max_words = max_words,
                    random_order = FALSE,
                    rotation = 0.25,
                    color = RColorBrewer::brewer.pal(8, "Dark2")
    )
    title(main = "Telegram: significant words in West Germany")
}

#plot distribution of grouped dictionary
plot_dist_grouped_subdicts <- function() {
    dfmat <- corp_tkns |>
        tokens_lookup(dictionary = rpc_dict_grouped) |>
        dfm(tolower = TRUE) |>
        dfm_remove(pattern = stopwords("german"))
    fstat <- textstat_frequency(dfmat)
    total <- sum(fstat$frequency)
    bp <- barplot(height = fstat$frequency / total,
            names.arg = fstat$feature,
            ylim = c(0, 1),
            main = "Telegram: application of grouped dictionary",
            col = RColorBrewer::brewer.pal(3, "Blues")
    )
    text(bp, fstat$frequency / total + 0.1, paste0(round(100 * fstat$frequency / total, 2), "%"), cex = 1)
}

#plot distribution of defined dictionaries and print them in grouped barplots
plot_dist_defined_subdicts <- function() {
    dfmat_0 <- corp_tkns |>
        tokens_lookup(dictionary = rpc_dict) |>
        dfm(tolower = TRUE) |>
        dfm_remove(pattern = stopwords("german"))
    #par(mfrow = c(1, 2))
    #group the feature matrix by c(ost, west, bund)
    dfmat <- dfm_group(dfmat_0, groups = region) |>
        dfm_weight(scheme = "prop")
    #print(dfmat)
    text_freq <- textstat_frequency(dfmat, groups = region) %>%
        select(feature, frequency, group) %>%
        spread(key = group, value = frequency)
    t <- as.matrix(select(text_freq, bund, ost, west))
    colnames(t) <- c("Bund", "Ost", "West")
    rownames(t) <- text_freq$feature
    barplot(t,
        col = RColorBrewer::brewer.pal(4, "Blues"),
        legend.text = text_freq$feature,
        ylim = c(0, 1),
        main = "Telegram: dictionary performed by East-West division",
        beside = TRUE
    )
    #group the feature matrix by states
    dfmat <- dfm_group(dfmat_0, groups = state) |>
        dfm_weight(scheme = "prop")
    #print(dfmat)
    text_freq <- textstat_frequency(dfmat, groups = state) %>%
        select(feature, frequency, group) %>%
        spread(key = group, value = frequency)
    t <- as.matrix(select(text_freq, -feature, -Bund))
    rownames(t) <- text_freq$feature
    barplot(t,
        col = RColorBrewer::brewer.pal(4, "Blues"),
        legend.text = text_freq$feature,
        ylim = c(0, 1),
        main = "Telegram: dictionary performed by state",
        beside = TRUE
    )
}


#number of collected messages in east, west, federal
rawdata %>% group_by(region) %>% summarise(n = n(), r = n() / nrow(rawdata))

plot_msgs_state_dist()

wordcloud_react_rate(40)

wordcloud_east_west(60)

plot_dist_grouped_subdicts()

plot_dist_defined_subdicts()