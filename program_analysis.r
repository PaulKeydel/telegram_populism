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
rawdata <- readtext(file = paste0(data_dir, "afd_wahlprogramm.tsv"),
                    docid_field = "state",
                    text_field = "text"
)
stopifnot(length(unique(rawdata$doc_id)) == nrow(rawdata))
rawdata <- rawdata %>% mutate(region = sapply(doc_id, switch,
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
rpc_dict_df <- select(rpc_dict_df, term, category_en)
rpc_dict_df <- filter(rpc_dict_df, category_en %in% selected_subdicts)
colnames(rpc_dict_df) <- c("word", "sentiment")
rpc_dict <<- as.dictionary(rpc_dict_df, tolower = TRUE)

#create corpus and the corpus that is tokenized
corp <<- corpus(rawdata)
corp_tkns <<- corp |> tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE)

#wordclouds between east and west
wordcloud_east_west <- function(max_words) {
    dfmat <- corp_tkns |>
        dfm(tolower = TRUE) |>
        dfm_remove(pattern = stopwords("german"))
    #print(topfeatures(dfmat, groups = region, 20))
    set.seed(100)
    textplot_wordcloud(dfm_subset(dfmat, region == "ost"),
                    min_count = 4,
                    max_words = max_words,
                    random_order = FALSE,
                    rotation = 0.25,
                    color = RColorBrewer::brewer.pal(8, "Dark2")
    )
    set.seed(100)
    textplot_wordcloud(dfm_subset(dfmat, region == "west"),
                    min_count = 4,
                    max_words = max_words,
                    random_order = FALSE,
                    rotation = 0.25,
                    color = RColorBrewer::brewer.pal(8, "Dark2")
    )
}

#determine probability of defined dictionaries and print them in grouped barplots
plot_prob_defined_subdicts <- function() {
    dfmat_0 <- corp_tkns |>
        tokens_lookup(dictionary = rpc_dict) |>
        dfm(tolower = TRUE) |>
        dfm_remove(pattern = stopwords("german"))
    #par(mfrow = c(1, 2))
    #group the feature matrix by c(ost, west, bund)
    dfmat <- dfmat_0 |> dfm_group(groups = region) |>
        dfm_weight(scheme = "prop")
    #print(dfmat)
    text_freq <- textstat_frequency(dfmat, groups = region) %>%
        select(feature, frequency, group) %>%
        spread(key = group, value = frequency)
    t <- as.matrix(select(text_freq, ost, west))
    colnames(t) <- c("Ost", "West")
    rownames(t) <- text_freq$feature
    barplot(t,
        #col = c("lightblue", "#bae4ba"),
        legend.text = text_freq$feature,
        ylim = c(0, 1),
        main = "election programmes",
        beside = TRUE)
    #group the feature matrix by states
    dfmat <- dfmat_0 |> dfm_weight(scheme = "prop")
    barplot(t(convert(dfmat, to = "matrix")),
        #col = c("lightblue", "#bae4ba"),
        legend.text = text_freq$feature,
        ylim = c(0, 1),
        main = "election programmes",
        beside = TRUE)
}


wordcloud_east_west(60)

plot_prob_defined_subdicts()


dfmat_3 <- corp_tkns |>
    dfm(tolower = TRUE) |>
    dfm_remove(pattern = stopwords("german")) |>
    dfm_group(groups = region)
print(dfmat_3, max_nfeat = 20)
textstat_simil(dfmat_3, method = "correlation", margin = "documents")