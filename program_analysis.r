library("quanteda")
library("readtext")
library("stopwords")
library("quanteda.tidy")
library("quanteda.textplots")
library("quanteda.textstats")
library("dplyr")
library("tidyr")
library("readr")
library("ggplot2")
library("ggfortify")

#working directory
data_dir <- "/Users/paulkeydel/Documents/coding projects/telegram/"

#load main data.frame and add variables
rawdata <- readtext(file = paste0(data_dir, "afd_manifestos.tsv"),
                    docid_field = "id",
                    text_field = "text"
)
stopifnot(length(unique(rawdata$doc_id)) == nrow(rawdata))
rawdata <- rawdata %>% mutate(region = sapply(state, switch,
                              TH = "ost",
                              SN = "ost",
                              ST = "ost",
                              BB = "ost",
                              MV = "ost",
                              NRW = "west",
                              BY = "west",
                              SH = "west",
                              SL = "west",
                              RLP = "west",
                              BW = "west",
                              HE = "west",
                              NI = "west",
                              HH = "west",
                              HB = "west",
                              BE = "Berlin")
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
corp_tkns <<- corp |> tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_numbers = TRUE)


#wordclouds grouped by category in dictionary
wordcloud_unique_words_subdicts <- function(min_count) {
    dict_len <- length(rpc_dict)
    dfmat <- NULL
    for (i in c(1:dict_len)) {
        complement_set <- c()
        for (j in c(1:dict_len)) {
            if (j == i) {
                next()
            }
            complement_set <- union(complement_set, unname(unlist(rpc_dict[j])))
        }
        dfm_i <- corp_tkns |>
            tokens_select(pattern = rpc_dict[i]) |>
            tokens_select(pattern = complement_set, selection = "remove") |>
            dfm(tolower = TRUE) |>
            dfm_remove(pattern = stopwords("german"))
        docvars(dfm_i, "cat") <- names(rpc_dict[i])
        dfm_i <- dfm_group(dfm_i, groups = cat)
        if (i == 1) {
            dfmat <- dfm_i
        } else {
           dfmat <- rbind(dfmat, dfm_i)
        }
    }
    set.seed(100)
    textplot_wordcloud(dfmat,
                    min_count = min_count,
                    random_order = FALSE,
                    rotation = 0.25,
                    comparison = TRUE,
                    color = RColorBrewer::brewer.pal(4, "Dark2")
    )
    title(main = "manifestos: word frequency within disjoint sub-dictionaries")
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
            main = "manifestos: application of grouped dictionary",
            col = RColorBrewer::brewer.pal(3, "Blues")
    )
    text(bp, fstat$frequency / total + 0.1, paste0(round(100 * fstat$frequency / total, 2), "%"), cex = 1)
}

#plot the distribution of subdicts per state and term, calc temporal differences per states
plot_dist_subdicts_per_state <- function() {
    dfmat_0 <- corp_tkns |>
        tokens_lookup(dictionary = rpc_dict) |>
        dfm(tolower = TRUE) |>
        dfm_remove(pattern = stopwords("german"))
    dfmat_13_18 <- dfm_subset(dfmat_0, year <= 2018) |>
        dfm_group(groups = state) |>
        dfm_weight(scheme = "prop")
    dist_13_18 <- t(convert(dfmat_13_18, to = "matrix"))
    names(dimnames(dist_13_18)) <- NULL
    barplot(dist_13_18,
        col = RColorBrewer::brewer.pal(4, "Blues"),
        legend.text = rownames(dist_13_18),
        names.arg = paste0(docnames(dfmat_13_18), docvars(dfmat_13_18, "year") %% 100),
        ylim = c(0, 1),
        main = "dictionary performed by state: manifestos 2013 - 2018",
        beside = TRUE
    )
    dfmat_18_23 <- dfm_subset(dfmat_0, year > 2018) |>
        dfm_group(groups = state) |>
        dfm_weight(scheme = "prop")
    dist_18_23 <- t(convert(dfmat_18_23, to = "matrix"))
    names(dimnames(dist_18_23)) <- NULL
    barplot(dist_18_23,
        col = RColorBrewer::brewer.pal(4, "Blues"),
        legend.text = rownames(dist_18_23),
        names.arg = paste0(docnames(dfmat_18_23), docvars(dfmat_18_23, "year") %% 100),
        cex.names = 0.82,
        ylim = c(0, 1),
        main = "dictionary performed by state: manifestos 2018 - 2023",
        beside = TRUE
    )
    #plot temporal differences
    diff_vectors <- dist_18_23[, colnames(dist_18_23) %in% colnames(dist_13_18)] - dist_13_18
    barplot(diff_vectors,
        col = RColorBrewer::brewer.pal(4, "Blues"),
        legend.text = rownames(dist_13_18),
        names.arg = docnames(dfmat_13_18),
        ylim = c(-0.06, 0.06),
        main = "state-wise differences between election periods",
        beside = TRUE
    )
    return(list("diff_matrix" = t(diff_vectors), "linked_regions" = docvars(dfmat_13_18, "region")))
}

#apply a principal component analysis to the temporal differences in order to detect clusters
plot_pca_diff_matrix <- function(diff_matrix, linked_regions) {
    diff_vec_df <- as.data.frame(diff_matrix)
    diff_vec_df <- cbind(diff_vec_df, region = linked_regions)
    diff_vec_df$region[diff_vec_df$region == "ost"] <- "East Germany"
    diff_vec_df$region[diff_vec_df$region == "west"] <- "West Germany"
    res_prc <- prcomp(diff_vec_df[, c(1:4)])
    autoplot(res_prc,
            data = diff_vec_df,
            colour = "region",
            label = TRUE,
            shape = FALSE,
            label.size = 4,
            loadings = TRUE,
            loadings.label = TRUE,
            loadings.colour = "blue") +
        ggtitle("Principal component analysis of the antagonism differences")
}

#distribution of anatogonisms over election year, plus linear regression
temporal_plot_subdicts <- function(geo_region = "") {
    dfmat <- corp_tkns |>
        tokens_lookup(dictionary = rpc_dict) |>
        dfm(tolower = TRUE) |>
        dfm_remove(pattern = stopwords("german"))
    if (geo_region == "") {
        dfmat <- dfmat |>
            dfm_weight(scheme = "prop")
    } else {
       dfmat <- dfmat |>
            dfm_subset(region == geo_region) |>
            dfm_weight(scheme = "prop")
    }
    t <- convert(dfmat, to = "data.frame")
    t <- cbind(year = docvars(dfmat, "year"), t)
    t <- select(t, -doc_id)
    t <- gather(t, "antagonism", "percentage", -year)
    ggplot(data = t, aes(x = year,  y = percentage, color = antagonism)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        ggtitle(paste("Temporal analysis: AfD manifestos in", ifelse(geo_region == "ost", "East-Germany", "West-Germany")))
}

#wordclouds of temporal differences in specific subdicts
wordcloud_east_west_diffs_subdicts <- function(max_words) {
    dfm_dict3 <- corp_tkns |>
            tokens_select(pattern = rpc_dict[3]) |>
            dfm(tolower = TRUE) |>
            dfm_remove(pattern = stopwords("german"))
    d1 <- dfm_subset(dfm_dict3, region == "ost" & year > 2018)
    d2 <- dfm_subset(dfm_dict3, region == "ost" & year <= 2018)
    set.seed(100)
    par(mar = c(2, 2, 2, 2))
    textplot_wordcloud(as.dfm(d1 - dfm_match(d2, features = featnames(d1))),
                    max_words = max_words,
                    random_order = FALSE,
                    rotation = 0.25,
                    color = "cornflowerblue"
    )
    title(main = "Anti-immigrant antagonism in East-Germany: differences to the election before")
    dfm_dict1 <- corp_tkns |>
            tokens_select(pattern = rpc_dict[1]) |>
            dfm(tolower = TRUE) |>
            dfm_remove(pattern = stopwords("german"))
    #the following lines exclude the incomplete manifesto pairs
    state_tbl <- table(dfm_dict1$state)
    dfm_dict1 <- dfm_subset(dfm_dict1, !(state %in% names(state_tbl)[state_tbl != 2]))
    d1 <- dfm_subset(dfm_dict1, region == "west" & year > 2018)
    d2 <- dfm_subset(dfm_dict1, region == "west" & year <= 2018)
    set.seed(100)
    textplot_wordcloud(as.dfm(d1 - dfm_match(d2, features = featnames(d1))),
                    max_words = max_words,
                    random_order = FALSE,
                    rotation = 0.25,
                    color = "cornflowerblue"
    )
    title(main = "Anti-elite antagonism in West-Germany: differences to the election before")
    par(mar = c(5.1, 4.1, 4.1, 2.1))
}


wordcloud_unique_words_subdicts(56)

plot_dist_grouped_subdicts()

diff_obj <- plot_dist_subdicts_per_state()
#k-means clustering in two groups
biclust <- kmeans(diff_obj$diff_matrix, 2)
sort(biclust$cluster)
plot_pca_diff_matrix(diff_obj$diff_matrix, diff_obj$linked_regions)

temporal_plot_subdicts("west")
temporal_plot_subdicts("ost")

wordcloud_east_west_diffs_subdicts(60)