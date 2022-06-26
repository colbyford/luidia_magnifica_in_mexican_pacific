## StrainHub Network Generation

## Load in Libraries
library(strainhub)
library(visNetwork)
library(dplyr)
library(readr)
library(randomcoloR)

### Galvan 16 - 54 Taxa
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("Galvan_16S_54taxa/RAxML_bestTree.LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa.tre")
metadata <- readr::read_csv("Galvan_16S_54taxa/LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa16s.csv", col_names = TRUE)

selected_meta = "Body_of_Water_formatted"
# ancestralStates = asr_max_parsimony(treedata,
#                                     as.numeric(as.factor(metadata[[selected_meta]])),
#                                     length(unique(metadata[[selected_meta]])))
# 
# 
# asr <- ancestralStates$ancestral_likelihoods %>%
#   data.frame() %>%
#   mutate(max_col_first = max.col(., ties.method = "first"),
#          max_col_last =  max.col(., ties.method = "last"),
#          ambig = ifelse(max_col_first == max_col_last, FALSE, TRUE),
#          # taxa = treedata$tip.label[1:(length(treedata$tip.label)-1)]
#          taxa = rev(treedata$tip.label[1:(length(treedata$tip.label)-1)])
#          ) %>% 
#   select(!c(max_col_first, max_col_last))
# 
# colnames(asr) <- c(sort(unique(metadata[[selected_meta]])), "ambig", "taxa")


# non_ambig_taxa <- asr %>% filter(!ambig) %>% select(taxa) %>% .[["taxa"]]
# ambig_taxa <- asr %>% filter(ambig) %>% select(taxa) %>% .[["taxa"]]
# 
# 
# non_ambig_treedata <- treedata %>% ape::drop.tip(ambig_taxa)


## Make the Transmission Network
full_graph <- makeTransNet(treedata,
                           metadata,
                           columnSelection = selected_meta,
                           centralityMetric = 3,
                           treeType = "parsimonious")


# non_ambig_graph <- makeTransNet(non_ambig_treedata,
#                                 metadata,
#                                 columnSelection = selected_meta,
#                                 centralityMetric = 3,
#                                 treeType = "parsimonious")

# graph


# readr::write_csv(asr, "16S_asr_max_parsimony.csv")
# saveRDS(full_graph, "Galvan_16S_54taxa/graph.rds")
# full_graph <- readRDS("Galvan_16S_54taxa/graph.rds")



## Determine Ambiguous Edges
# non_ambig_df <- non_ambig_graph$x$edges %>% 
#   inner_join(non_ambig_graph$x$nodes, by=c('from'='id'), suffix = c("", "_from")) %>% 
#   inner_join(non_ambig_graph$x$nodes, by=c('to'='id'),  suffix = c("", "_to")) %>% 
#   mutate(transmission = paste0(label,">", label_to))


full_df <- full_graph$x$edges %>% 
  inner_join(full_graph$x$nodes, by=c('from'='id'), suffix = c("", "_from")) %>% 
  inner_join(full_graph$x$nodes, by=c('to'='id'),  suffix = c("", "_to")) %>% 
  mutate(transmission = paste0(label,">", label_to)) %>% 
  ## Manual Edge Selection/Ambiguity
  filter(!transmission %in% c("Almirante Bay>Bering Sea",
                              "Almirante Bay>North Pacific Ocean",
                              "Almirante Bay>Persian Gulf",
                              "Gulf of Thailand>?")) %>% 
  mutate(ambig = ifelse(label_to == "Club de Yates", TRUE, FALSE))
  

# colors_16s <- readr::read_csv("Galvan_16S_54taxa/Galvan_16S_colors.csv")
colors_greys <- c("#FFFFFF", rev(grDevices::gray.colors(100)))

nodes <- full_graph$x$nodes %>%
  mutate(shape = "dot",
         # color = color_palette
         font.size = 20) %>% 
  filter(!label %in% c("?",
                       "Bering Sea",
                       "Persian Gulf",
                       "North Pacific Ocean")) %>% 
  mutate(value_percentiles = as.integer(value/max(value)*100)+1)
  # inner_join(colors_16s, by = c("label" = "Body_of_Water_formatted"))

for (i in 1:nrow(nodes)){
  nodes$color[i] <- colors_greys[nodes$value_percentiles[i]]
}

  
edges <- full_df %>% 
    mutate(arrows = "to",
           smooth = TRUE,
           dashes = full_df$ambig,
           color = ifelse(to == 5, "red", "grey"),
           width = value) %>% 
    select(from, to, arrows, smooth, dashes, color, width)

# ambigs <- setdiff(full_df$transmission, non_ambig_df$transmission)
# 
# 
# full_df$ambig <- ifelse(full_df$transmission %in% ambigs, TRUE, FALSE)


## Recolor Nodes and Edges
# color_palette <- distinctColorPalette(nrow(full_graph$x$nodes))
# 
# nodes <- full_graph$x$nodes %>%
#   mutate(shape = "dot",
#          color = color_palette,
#          font.size = 20)
# 
# edges <- full_graph$x$edges %>%
#   mutate(arrows = "to",
#          smooth = TRUE,
#          # dashes = full_df$ambig,
#          dashes = ifelse(to == 5, TRUE, FALSE),
#          color = ifelse(to == 5, "red", "grey"),
#          width = value,
#          value = NULL)

## Generate the custom network using `visNetwork`
output_graph <- visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE))

output_graph

write_csv(output_graph$x$nodes, "16S_network_strainhub_output.csv")


# ## Recolor Nodes and Edges
# color_palette <- distinctColorPalette(nrow(non_ambig_graph$x$nodes))
# 
# nodes <- non_ambig_graph$x$nodes %>%
#   mutate(shape = "dot",
#          color = color_palette,
#          font.size = 20)
# 
# edges <- non_ambig_graph$x$edges %>%
#   mutate(arrows = "to",
#          smooth = TRUE,
#          # dashes = full_df$ambig,
#          color = ifelse(to == 5, "red", "grey"),
#          width = value,
#          value = NULL)
# 
# ## Generate the custom network using `visNetwork`
# visNetwork(nodes, edges) %>%
#   visOptions(nodesIdSelection = list(enabled = TRUE))





####################################
### Galvan COI Tree - 101 Taxa
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("Galvan_COI_101taxa/RAxML_bestTree.COI_nataly_brenda_JANESSA.DEDUP.trim.tre")
metadata <- readr::read_csv("Galvan_COI_101taxa/RAxML_bestTree.COI_nataly_brenda_JANESSA.DEDUP.trim.out_101.fix.csv", col_names = TRUE)


selected_meta = "Body_of_Water_formatted"
# ancestralStates = asr_max_parsimony(treedata,
#                                     as.numeric(as.factor(metadata[[selected_meta]])),
#                                     length(unique(metadata[[selected_meta]])))
# 
# 
# asr <- ancestralStates$ancestral_likelihoods %>%
#   data.frame() %>%
#   mutate(max_col_first = max.col(., ties.method = "first"),
#          max_col_last =  max.col(., ties.method = "last"),
#          ambig = ifelse(max_col_first == max_col_last, FALSE, TRUE),
#          # taxa = treedata$tip.label[1:(length(treedata$tip.label)-1)]
#          taxa = rev(treedata$tip.label[1:(length(treedata$tip.label)-1)])
#          ) %>% 
#   select(!c(max_col_first, max_col_last))
# 
# colnames(asr) <- c(sort(unique(metadata[[selected_meta]])), "ambig", "taxa")


# non_ambig_taxa <- asr %>% filter(!ambig) %>% select(taxa) %>% .[["taxa"]]
# ambig_taxa <- asr %>% filter(ambig) %>% select(taxa) %>% .[["taxa"]]
# 
# 
# non_ambig_treedata <- treedata %>% ape::drop.tip(ambig_taxa)


## Make the Transmission Network
full_graph <- makeTransNet(treedata,
                           metadata,
                           columnSelection = selected_meta,
                           centralityMetric = 3,
                           treeType = "parsimonious")


# non_ambig_graph <- makeTransNet(non_ambig_treedata,
#                                 metadata,
#                                 columnSelection = selected_meta,
#                                 centralityMetric = 3,
#                                 treeType = "parsimonious")

# graph

# readr::write_csv(asr, "COI_asr_max_parsimony.csv")
# saveRDS(full_graph, "Galvan_COI_101taxa/graph.rds")
# full_graph <- readRDS("Galvan_COI_101taxa/graph.rds")

## Determine Ambiguous Edges
# non_ambig_df <- non_ambig_graph$x$edges %>% 
#   inner_join(non_ambig_graph$x$nodes, by=c('from'='id'), suffix = c("", "_from")) %>% 
#   inner_join(non_ambig_graph$x$nodes, by=c('to'='id'),  suffix = c("", "_to")) %>% 
#   mutate(transmission = paste0(label,">", label_to))


full_df <- full_graph$x$edges %>% 
  inner_join(full_graph$x$nodes, by=c('from'='id'), suffix = c("", "_from")) %>% 
  inner_join(full_graph$x$nodes, by=c('to'='id'),  suffix = c("", "_to")) %>% 
  mutate(transmission = paste0(label,">", label_to)) %>% 
  ## Manual Edge Selection/Ambiguity
  filter(!transmission %in% c("Coral Sea>Queen Charlotte Islands",
                              "Gulf of Thailand>?")) %>% 
    mutate(ambig = ifelse(label_to == "Club de Yates", TRUE, FALSE))
  
# ambigs <- setdiff(full_df$transmission, non_ambig_df$transmission)
# 
# 
# full_df$ambig <- ifelse(full_df$transmission %in% ambigs, TRUE, FALSE)
  
colors_coi <- readr::read_csv("Galvan_COI_101taxa/Galvan_COI_colors.csv")
colors_greys <- c("#FFFFFF", rev(grDevices::gray.colors(100)))

nodes <- full_graph$x$nodes %>%
  mutate(shape = "dot",
         # color = color_palette
         font.size = 20) %>% 
  filter(!label %in% c("?",
                       "Queen Charlotte Islands")) %>% 
  mutate(value_percentiles = as.integer(value/max(value)*100)+1)
# inner_join(colors_coi, by = c("label" = "Body_of_Water_formatted"))

for (i in 1:nrow(nodes)){
  nodes$color[i] <- colors_greys[nodes$value_percentiles[i]]
}


edges <- full_df %>% 
  mutate(arrows = "to",
         smooth = TRUE,
         dashes = full_df$ambig,
         color = ifelse(to == 6, "red", "grey"),
         width = value) %>% 
  select(from, to, arrows, smooth, dashes, color, width)


## Recolor Nodes and Edges
# color_palette <- distinctColorPalette(nrow(full_graph$x$nodes))
# 
# nodes <- full_graph$x$nodes %>%
#   mutate(shape = "dot",
#          color = color_palette,
#          font.size = 20)
# 
# edges <- full_graph$x$edges %>%
#   mutate(arrows = "to",
#          smooth = TRUE,
#          dashes = full_df$ambig,
#          color = ifelse(to == 6, "red", "grey"),
#          width = value,
#          value = NULL)

## Generate the custom network using `visNetwork`
output_graph <- visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE))

output_graph

write_csv(output_graph$x$nodes, "COI_network_strainhub_output.csv")


## Recolor Nodes and Edges
# color_palette <- distinctColorPalette(nrow(non_ambig_graph$x$nodes))
# 
# nodes <- non_ambig_graph$x$nodes %>%
#   mutate(shape = "dot",
#          color = color_palette,
#          font.size = 20)
# 
# edges <- non_ambig_graph$x$edges %>%
#   mutate(arrows = "to",
#          smooth = TRUE,
#          # dashes = full_df$ambig,
#          color = ifelse(to == 6, "red", "grey"),
#          width = value,
#          value = NULL)
# 
# ## Generate the custom network using `visNetwork`
# visNetwork(nodes, edges) %>%
#   visOptions(nodesIdSelection = list(enabled = TRUE))
