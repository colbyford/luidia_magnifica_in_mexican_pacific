## StrainHub Network Generation

## Load in Libraries
library(strainhub)
library(visNetwork)
library(dplyr)
library(readr)
library(randomcoloR)


### Galvan 16 - 54 Taxa
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("betweenness_fix_grey_16S/RAxML_bestTree.LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa.out")
metadata <- readr::read_csv("betweenness_fix_grey_16S/LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa16s.csv", col_names = TRUE)

selected_meta = "Body_of_Water_formatted"

## Make the Transmission Network
full_graph <- makeTransNet(treedata,
                           metadata,
                           columnSelection = selected_meta,
                           centralityMetric = 3,
                           treeType = "parsimonious")

## Convert Graph info to DataFrame
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


## Generate the custom network using `visNetwork`
output_graph <- visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE))

output_graph

write_csv(output_graph$x$nodes, "betweenness_fix_grey_16S/16S_network_strainhub_output.csv")



####################################
### Galvan COI Tree - 101 Taxa
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("betweenness_fix_grey_COI/RAxML_bestTree.COI_nataly_brenda_JANESSA.DEDUP.trim.out")
metadata <- readr::read_csv("betweenness_fix_grey_COI/RAxML_bestTree.COI_nataly_brenda_JANESSA.DEDUP.trim.out_101.fix.csv", col_names = TRUE)


selected_meta = "Body_of_Water_formatted"


## Make the Transmission Network
full_graph <- makeTransNet(treedata,
                           metadata,
                           columnSelection = selected_meta,
                           centralityMetric = 3,
                           treeType = "parsimonious")


## Convert Graph into to DataFrame
full_df <- full_graph$x$edges %>% 
  inner_join(full_graph$x$nodes, by=c('from'='id'), suffix = c("", "_from")) %>% 
  inner_join(full_graph$x$nodes, by=c('to'='id'),  suffix = c("", "_to")) %>% 
  mutate(transmission = paste0(label,">", label_to)) %>% 
  ## Manual Edge Selection/Ambiguity
  filter(!transmission %in% c("Coral Sea>Queen Charlotte Islands",
                              "Gulf of Thailand>?")) %>% 
  mutate(ambig = ifelse(label_to == "Club de Yates", TRUE, FALSE))


colors_coi <- readr::read_csv("betweenness_fix_grey_COI/Galvan_COI_colors.csv")
colors_greys <- c("#FFFFFF", rev(grDevices::gray.colors(100)))

nodes <- full_graph$x$nodes %>%
  mutate(shape = "dot",
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


## Generate the custom network using `visNetwork`
output_graph <- visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE))

output_graph

write_csv(output_graph$x$nodes, "betweenness_fix_grey_COI/COI_network_strainhub_output.csv")



############################
### May 2023 Taiwan Data - 25 Taxa
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("2023May_run/w-baRnfBpf1L0Yw4jdGNxQ_newick.txt")
metadata <- readr::read_csv("2023May_run/SD_TaiwanCOIv3_DJ_body_of_waterFIX2jpl2.csv", col_names = TRUE) %>% 
  mutate("Body_of_Water_letter" = paste0(Body_of_Water, " (", acro, ")"))

selected_meta = "Body_of_Water_letter"

## Make the Transmission Network
full_graph <- makeTransNet(treedata,
                           metadata,
                           columnSelection = selected_meta,
                           centralityMetric = 6,
                           treeType = "parsimonious")

## Convert Graph info to DataFrame
full_df <- full_graph$x$edges %>% 
  inner_join(full_graph$x$nodes, by=c('from'='id'), suffix = c("", "_from")) %>% 
  inner_join(full_graph$x$nodes, by=c('to'='id'),  suffix = c("", "_to")) %>% 
  # mutate(label = case_when(
  #   label == "a" ~ "EIWP",
  #   label == "b" ~ "SANZ",
  #   label == "c" ~ "NWP",
  #   label == "d" ~ "WIO",
  #   label == "e" ~ "NEP",
  #   label == "f" ~ "NWA",
  #   label == "g" ~ "EP",
  #   label == "h" ~ "WA",
  #   label == "i" ~ "NEA",
  #   label == "k" ~ "SAFR",
  #   .default = as.character(label)
  # ),
  # label_to = case_when(
  #   label_to == "a" ~ "EIWP",
  #   label_to == "b" ~ "SANZ",
  #   label_to == "c" ~ "NWP",
  #   label_to == "d" ~ "WIO",
  #   label_to == "e" ~ "NEP",
  #   label_to == "f" ~ "NWA",
  #   label_to == "g" ~ "EP",
  #   label_to == "h" ~ "WA",
  #   label_to == "i" ~ "NEA",
  #   label_to == "k" ~ "SAFR",
  #   .default = as.character(label_to)
  # ),
  # ) %>% 
  mutate(transmission = paste0(label,">", label_to))# %>% 
  ## Manual Edge Selection/Ambiguity
  # filter(!transmission %in% c("Almirante Bay>Bering Sea",
  #                             "Almirante Bay>North Pacific Ocean",
  #                             "Almirante Bay>Persian Gulf",
  #                             "Gulf of Thailand>?")) %>% 
  # mutate(ambig = ifelse(label_to == "Club de Yates", TRUE, FALSE))


# colors_16s <- readr::read_csv("Galvan_16S_54taxa/Galvan_16S_colors.csv")
colors_greys <- c("#FFFFFF", rev(grDevices::gray.colors(100)))

nodes <- full_graph$x$nodes %>%
  mutate(shape = "dot",
         font.size = 20) %>% 
  mutate(color = case_when(
    label == "a (EIWP)" ~ "#bcdc90",
    label == "b (SANZ)" ~ "#65a97a",
    label == "c (NWP)" ~ "#65a97a",
    label == "d (WIO)" ~ "#cd8f5f",
    label == "e (NEP)" ~ "#7593bb",
    label == "f (NWA)" ~ "#7593bb",
    label == "g (EP)" ~ "#c5d4e8",
    label == "h (WA)" ~ "#c5d4e8",
    label == "i (NEA)" ~ "#7a797a",
    label == "k (SAFR)" ~ "#7a797a"
  )) %>% 
  # mutate(label = case_when(
  #   label == "a" ~ "EIWP",
  #   label == "b" ~ "SANZ",
  #   label == "c" ~ "NWP",
  #   label == "d" ~ "WIO",
  #   label == "e" ~ "NEP",
  #   label == "f" ~ "NWA",
  #   label == "g" ~ "EP",
  #   label == "h" ~ "WA",
  #   label == "i" ~ "NEA",
  #   label == "k" ~ "SAFR",
  #   .default = as.character(label)
  # )) %>% 
  # filter(!label %in% c("?",
  #                      "Bering Sea",
  #                      "Persian Gulf",
  #                      "North Pacific Ocean")) %>% 
  mutate(value_percentiles = as.integer(value/max(value)*100)+1)
# inner_join(colors_16s, by = c("label" = "Body_of_Water_formatted"))

# for (i in 1:nrow(nodes)){
#   nodes$color[i] <- colors_greys[nodes$value_percentiles[i]]
# }


edges <- full_df %>% 
  mutate(arrows = "to",
         smooth = TRUE,
         # dashes = full_df$ambig,
         color = "grey",
         # color = ifelse(to == 5, "red", "grey"),
         width = value^2) %>% ## Exaggerate line widths to see differences
  select(from,
         to,
         arrows,
         smooth,
         # dashes,
         color,
         width)


## Generate the custom network using `visNetwork`
output_graph <- visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE))

output_graph

write_csv(output_graph$x$nodes, "2023May_run/SD_TaiwanCOI_network_strainhub_output.csv")

full_df_renamed <- full_df %>% 
  rename(number_of_transmissions = value,
         from_node_sourcehubratio = value_from,
         to_node_sourcehubratio = value_to,
         from_node_id = from,
         to_node_id = to,
         from_node_label = label,
         to_node_label = label_to) %>% 
  select("transmission",
         "number_of_transmissions",
         "from_node_label",
         "from_node_id",
         "from_node_sourcehubratio",
         "to_node_label",
         "to_node_id",
         "to_node_sourcehubratio")
write_csv(full_df_renamed, "2023May_run/SD_TaiwanCOI_network_strainhub_output_networkvalues.csv")
