## Load in Libraries
library(strainhub)
library(visNetwork)
library(dplyr)
library(randomcoloR)

### Galvan 16 0 54 Taxa
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("Galvan_16S_54taxa/RAxML_bestTree.LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa.tre")
metadata <- readr::read_csv("Galvan_16S_54taxa/LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa16s.csv", col_names = TRUE)

selected_meta = "Body_of_Water_formatted"
ancestralStates = asr_max_parsimony(treedata,
                                    as.numeric(as.factor(metadata[[selected_meta]])),
                                    length(unique(metadata[[selected_meta]])))

asr <- ancestralStates$ancestral_likelihoods %>% data.frame()

## Make the Transmission Network
graph <- makeTransNet(treedata,
                      metadata,
                      columnSelection = selected_meta,
                      centralityMetric = 6,
                      treeType = "parsimonious")

# graph


saveRDS(graph, "Galvan_16S_54taxa/graph.rds")
graph <- readRDS("Galvan_16S_54taxa/graph.rds")

## Recolor Nodes and Edges
palette_20 <- distinctColorPalette(20)

nodes <- graph$x$nodes %>%
  mutate(shape = "dot",
         color = palette_20,
         font.size = 20)

edges <- graph$x$edges %>%
  mutate(arrows = "to",
         smooth = TRUE,
         color = ifelse(to == 5, "red", "grey"),
         width = value,
         value = NULL)

## Generate the custom network using `visNetwork`
visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE))

####################################
### Second Network
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("Galvan_COI_101taxa/RAxML_bestTree.COI_nataly_brenda_JANESSA.DEDUP.trim.tre")
metadata <- readr::read_csv("Galvan_COI_101taxa/RAxML_bestTree.COI_nataly_brenda_JANESSA.DEDUP.trim.out_101.fix.csv", col_names = TRUE)


## Make the Transmission Network
graph <- makeTransNet(treedata,
                      metadata,
                      columnSelection = "Body_of_Water_formatted",
                      centralityMetric = 6,
                      treeType = "parsimonious")
graph

saveRDS(graph, "Galvan_COI_101taxa/graph.rds")
graph <- readRDS("Galvan_COI_101taxa/graph.rds")

## Recolor Nodes and Edges
palette_25 <- distinctColorPalette(25)

nodes <- graph$x$nodes %>%
  mutate(shape = "dot",
         color = palette_25,
         font.size = 20)

edges <- graph$x$edges %>%
  mutate(arrows = "to",
         smooth = TRUE,
         color = ifelse(to == 6, "red", "grey"),
         width = value,
         value = NULL)

## Generate the custom network using `visNetwork`
visNetwork(nodes, edges) %>%
  visOptions(nodesIdSelection = list(enabled = TRUE))
