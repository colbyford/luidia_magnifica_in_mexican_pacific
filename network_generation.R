## Load in Libraries
library(strainhub)
library(visNetwork)
library(dplyr)
library(randomcoloR)

### First Network
## Read in tree, metadata, and geodata
treedata <- ape::read.tree("RAxML_bestTree.LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa.tre")
metadata <- readr::read_csv("LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa16s.csv", col_names = TRUE)


## Make the Transmission Network
graph <- makeTransNet(treedata,
                      metadata,
                      columnSelection = "Body_of_Water_formatted",
                      centralityMetric = 6,
                      treeType = "parsimonious")

# graph

saveRDS(graph, "graph1.rds")
graph <- readRDS("graph1.rds")


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
treedata <- ape::read.tree("RAxML_bestTree.COI_nataly_brenda_JANESSA.DEDUP.trim.tre")
metadata <- readr::read_csv("RAxML_bestTree.COI_nataly_brenda_JANESSA.DEDUP.trim.out_101.fix.csv", col_names = TRUE)


## Make the Transmission Network
graph <- makeTransNet(treedata,
                      metadata,
                      columnSelection = "Body_of_Water_formatted",
                      centralityMetric = 6,
                      treeType = "parsimonious")
graph

saveRDS(graph, "graph2.rds")
graph <- readRDS("graph2.rds")

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
