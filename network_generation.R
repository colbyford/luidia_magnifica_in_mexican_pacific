## Load in StrainHub
library(strainhub)


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

graph

saveRDS(graph, "graph1.rds")



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