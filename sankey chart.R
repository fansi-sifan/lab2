# Library
library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name = c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p


# struggling workers in Denver

links <- data.frame(stringsAsFactors=FALSE,
      source = c("all adults 18-64", "all adults 18-64", "working", "working",
                 "not working", "not working", "not working"),
      target = c("working", "not working", "employed, low wage", "employed,
                 mid-high wage", "Unemployed",
                 "stopped looking", "doesn't need a job"),
       value = c(1524922, 528544.01, 625993, 898929, 86937.49, 178793.57,
                 262812.95)
)



nodes <- data.frame(
  name = c(as.character(links$source), 
           as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   width = 2000, height = 500,
                   sinksRight=FALSE)
p

