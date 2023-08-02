# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(ggraph)
library(igraph)
library(networkD3)

# Load researcher data
dataUU <- read.delim("https://raw.githubusercontent.com/roettger/Data_to_viz/main/Work_Theme.tsv", header=TRUE, sep = "\t", fileEncoding = "UTF-8")

# Transform the adjacency matrix in a long format
connect <- dataUU %>% 
  gather(key="to", value="value", -1) %>%
  na.omit()

# Number of connection per person
c( as.character(connect$Source), as.character(connect$Target)) %>%
  as.tibble() %>%
  group_by(value) %>%
  summarize(n=n()) -> coauth
colnames(coauth) <- c("name", "n")

# NetworkD3 format
graph=simpleNetwork(connect)


# Anpassen der Node-Größe basierend auf der Anzahl der Verbindungen
coauth$max_n <- max(coauth$n)
coauth$node_size <- coauth$n / coauth$max_n * 15

# Anpassen der Node-Farbe basierend auf der Anzahl der Verbindungen
coauth$node_color <- viridisLite::viridis(10)[cut(coauth$n, breaks = 10, labels = FALSE)]

# Plot
simpleNetwork(connect,     
              Source = 1,                 # column number of source
              Target = 2,                 # column number of target
              height = 880,               # height of frame area in pixels
              width = 1980,
              linkDistance = 10,         # distance between node. Increase this value to have more space between nodes
              charge = -4,              # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
              fontSize = 5,              # size of the node names
              fontFamily = "serif",       # font og node names
              linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
              nodeColour = coauth$node_color,     # colour of nodes, MUST be a common colour for the whole graph
              opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
              zoom = TRUE                   # Can you zoom on the figure?
              )




