library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggmosaic)
library(tidyverse)
library(readxl)

d <- read_excel("data/Final Extraction Form ALL.xlsx")

consolidated_types <- c(
  # Climate change and related issues
  "Air quality (climate change)" = "Climate change",
  "Climate change" = "Climate change",
  "Climate change (pesticides)" = "Climate change",
  "Climate change,Biodiversity loss" = "Climate change",
  "Climate change,Carbon capture and storage" = "Climate change",
  "Climate change,Plastic pollution" = "Climate change",
  "Climate change,Social change" = "Climate change",
  "Climate change,Social change and equality" = "Climate change",
  "Climate change/Biodiveristy loss" = "Climate change",
  "Climate change/biodiversity" = "Climate change",
  "Climate change/Energy conservation; Electricity conservation" = "Climate change",
  "Climate change; Climate change,Social change" = "Climate change",
  "Climate change; Climate change,Social equality" = "Climate change",
  "Particulate Matter" = "Climate change",
  "Protection of endangered animals" = "Climate change",
  "Sustainability" = "Climate change",
  
  # COVID-19 related
  "COVID-19 containment measures" = "COVID-19",
  "COVID-19 mitigation behavior" = "COVID-19",
  "COVID-19 vaccination" = "COVID-19",
  "COVID19 containment measures" = "COVID-19",
  "COVID19 containment measures-travel" = "COVID-19",
  "Covid19 vaccination" = "COVID-19",
  "COVID19 vaccination" = "COVID-19",
  "COVID19 vaccination,COVID19 prevention measures" = "COVID-19",
  
  # Vaccination programs
  "Flu vaccination" = "Vaccination",
  "HPV vaccination" = "Vaccination",
  "Monkeypox vaccination" = "Vaccination",
  "Pertussis cocooning vaccination" = "Vaccination",
  
  # Other categories
  "AMR" = "Antimicrobial resistance",
  "General Long-Term Collective-Action" = "General collective action",
  "Infectious disease (give to charity focused on malaria); Infectious disease (give to charity focused on deworming)" = "Infectious disease",
  "Social change" = "Social issues",
  "Social change and equality" = "Social issues",
  "Socially responsible investments in healthcare sector (pension scheme)" = "Socially responsible investing",
  "Zika transmission" = "Infectious disease",
  "Environmental quality\" vs \"Healthcare\" vs \"Safety and security\" vs \"Natural disaster prevention" = "Multiple collective-action problems"
)

# Recode collective-action problem types
d <- d %>%
  mutate(`Type of collective-action problem` = recode(`Type of collective-action problem`, !!!consolidated_types))

# Define theory columns
theory_columns <- c("Temporal Discounting", "Utopian Thinking", "Episodic Future Thinking", 
                    "Hope", "Construal Level Theory", "Consideration of Future Consequences", 
                    "Cognitive Alternatives", "Anxiety", "Anticipated Emotion")

# --- Build weighted edge list: Type ↔ Theory ---
# Assumes theory columns are 0/1 (or TRUE/FALSE). If they are text labels, adapt the filter condition accordingly.
edges_bipartite <- d %>%
  pivot_longer(cols = all_of(theory_columns), names_to = "Theory", values_to = "value") %>%
  # Keep rows where a given theory is present for the observation:
  filter(!is.na(value), value != 0, value != FALSE, value != "0") %>%
  group_by(`Type of collective-action problem`, Theory) %>%
  summarise(weight = n(), .groups = "drop") %>%
  rename(from = `Type of collective-action problem`, to = Theory)

# --- Create node list with a 'type' flag for bipartite layout ---
nodes <- tibble(name = unique(c(edges_bipartite$from, edges_bipartite$to))) %>%
  mutate(type = if_else(name %in% unique(edges_bipartite$from), "Type", "Theory"))

# --- Build igraph with weighted edges ---
g_bip <- graph_from_data_frame(edges_bipartite, vertices = nodes, directed = FALSE)

# Optional: check edge weights
# E(g_bip)$weight

# --- Plot bipartite graph with edge weights ---
set.seed(123)
ggraph(g_bip, layout = "bipartite") +
  # the y-axis in bipartite layout separates types vs theories by vertex attribute 'type'
  geom_edge_link(aes(width = weight), alpha = 0.4, colour = "grey40") +
  scale_edge_width(range = c(0.5, 3), guide = "none") +
  geom_node_point(aes(color = type), size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c(Type = "#2C7FB8", Theory = "#F03B20")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "Weighted Bipartite Network: Collective-action Type ↔ Theory",
       subtitle = "Edge width = number of observations with that Type–Theory pairing")


























# MODIFIED FUNCTION TO INCLUDE THEORY-THEORY CONNECTIONS
plot_theory_network_with_cooccurrence <- function(data, theory_cols, title = "Network Map") {
  
  # Reshape to long format for problem-theory edges
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Create problem-theory edges
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "Weight") %>%
    rename(from = Theory, to = `Type of collective-action problem`)
  
  # CREATE THEORY-THEORY EDGES
  # Get unique problems
  problems <- unique(data_long$`Type of collective-action problem`)
  
  theory_theory_edges_list <- list()
  
  # For each problem, create edges between theories that co-occur
  for (problem in problems) {
    # Get all theories for this problem
    theories_for_problem <- data_long %>%
      filter(`Type of collective-action problem` == problem) %>%
      pull(Theory) %>%
      unique()
    
    # If there are at least 2 theories for this problem
    if (length(theories_for_problem) >= 2) {
      # Create all combinations of theories for this problem
      combos <- combn(theories_for_problem, 2, simplify = TRUE)
      
      # Create edge list for this problem
      for (i in 1:ncol(combos)) {
        theory_theory_edges_list[[length(theory_theory_edges_list) + 1]] <- 
          tibble(
            from = combos[1, i],
            to = combos[2, i],
            Weight = 1,  # Each co-occurrence counts as 1
            Problem = problem  # Track which problem caused this connection
          )
      }
    }
  }
  
  # Combine all theory-theory edges
  if (length(theory_theory_edges_list) > 0) {
    theory_theory_edges <- bind_rows(theory_theory_edges_list) %>%
      group_by(from, to) %>%
      summarise(
        Weight = sum(Weight),  # Sum weights if same theories co-occur in multiple problems
        Problems = paste(unique(Problem), collapse = "; "),  # List problems causing connection
        .groups = "drop"
      )
  } else {
    theory_theory_edges <- tibble(from = character(), to = character(), 
                                  Weight = numeric(), Problems = character())
  }
  
  # Combine both edge types
  all_edges <- bind_rows(
    problem_theory_edges %>% mutate(edge_type = "problem-theory"),
    theory_theory_edges %>% mutate(edge_type = "theory-theory")
  )
  
  # Create graph object
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  
  # Add edge attributes
  E(g)$weight <- all_edges$Weight
  E(g)$edge_type <- all_edges$edge_type
  E(g)$problems <- ifelse(all_edges$edge_type == "theory-theory", 
                          all_edges$Problems, NA)
  
  # Identify node types
  all_nodes <- tibble(name = V(g)$name) %>%
    mutate(type = if_else(name %in% theory_columns, "Theory", "Collective-action problem"))
  
  # Add node attributes
  V(g)$type <- all_nodes$type
  
  # Calculate degree centrality for sizing nodes
  V(g)$degree <- degree(g)
  
  # Plot network
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(
      aes(
        width = weight, 
        color = edge_type,
        alpha = weight
      ),
      show.legend = TRUE
    ) +
    geom_node_point(
      aes(
        color = type, 
        size = degree
      )
    ) +
    geom_node_text(
      aes(label = name, size = ifelse(type == "Theory", 4, 5)), 
      repel = TRUE,
      fontface = ifelse(V(g)$type == "Theory", "bold", "plain")
    ) +
    scale_edge_color_manual(
      values = c("problem-theory" = "gray70", "theory-theory" = "darkorange"),
      name = "Edge Type"
    ) +
    scale_edge_width_continuous(range = c(0.5, 3), name = "Co-occurrence Strength") +
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "lightgreen"),
      name = "Node Type"
    ) +
    scale_size_continuous(range = c(5, 15), guide = "none") +
    theme_void() +
    labs(
      title = title,
      subtitle = "Theories connected when they co-occur in the same studies"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "bottom"
    )
  
  # Print summary statistics
  cat("\n=== Network Summary ===\n")
  cat("Total nodes:", vcount(g), "\n")
  cat("Total edges:", ecount(g), "\n")
  cat("Problem-Theory edges:", sum(all_edges$edge_type == "problem-theory"), "\n")
  cat("Theory-Theory edges:", sum(all_edges$edge_type == "theory-theory"), "\n")
  
  # Calculate and print theory co-occurrence statistics
  if (nrow(theory_theory_edges) > 0) {
    cat("\n=== Theory Co-occurrence Summary ===\n")
    theory_connections <- theory_theory_edges %>%
      arrange(desc(Weight))
    
    print(head(theory_connections, 10))
  }
  
  return(p)
}






# Use the function with your data
p1 <- plot_theory_network_with_cooccurrence(
  d, 
  theory_columns, 
  title = "Network Map: Theories and Collective-Action Problems"
)
print(p1)

# For d2 (with consolidated_types2)
d2 <- d %>%
  mutate(`Type of collective-action problem` = recode(`Type of collective-action problem`, !!!consolidated_types2))

p2 <- plot_theory_network_with_cooccurrence(
  d2, 
  theory_columns, 
  title = "Network Map: Theories and Collective-Action Problems (d2)"
)
print(p2)

plot_theory_network <- function(data, theory_cols, title = "Network Map") {
  # Reshape to long format
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Count problem-theory co-occurrences
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "Weight")
  
  # ADDED: Create theory-theory edges
  # For each problem, create connections between all theories used for that problem
  theory_theory_edges <- data_long %>%
    # Group by problem
    group_by(`Type of collective-action problem`) %>%
    # For each group, create all possible theory pairs
    summarise(
      theory_pairs = list(combn(Theory, 2, simplify = FALSE)),
      .groups = "drop"
    ) %>%
    # Unnest the pairs
    unnest_longer(theory_pairs) %>%
    # Extract from and to from each pair
    mutate(
      from = map_chr(theory_pairs, ~.[1]),
      to = map_chr(theory_pairs, ~.[2])
    ) %>%
    # Count how many problems each theory pair appears in together
    count(from, to, name = "Weight") %>%
    # Add edge type identifier
    mutate(edge_type = "theory-theory")
  
  # Combine both edge types
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "problem-theory"),
    theory_theory_edges
  )
  
  # Create graph object
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  
  # Add edge attributes
  E(g)$weight <- all_edges$Weight
  E(g)$edge_type <- all_edges$edge_type
  
  # Identify node types
  node_types <- tibble(name = V(g)$name) %>%
    mutate(type = if_else(name %in% theory_cols, "Theory", "Collective-action problem"))
  
  # Add node type to graph
  V(g)$type <- node_types$type
  
  # Plot network with node color by type
  ggraph(g, layout = "fr") +
    # Draw theory-theory edges with different color/style
    geom_edge_link(
      aes(
        width = weight, 
        color = edge_type,
        alpha = ifelse(edge_type == "theory-theory", 0.8, 0.6)
      ),
      show.legend = TRUE
    ) +
    geom_node_point(
      aes(color = type), 
      size = 5
    ) +
    geom_node_text(
      aes(label = name, color = type), 
      repel = TRUE, 
      size = 4,
      fontface = ifelse(V(g)$type == "Theory", "bold", "plain")
    ) +
    scale_color_manual(
      values = c(
        "Theory" = "skyblue", 
        "Collective-action problem" = "darkred"
      ),
      name = "Node Type"
    ) +
    scale_edge_color_manual(
      values = c(
        "problem-theory" = "gray70",
        "theory-theory" = "darkorange"
      ),
      name = "Connection Type"
    ) +
    scale_edge_width_continuous(
      range = c(0.5, 3), 
      name = "Co-occurrence Strength"
    ) +
    guides(
      edge_alpha = "none"  # Hide alpha from legend since we're using it for visibility
    ) +
    theme_void() +
    labs(
      title = title, 
      subtitle = "Theories connect when used together in same studies"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50")
    )
}

# Alternative: Simpler version without edge type distinction
plot_theory_network <- function(data, theory_cols, title = "Network Map") {
  # Reshape to long format
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Count problem-theory co-occurrences
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "weight")
  
  # Create theory-theory edges
  theory_theory_edges <- data_long %>%
    # For each problem, get theories
    group_by(`Type of collective-action problem`) %>%
    # Only create combinations when we have at least 2 theories
    filter(n() >= 2) %>%
    # Create all combinations of theories
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    # Extract from and to from combinations
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    # Count co-occurrences
    count(from, to, name = "weight") %>%
    mutate(edge_type = "Theory–Theory")
  
  # Combine edges and add edge_type to problem-theory edges
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "Theory–CAP"),
    theory_theory_edges
  )
  
  # Create graph object - IMPORTANT: include edges data frame
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  
  # Verify edge attributes are correctly set
  # They should already be there from graph_from_data_frame
  
  # Create node type dataframe for plotting
  node_df <- tibble(name = V(g)$name) %>%
    mutate(type = if_else(name %in% theory_cols, "Theory", "Collective-action problem"))
  
  # Edge aesthetics: width by weight, color by edge type
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = weight, colour = edge_type), alpha = 0.55) +
    geom_node_point(aes(color = node_df$type), size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, size = 4) +
    scale_color_manual(values = c(
      "Theory" = "skyblue",
      "Collective-action problem" = "darkred"
    ), name = "Node Type") +
    scale_edge_colour_manual(values = c(
      "Theory–CAP" = "grey60",
      "Theory–Theory" = "steelblue"
    ), name = "Edge Type") +
    scale_edge_width(range = c(0.4, 3), guide = "none") +
    theme_void(base_size = 12) +
    labs(title = title)
}

# Alternative: More explicit version that ensures edge attributes
plot_theory_network_explicit <- function(data, theory_cols, title = "Network Map") {
  # Reshape to long format
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Count problem-theory co-occurrences
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "Weight")
  
  # Create theory-theory edges
  theory_theory_edges <- data_long %>%
    group_by(`Type of collective-action problem`) %>%
    filter(n() >= 2) %>%
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    count(from, to, name = "Weight") %>%
    mutate(edge_type = "Theory–Theory")
  
  # Combine edges
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "Theory–CAP"),
    theory_theory_edges
  )
  
  # Create vertices list
  vertices <- unique(c(all_edges$from, all_edges$to))
  vertex_types <- ifelse(vertices %in% theory_cols, "Theory", "Collective-action problem")
  
  # Create graph explicitly
  g <- graph_from_data_frame(all_edges, directed = FALSE, vertices = data.frame(name = vertices, type = vertex_types))
  
  # Ensure edge weights are set properly
  E(g)$weight <- all_edges$Weight
  E(g)$edge_type <- all_edges$edge_type
  
  # Edge aesthetics: width by weight, color by edge type
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = weight, colour = edge_type), alpha = 0.55) +
    geom_node_point(aes(color = type), size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, size = 4) +
    scale_color_manual(values = c(
      "Theory" = "skyblue",
      "Collective-action problem" = "darkred"
    ), name = "Node Type") +
    scale_edge_colour_manual(values = c(
      "Theory–CAP" = "grey60",
      "Theory–Theory" = "steelblue"
    ), name = "Edge Type") +
    scale_edge_width(range = c(0.4, 3), guide = "none") +
    theme_void(base_size = 12) +
    labs(title = title)
}

# Even simpler debugging version
plot_theory_network_debug <- function(data, theory_cols, title = "Network Map") {
  # Reshape to long format
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Count problem-theory co-occurrences
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "Weight")
  
  # Create theory-theory edges
  theory_theory_edges <- data_long %>%
    group_by(`Type of collective-action problem`) %>%
    filter(n() >= 2) %>%
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    count(from, to, name = "Weight") %>%
    mutate(edge_type = "Theory–Theory")
  
  # Combine edges
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "Theory–CAP"),
    theory_theory_edges
  )
  
  # Create graph
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  
  # DEBUG: Check what attributes exist
  cat("Edge attributes:", list.edge.attributes(g), "\n")
  cat("Edge weights exist?:", "weight" %in% list.edge.attributes(g), "\n")
  
  # If weight attribute doesn't exist, create it from Weight column
  if (!"weight" %in% list.edge.attributes(g)) {
    E(g)$weight <- all_edges$Weight
  }
  
  # Ensure edge_type exists
  if (!"edge_type" %in% list.edge.attributes(g)) {
    E(g)$edge_type <- all_edges$edge_type
  }
  
  # Create node type dataframe
  node_df <- tibble(name = V(g)$name) %>%
    mutate(type = if_else(name %in% theory_cols, "Theory", "Collective-action problem"))
  
  # Edge aesthetics: width by weight, color by edge type
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = weight, colour = edge_type), alpha = 0.55) +
    geom_node_point(aes(color = node_df$type), size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, size = 4) +
    scale_color_manual(values = c(
      "Theory" = "skyblue",
      "Collective-action problem" = "darkred"
    ), name = "Node Type") +
    scale_edge_colour_manual(values = c(
      "Theory–CAP" = "grey60",
      "Theory–Theory" = "steelblue"
    ), name = "Edge Type") +
    scale_edge_width(range = c(0.4, 3), guide = "none") +
    theme_void(base_size = 12) +
    labs(title = title)
}

# Test it
plot_theory_network(d2, theory_columns, title = "Network Map: Theories and Collective-Action Problems")

plot_clean_network <- function(g, title = "Network") {
  # First, let's make sure the graph has the necessary attributes
  if (!"weight" %in% list.edge.attributes(g)) {
    # Try to get weight from edge data frame
    edge_df <- get.data.frame(g, what = "edges")
    if ("weight" %in% names(edge_df)) {
      E(g)$weight <- edge_df$weight
    } else if ("Weight" %in% names(edge_df)) {
      E(g)$weight <- edge_df$Weight
    } else {
      # Default weight of 1
      E(g)$weight <- 1
    }
  }
  
  # Calculate node degree
  node_degree <- degree(g)
  high_impact_nodes <- names(node_degree)[node_degree > median(node_degree)]
  
  # Get edge weights for filtering
  edge_weights <- E(g)$weight
  
  ggraph(g, layout = "stress") +
    # All edges (faint)
    geom_edge_link(
      aes(alpha = weight/max(weight)),
      width = 0.5,
      color = "grey80"
    ) +
    # Important edges (based on weight quantile)
    geom_edge_link(
      data = function(x) {
        # x is the edge data in ggraph format
        # Filter edges where weight is in top 25%
        x[edge_weights >= quantile(edge_weights, 0.75, na.rm = TRUE), ]
      },
      aes(width = weight),
      alpha = 0.4,
      color = "steelblue"
    ) +
    geom_node_point(
      aes(color = V(g)$type, size = node_degree),
      alpha = 0.8
    ) +
    geom_node_text(
      aes(label = ifelse(name %in% high_impact_nodes, name, "")),
      repel = TRUE,
      size = 3,
      fontface = "bold",
      box.padding = 0.5,
      max.overlaps = 15
    ) +
    scale_size_continuous(range = c(3, 10), guide = "none") +
    scale_edge_alpha_continuous(range = c(0.1, 0.6), guide = "none") +
    scale_color_manual(values = c("Theory" = "skyblue", 
                                  "Collective-action problem" = "darkred"),
                       name = "Node Type") +
    theme_void() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
      legend.position = "bottom"
    )
}

# Alternative: Simpler, more robust version
plot_clean_network_simple <- function(g, title = "Network") {
  # Ensure graph has weight attribute
  if (!"weight" %in% list.edge.attributes(g)) {
    edge_df <- get.data.frame(g, what = "edges")
    if ("Weight" %in% names(edge_df)) {
      E(g)$weight <- edge_df$Weight
    } else if ("weight" %in% names(edge_df)) {
      E(g)$weight <- edge_df$weight
    } else {
      E(g)$weight <- 1
    }
  }
  
  # Get node type
  if (!"type" %in% list.vertex.attributes(g)) {
    # Try to infer from names
    all_node_names <- V(g)$name
    theory_names <- c("Temporal Discounting", "Utopian Thinking", "Episodic Future Thinking", 
                      "Hope", "Construal Level Theory", "Consideration of Future Consequences", 
                      "Cognitive Alternatives", "Anxiety", "Anticipated Emotion")
    V(g)$type <- ifelse(V(g)$name %in% theory_names, "Theory", "Collective-action problem")
  }
  
  # Calculate metrics
  node_degree <- degree(g)
  edge_weights <- E(g)$weight
  
  # Create layout
  ggraph(g, layout = "fr") +
    # Edges - single layer with alpha based on weight
    geom_edge_link(
      aes(alpha = weight, width = weight),
      color = "grey70"
    ) +
    # Nodes
    geom_node_point(
      aes(color = type, size = node_degree),
      alpha = 0.8
    ) +
    # Labels - only for high-degree nodes
    geom_node_text(
      aes(label = ifelse(node_degree > median(node_degree), name, "")),
      repel = TRUE,
      size = 3,
      fontface = "bold",
      max.overlaps = 20,
      box.padding = 0.5,
      point.padding = 0.5
    ) +
    scale_edge_alpha_continuous(range = c(0.1, 0.5), guide = "none") +
    scale_edge_width_continuous(range = c(0.3, 2), guide = "none") +
    scale_size_continuous(range = c(3, 12), guide = "none") +
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "darkred"),
      name = "Node Type"
    ) +
    theme_void() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
}

# Even simpler: Fix your original function approach
plot_theory_network_fixed <- function(data, theory_cols, title = "Network Map") {
  # Reshape to long format
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Count problem-theory co-occurrences
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "weight")
  
  # Create theory-theory edges
  theory_theory_edges <- data_long %>%
    group_by(`Type of collective-action problem`) %>%
    filter(n() >= 2) %>%
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    count(from, to, name = "weight") %>%
    mutate(edge_type = "Theory-Theory")
  
  # Combine edges
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "Theory-CAP"),
    theory_theory_edges
  )
  
  # Create graph
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  
  # Ensure weight attribute exists (lowercase)
  E(g)$weight <- all_edges$weight
  E(g)$edge_type <- all_edges$edge_type
  
  # Add node types
  V(g)$type <- ifelse(V(g)$name %in% theory_cols, "Theory", "Collective-action problem")
  
  # Calculate degree for sizing
  V(g)$degree <- degree(g)
  
  # Create plot with better label handling
  p <- ggraph(g, layout = "fr") +
    # Edges
    geom_edge_link(
      aes(width = weight, color = edge_type),
      alpha = 0.3
    ) +
    # Nodes
    geom_node_point(
      aes(color = type, size = degree),
      alpha = 0.8
    ) +
    # Labels - only show if they fit
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3.5,
      max.overlaps = 10,  # Limit overlaps
      min.segment.length = 0,  # Always draw segments
      box.padding = 0.7,  # More padding
      point.padding = 0.5
    ) +
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "darkred"),
      name = "Node Type"
    ) +
    scale_edge_color_manual(
      values = c("Theory-CAP" = "grey70", "Theory-Theory" = "darkorange"),
      name = "Edge Type"
    ) +
    scale_edge_width(range = c(0.5, 2.5)) +
    scale_size(range = c(4, 12)) +
    guides(
      edge_width = "none",
      size = "none"
    ) +
    theme_void() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  return(p)
}

plot_theory_network_clean <- function(data, theory_cols, title = "Network Map") {
  # Reshape to long format
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Count problem-theory co-occurrences
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "weight")
  
  # Create theory-theory edges
  theory_theory_edges <- data_long %>%
    group_by(`Type of collective-action problem`) %>%
    filter(n() >= 2) %>%
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    count(from, to, name = "weight") %>%
    mutate(edge_type = "theory_theory")
  
  # Combine edges
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "problem_theory"),
    theory_theory_edges
  )
  
  # Create graph
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  
  # Ensure weight and edge_type attributes exist
  E(g)$weight <- all_edges$weight
  E(g)$edge_type <- all_edges$edge_type
  
  # Add node types
  V(g)$type <- ifelse(V(g)$name %in% theory_cols, "Theory", "Collective-action problem")
  
  # Calculate degree for label decisions
  V(g)$degree <- degree(g)
  
  # Create the plot with clean layout
  ggraph(g, layout = "fr") +
    # Edges with different colors for different types
    geom_edge_link(
      aes(width = weight, color = edge_type),
      alpha = 0.4
    ) +
    # Nodes
    geom_node_point(
      aes(color = type),
      size = 8,
      alpha = 0.8
    ) +
    # Labels with better repelling
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3.8,
      max.overlaps = 8,          # Limit label overlaps
      force = 3,                 # Repelling force
      force_pull = 1,            # Pull toward center
      box.padding = 0.8,         # Padding around text
      point.padding = 0.5,       # Padding around points
      min.segment.length = 0.1,  # Minimum segment length
      segment.color = "grey40",  # Color of repelling lines
      segment.alpha = 0.5,       # Transparency of lines
      segment.size = 0.3         # Thickness of lines
    ) +
    # Color scales
    scale_color_manual(
      values = c(
        "Theory" = "skyblue", 
        "Collective-action problem" = "darkred"
      ),
      name = "Node Type"
    ) +
    scale_edge_color_manual(
      values = c(
        "problem_theory" = "grey60",
        "theory_theory" = "darkorange"
      ),
      name = "Edge Type",
      labels = c("Theory-CAP", "Theory-Theory")  # Nicer labels in legend
    ) +
    scale_edge_width_continuous(
      range = c(0.5, 3),
      name = "Co-occurrence Strength"
    ) +
    theme_void() +
    labs(
      title = title,
      subtitle = "Theories connect when used together in same studies"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40", margin = margin(b = 10)),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10)
    )
}

# Alternative with even cleaner labels (shows only important nodes)
plot_theory_network_selective <- function(data, theory_cols, title = "Network Map") {
  # Same data processing as above...
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "weight")
  
  theory_theory_edges <- data_long %>%
    group_by(`Type of collective-action problem`) %>%
    filter(n() >= 2) %>%
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    count(from, to, name = "weight") %>%
    mutate(edge_type = "theory_theory")
  
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "problem_theory"),
    theory_theory_edges
  )
  
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  E(g)$weight <- all_edges$weight
  E(g)$edge_type <- all_edges$edge_type
  V(g)$type <- ifelse(V(g)$name %in% theory_cols, "Theory", "Collective-action problem")
  
  # Calculate centrality for selective labeling
  V(g)$degree <- degree(g)
  
  # Label only nodes with high degree or all theories
  label_threshold <- quantile(V(g)$degree, 0.5)  # Label top 50%
  
  ggraph(g, layout = "fr") +
    # Edges
    geom_edge_link(
      aes(width = weight, color = edge_type),
      alpha = 0.35
    ) +
    # Nodes
    geom_node_point(
      aes(color = type, size = V(g)$degree),
      alpha = 0.9
    ) +
    # Labels for important nodes only
    geom_node_text(
      aes(label = ifelse(V(g)$degree > label_threshold | V(g)$type == "Theory", name, "")),
      repel = TRUE,
      size = 3.5,
      fontface = "bold",
      max.overlaps = 12,
      box.padding = 0.7,
      point.padding = 0.4
    ) +
    # Small, faint labels for all nodes (optional)
    geom_node_text(
      aes(label = name),
      size = 2,
      alpha = 0.4,
      check_overlap = TRUE  # Don't draw overlapping labels
    ) +
    # Color and size scales
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "darkred"),
      name = "Node Type"
    ) +
    scale_edge_color_manual(
      values = c("problem_theory" = "grey60", "theory_theory" = "darkorange"),
      name = "Edge Type",
      labels = c("Theory ↔ CAP", "Theory ↔ Theory")
    ) +
    scale_edge_width_continuous(range = c(0.5, 2.5), guide = "none") +
    scale_size_continuous(range = c(4, 12), guide = "none") +
    theme_void() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}

# Version with curved edges to reduce overlap
plot_theory_network_curved <- function(data, theory_cols, title = "Network Map") {
  # Same data processing...
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "weight")
  
  theory_theory_edges <- data_long %>%
    group_by(`Type of collective-action problem`) %>%
    filter(n() >= 2) %>%
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    count(from, to, name = "weight") %>%
    mutate(edge_type = "theory_theory")
  
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "problem_theory"),
    theory_theory_edges
  )
  
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  E(g)$weight <- all_edges$weight
  E(g)$edge_type <- all_edges$edge_type
  V(g)$type <- ifelse(V(g)$name %in% theory_cols, "Theory", "Collective-action problem")
  
  # Use curved edges
  ggraph(g, layout = "fr") +
    geom_edge_arc(
      aes(width = weight, color = edge_type),
      strength = 0.1,  # Curvature amount
      alpha = 0.4
    ) +
    geom_node_point(
      aes(color = type),
      size = 7
    ) +
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3.5,
      max.overlaps = 10
    ) +
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "darkred"),
      name = "Node Type"
    ) +
    scale_edge_color_manual(
      values = c("problem_theory" = "grey60", "theory_theory" = "darkorange"),
      name = "Edge Type"
    ) +
    scale_edge_width(range = c(0.5, 2.5)) +
    theme_void() +
    labs(title = title)
}

# Test the functions
p1 <- plot_theory_network_clean(d2, theory_columns, "Network Map with Theory Connections")
print(p1)

# Try selective labeling
p2 <- plot_theory_network_selective(d2, theory_columns, "Selective Labeling Version")
print(p2)

# Try curved edges
p3 <- plot_theory_network_curved(d2, theory_columns, "Curved Edges Version")
print(p3)

# Minimal working example enhanced
plot_minimal_clean_enhanced <- function(g, title = "Network") {
  # Ensure the graph has necessary attributes
  # Check and add edge type if missing
  if (!"edge_type" %in% list.edge.attributes(g)) {
    # Try to infer edge type: if both ends are theories, it's theory-theory
    edge_df <- get.data.frame(g, what = "edges")
    
    # Get node types
    if (!"type" %in% list.vertex.attributes(g)) {
      # Create simple type detection (you might need to customize this)
      theory_names <- c("Temporal Discounting", "Utopian Thinking", "Episodic Future Thinking", 
                        "Hope", "Construal Level Theory", "Consideration of Future Consequences", 
                        "Cognitive Alternatives", "Anxiety", "Anticipated Emotion")
      V(g)$type <- ifelse(V(g)$name %in% theory_names, "Theory", "Collective-action problem")
    }
    
    node_types <- V(g)$type
    names(node_types) <- V(g)$name
    
    # Determine edge types
    edge_types <- sapply(1:nrow(edge_df), function(i) {
      from_type <- node_types[edge_df$from[i]]
      to_type <- node_types[edge_df$to[i]]
      if (from_type == "Theory" && to_type == "Theory") {
        return("theory_theory")
      } else {
        return("problem_theory")
      }
    })
    
    E(g)$edge_type <- edge_types
  }
  
  # Ensure weight attribute exists
  if (!"weight" %in% list.edge.attributes(g)) {
    # Default weight of 1 for all edges
    E(g)$weight <- 1
  }
  
  # Ensure node type exists
  if (!"type" %in% list.vertex.attributes(g)) {
    theory_names <- c("Temporal Discounting", "Utopian Thinking", "Episodic Future Thinking", 
                      "Hope", "Construal Level Theory", "Consideration of Future Consequences", 
                      "Cognitive Alternatives", "Anxiety", "Anticipated Emotion")
    V(g)$type <- ifelse(V(g)$name %in% theory_names, "Theory", "Collective-action problem")
  }
  
  # Create the plot
  ggraph(g, layout = "fr") +
    # Edges with weights and different colors for edge types
    geom_edge_link(
      aes(width = weight, color = edge_type, alpha = weight),
      lineend = "round"
    ) +
    # Nodes
    geom_node_point(
      aes(color = type),
      size = 7,
      alpha = 0.9
    ) +
    # Labels with aggressive repelling
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3.2,
      max.overlaps = 6,
      force = 4,
      box.padding = 0.9,
      point.padding = 0.7,
      segment.color = "grey30",
      segment.size = 0.3
    ) +
    # Color scales
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "darkred"),
      name = "Node Type"
    ) +
    scale_edge_color_manual(
      values = c(
        "problem_theory" = "grey60",
        "theory_theory" = "darkorange"
      ),
      name = "Edge Type",
      labels = c("Theory ↔ CAP", "Theory ↔ Theory")
    ) +
    scale_edge_width_continuous(
      range = c(0.5, 3),
      name = "Edge Weight",
      breaks = c(1, 2, 3, 4, 5)  # Adjust based on your data
    ) +
    scale_edge_alpha_continuous(
      range = c(0.2, 0.7),
      guide = "none"  # Alpha tied to weight, no separate legend
    ) +
    theme_void() +
    labs(
      title = title,
      subtitle = "Edge thickness = frequency of co-occurrence"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40", margin = margin(b = 10)),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10)
    )
}

# Alternative: Function that creates the graph properly first, then plots
create_and_plot_network <- function(data, theory_cols, title = "Network Map") {
  # Create the network with theory-theory connections
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Problem-theory edges
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "weight")
  
  # Theory-theory edges (when theories co-occur in same studies)
  theory_theory_edges <- data_long %>%
    group_by(`Type of collective-action problem`) %>%
    filter(n() >= 2) %>%
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    count(from, to, name = "weight") %>%
    mutate(edge_type = "theory_theory")
  
  # Combine edges
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "problem_theory"),
    theory_theory_edges
  )
  
  # Create graph
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  
  # Add attributes
  E(g)$weight <- all_edges$weight
  E(g)$edge_type <- all_edges$edge_type
  
  # Add node types
  V(g)$type <- ifelse(V(g)$name %in% theory_cols, "Theory", "Collective-action problem")
  
  # Plot with minimal clean enhanced
  plot_minimal_clean_enhanced(g, title)
}

# Version that works with your existing graph 'g' if already created
plot_minimal_with_weights <- function(g, title = "Network") {
  # Basic plot with weighted edges and theory-theory connections
  ggraph(g, layout = "fr") +
    # Weighted edges with different colors for edge types
    geom_edge_link(
      aes(
        width = ifelse("weight" %in% list.edge.attributes(g), weight, 1),
        color = ifelse("edge_type" %in% list.edge.attributes(g), edge_type, "problem_theory"),
        alpha = ifelse("weight" %in% list.edge.attributes(g), weight, 1)
      ),
      lineend = "round"
    ) +
    # Nodes
    geom_node_point(
      aes(color = ifelse("type" %in% list.vertex.attributes(g), type, 
                         ifelse(name %in% theory_columns, "Theory", "Collective-action problem"))),
      size = 6,
      alpha = 0.9
    ) +
    # Labels
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3.2,
      max.overlaps = 5,
      force = 5,
      box.padding = 1.0,
      point.padding = 0.8,
      segment.color = "grey30",
      segment.size = 0.3
    ) +
    # Scales
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "darkred"),
      name = "Node Type"
    ) +
    scale_edge_color_manual(
      values = c("problem_theory" = "grey60", "theory_theory" = "darkorange"),
      name = "Edge Type"
    ) +
    scale_edge_width_continuous(range = c(0.5, 3), name = "Weight") +
    scale_edge_alpha_continuous(range = c(0.2, 0.6), guide = "none") +
    theme_void() +
    labs(title = title)
}

# If you want to create a graph from scratch with proper weights:
make_weighted_graph <- function(data, theory_cols) {
  data_long <- data %>%
    select(`Type of collective-action problem`, all_of(theory_cols)) %>%
    pivot_longer(cols = all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  # Problem-theory edges with weights
  problem_theory_edges <- data_long %>%
    count(Theory, `Type of collective-action problem`, name = "weight")
  
  # Theory-theory edges with weights
  theory_theory_edges <- data_long %>%
    group_by(`Type of collective-action problem`) %>%
    filter(n() >= 2) %>%
    reframe(
      combos = combn(Theory, 2, simplify = FALSE)
    ) %>%
    mutate(
      from = map_chr(combos, ~.[1]),
      to = map_chr(combos, ~.[2])
    ) %>%
    count(from, to, name = "weight") %>%
    mutate(edge_type = "theory_theory")
  
  # Combine
  all_edges <- bind_rows(
    problem_theory_edges %>% 
      rename(from = Theory, to = `Type of collective-action problem`) %>%
      mutate(edge_type = "problem_theory"),
    theory_theory_edges
  )
  
  # Create graph
  g <- graph_from_data_frame(all_edges, directed = FALSE)
  
  # Add attributes
  E(g)$weight <- all_edges$weight
  E(g)$edge_type <- all_edges$edge_type
  V(g)$type <- ifelse(V(g)$name %in% theory_cols, "Theory", "Collective-action problem")
  
  return(g)
}

# Usage examples:

# Option 1: Create graph and plot in one step
p1 <- create_and_plot_network(d2, theory_columns, "Network with Theory Connections")
print(p1)

# Option 2: Create graph first, then plot
g_weighted <- make_weighted_graph(d2, theory_columns)
p2 <- plot_minimal_clean_enhanced(g_weighted, "Weighted Network")
print(p2)

# Option 3: If you already have graph 'g', use the simple version
p3 <- plot_minimal_with_weights(g, "Existing Graph with Weights")
print(p3)

plot_minimal_clean <- function(g, title = "Network") {
  # Basic plot with minimal clutter
  ggraph(g, layout = "fr") +
    # Very transparent edges
    geom_edge_link(alpha = 0.2, color = "grey60", width = 0.8) +
    # Nodes
    geom_node_point(aes(color = V(g)$type), size = 6) +
    # Labels with more aggressive repelling
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3.2,
      max.overlaps = 5,  # Very strict overlap limit
      force = 5,  # More repelling force
      box.padding = 1.0,  # More padding
      point.padding = 0.8,
      segment.color = "grey30",
      segment.size = 0.3
    ) +
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "darkred"),
      name = "Node Type"
    ) +
    theme_void() +
    labs(title = title)
}

# First create your graph, then plot it
# Using your existing code to create 'g':
p <- plot_theory_network_fixed(d2, theory_columns, "Network Map")
print(p)

# Or if you already have 'g' created:
plot_minimal_clean(g, "Clean Network Visualization")


library(igraph)
library(ggraph)
library(ggplot2)
library(dplyr)

plot_minimal_clean <- function(
    g,
    title = "Network",
    min_weight = 1,          # drop edges with weight < min_weight
    edge_width_range = c(0.4, 3),
    edge_alpha_range = c(0.15, 0.6),
    size_range = c(4, 10)    # node size range based on weighted degree ("strength")
) {
  # --- Guardrails ---
  if (!inherits(g, "igraph")) stop("`g` must be an igraph object.")
  if (is.null(E(g)$weight)) {
    warning("No edge weights found; setting all weights to 1.")
    E(g)$weight <- 1
  }
  if (is.null(V(g)$type)) {
    # If node type isn't set, default everything to "Node"
    V(g)$type <- "Node"
  }
  
  # --- Threshold edges by min_weight ---
  keep_edges <- which(E(g)$weight >= min_weight)
  if (length(keep_edges) == 0) {
    warning("No edges remain at min_weight = ", min_weight, ". Lower the threshold.")
    return(invisible(NULL))
  }
  g_sub <- igraph::subgraph.edges(g, eids = keep_edges, delete.vertices = FALSE)
  
  # --- Node size by weighted degree (strength) ---
  node_strength <- strength(g_sub, vids = V(g_sub), weights = E(g_sub)$weight)
  # Rescale to desired plotting range
  rescale_to <- function(x, to = size_range) {
    if (length(unique(x)) == 1) return(rep(mean(to), length(x)))
    (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) * diff(to) + to[1]
  }
  V(g_sub)$size_plot <- rescale_to(node_strength, to = size_range)
  
  # --- Build plot ---
  ggraph(g_sub, layout = "fr") +
    # Edges: width & alpha by weight
    geom_edge_link(
      aes(width = ..index..),         # use ..index.. to allow scale_edge_width; we'll map actual weights via scale
      # Alternative: aes(edge_width = weight) also works in ggraph >= 2.0
      colour = "grey50",
      alpha = edge_alpha_range[2]
    ) +
    # Manually map edge width to weights (via scale)
    scale_edge_width(
      range = edge_width_range,
      limits = c(min(E(g_sub)$weight), max(E(g_sub)$weight)),
      name = "Tie frequency",
      guide = "legend"
    ) +
    # Nodes
    geom_node_point(aes(color = V(g_sub)$type, size = V(g_sub)$size_plot)) +
    scale_size_identity() +  # use precomputed sizes directly; no size legend
    # Labels (repel)
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3.2,
      max.overlaps = 8,
      force = 2,
      box.padding = 0.8,
      point.padding = 0.6,
      segment.color = "grey30",
      segment.size = 0.3
    ) +
    scale_color_manual(
      values = c("Theory" = "skyblue", "Collective-action problem" = "darkred", "Node" = "grey40"),
      name = "Node Type"
    ) +
    theme_void() +
    labs(title = title)
}


library(dplyr)
library(tidyr)
library(purrr)
library(igraph)

build_theory_network_graph <- function(
    data,
    theory_cols,
    label_col = "Type of collective-action problem",  # or "Type_cons"
    include_theory_theory = TRUE,
    theory_scope = c("global", "within_type"),
    min_weight = 1
) {
  theory_scope <- match.arg(theory_scope)
  
  # Normalize theory columns to 0/1
  data_norm <- data %>%
    mutate(across(all_of(theory_cols), ~ {
      x <- suppressWarnings(as.numeric(.))
      x <- tidyr::replace_na(x, 0)
      as.integer(x > 0)
    }))
  
  # Long format for Theory–CAP
  data_long <- data_norm %>%
    select(all_of(label_col), all_of(theory_cols)) %>%
    pivot_longer(all_of(theory_cols), names_to = "Theory", values_to = "Presence") %>%
    filter(Presence == 1)
  
  edges_bip <- data_long %>%
    count(Theory, !!rlang::sym(label_col), name = "weight") %>%
    rename(from = Theory, to = !!rlang::sym(label_col)) %>%
    filter(weight >= min_weight) %>%
    mutate(edge_type = "Theory–CAP")
  
  # Theory–Theory co-occurrence
  edges_tt <- tibble(from = character(), to = character(), weight = integer())
  if (include_theory_theory) {
    if (theory_scope == "global") {
      mat <- as.matrix(select(data_norm, all_of(theory_cols)))
      co <- t(mat) %*% mat
      diag(co) <- 0
      edges_tt <- as_tibble(as.data.frame(co), rownames = "from") %>%
        pivot_longer(-from, names_to = "to", values_to = "weight") %>%
        filter(from < to, weight >= min_weight) %>%
        mutate(edge_type = "Theory–Theory")
    } else { # within_type
      edges_tt <- data_norm %>%
        group_split(!!rlang::sym(label_col)) %>%
        purrr::map_df(function(df) {
          mat <- as.matrix(select(df, all_of(theory_cols)))
          if (nrow(mat) == 0) return(tibble(from = character(), to = character(), weight = integer()))
          co <- t(mat) %*% mat
          diag(co) <- 0
          as_tibble(as.data.frame(co), rownames = "from") %>%
            pivot_longer(-from, names_to = "to", values_to = "weight") %>%
            filter(from < to, weight > 0)
        }) %>%
        group_by(from, to) %>%
        summarise(weight = sum(weight), .groups = "drop") %>%
        filter(weight >= min_weight) %>%
        mutate(edge_type = "Theory–Theory")
    }
  }
  
  # Combine edges
  edges <- bind_rows(edges_bip, edges_tt)
  if (nrow(edges) == 0) {
    stop("No edges constructed; consider lowering min_weight or checking inputs.")
  }
  
  # Build igraph
  g <- graph_from_data_frame(edges, directed = FALSE)
  # Node types from data_long
  V(g)$type <- ifelse(V(g)$name %in% unique(data_long$Theory), "Theory", "Collective-action problem")
  
  # Attach edge attributes
  E(g)$weight    <- edges$weight
  E(g)$edge_type <- edges$edge_type
  
  return(g)
}


# Build weighted graph
g <- build_theory_network_graph(
  data = d2,
  theory_cols = theory_columns,
  label_col = "Type_cons",     # Use your consolidated label column
  include_theory_theory = TRUE,
  theory_scope = "global",     # or "within_type"
  min_weight = 2               # edge frequency threshold
)

# Plot with weights
plot_minimal_clean(g, title = "Network (Weighted by Tie Frequency)", min_weight = 2)

