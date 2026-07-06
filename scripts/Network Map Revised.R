library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(igraph)
library(ggraph)
library(ggplot2)
library(scales)

# =====================================================
# DATA
# =====================================================

d <- Studies_with_years

# =====================================================
# CREATE THEORY INDICATORS
# =====================================================

d <- d %>%
  mutate(
    `Temporal Discounting` =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("Temporal Discounting", ignore_case = TRUE))),
    
    `Utopian Thinking` =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("Utopian Thinking", ignore_case = TRUE))),
    
    `Episodic Future Thinking` =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("Episodic Future Thinking", ignore_case = TRUE))),
    
    Hope =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("\\bHope\\b", ignore_case = TRUE))),
    
    `Construal Level Theory` =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("Construal Level Theory", ignore_case = TRUE))),
    
    `Consideration of Future Consequences` =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("Consideration.*Future.*Consequences",
                                  ignore_case = TRUE))),
    
    `Cognitive Alternatives` =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("Cognitive Alternative",
                                  ignore_case = TRUE))),
    
    Anxiety =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("Anxiety", ignore_case = TRUE))),
    
    `Anticipated Emotion` =
      as.integer(str_detect(`Theoretical Frameworks`,
                            regex("Anticipated Emotion",
                                  ignore_case = TRUE)))
  )

# =====================================================
# THEORY LIST
# =====================================================

theory_columns <- c(
  "Temporal Discounting",
  "Utopian Thinking",
  "Episodic Future Thinking",
  "Hope",
  "Construal Level Theory",
  "Consideration of Future Consequences",
  "Cognitive Alternatives",
  "Anxiety",
  "Anticipated Emotion"
)

# =====================================================
# CONSOLIDATED CAP CATEGORIES
# =====================================================

# =====================================================
# CONSOLIDATE COLLECTIVE-ACTION PROBLEM TYPES
# =====================================================

d <- d %>%
  mutate(
    Type_cons = case_when(
      
      # Climate change and related environmental issues
      str_detect(
        `Type of collective-action problem`,
        regex(
          "Climate change|Air quality|Particulate Matter|Sustainability|Carbon capture|Biodiver|Plastic pollution|Protection of endangered animals|Energy conservation|Electricity conservation",
          ignore_case = TRUE
        )
      ) ~ "Climate change",
      
      # COVID-19
      str_detect(
        `Type of collective-action problem`,
        regex(
          "COVID",
          ignore_case = TRUE
        )
      ) ~ "COVID-19",
      
      # Vaccination
      str_detect(
        `Type of collective-action problem`,
        regex(
          "vaccin",
          ignore_case = TRUE
        )
      ) ~ "Vaccination",
      
      # Antimicrobial resistance
      str_detect(
        `Type of collective-action problem`,
        regex(
          "AMR|Antimicrobial resistance",
          ignore_case = TRUE
        )
      ) ~ "Antimicrobial resistance",
      
      # Infectious disease
      str_detect(
        `Type of collective-action problem`,
        regex(
          "Infectious disease|malaria|deworm|zika",
          ignore_case = TRUE
        )
      ) ~ "Infectious disease",
      
      # General collective action
      str_detect(
        `Type of collective-action problem`,
        regex(
          "General collective action|General Long-Term Collective-Action",
          ignore_case = TRUE
        )
      ) ~ "General collective action",
      
      # Social issues
      str_detect(
        `Type of collective-action problem`,
        regex(
          "Social change|Social equality|equality",
          ignore_case = TRUE
        )
      ) ~ "Social issues",
      
      # Socially responsible investing
      str_detect(
        `Type of collective-action problem`,
        regex(
          "Socially responsible",
          ignore_case = TRUE
        )
      ) ~ "Socially responsible investing",
      
      # Multiple collective-action problems
      str_detect(
        `Type of collective-action problem`,
        regex(
          "Multiple collective-action problems|Environmental quality.*Healthcare.*Safety.*Natural disaster",
          ignore_case = TRUE
        )
      ) ~ "Multiple collective-action problems",
      
      TRUE ~ `Type of collective-action problem`
    )
  )

# =====================================================
# BUILD NETWORK GRAPH
# =====================================================

build_bipartite_graph <- function(
    data,
    theory_cols,
    label_col = "Type_cons",
    min_weight = 1
) {
  
  data_long <- data %>%
    select(all_of(label_col), all_of(theory_cols)) %>%
    pivot_longer(
      cols = all_of(theory_cols),
      names_to = "Theory",
      values_to = "Presence"
    ) %>%
    filter(Presence == 1)
  
  edges <- data_long %>%
    count(
      Theory,
      !!rlang::sym(label_col),
      name = "weight"
    ) %>%
    filter(weight >= min_weight) %>%
    rename(
      from = Theory,
      to = !!rlang::sym(label_col)
    )
  
  vertices <- tibble(
    name = unique(c(edges$from, edges$to))
  ) %>%
    mutate(
      type = ifelse(
        name %in% theory_cols,
        "Theory",
        "Collective-action problem"
      )
    )
  
  g <- graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = vertices
  )
  
  E(g)$weight <- edges$weight
  
  return(g)
}

# =====================================================
# PLOT NETWORK
# =====================================================

plot_network <- function(g) {
  
  ggraph(g, layout = "fr") +
    
    geom_edge_link(
      aes(width = weight),
      colour = "grey80",
      alpha = 0.8
    ) +
    
    geom_node_point(
      aes(colour = type),
      size = 4
    ) +
    
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 4,
      force = 5,
      max.overlaps = Inf
    ) +
    
    scale_color_manual(
      values = c(
        "Collective-action problem" = "darkred",
        "Theory" = "skyblue"
      ),
      name = "Node Type"
    ) +
    
    scale_edge_width(
      range = c(0.4, 3),
      name = "Weight"
    ) +
    
    theme_void() +
    
    theme(
      legend.position = "right"
    )
}

# =====================================================
# CREATE GRAPH
# =====================================================

g <- build_bipartite_graph(
  data = d,
  theory_cols = theory_columns,
  label_col = "Type_cons",
  min_weight = 1
)

# =====================================================
# PLOT
# =====================================================

p <- plot_network(g)

print(p)

# =====================================================
# SAVE
# =====================================================

ggsave(
  "Network_Map_Theories_CAPs.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 300
)

