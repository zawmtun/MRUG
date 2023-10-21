library(dagitty)
library(ggdag)
library(tidyverse)

# Draw DAGs ----
## Model 1 ----
# dagitty
g1a <- dagitty("dag {
  M -> D
  M <- A -> D
  M [exposure]
  D [outcome]
}")

coords <- list(
  x = c(M = 0, D = 2, A = 1),
  y = c(M = 1, D = 1, A = 0)
)

coordinates(g1a) <- coords
  
plot(g1a)

# ggdag
g1_lbl <- c(
  M = "Marriage rate",
  D = "Divorce rate",
  A = "Age of marraige"
)

g1b <- dagify(
  D ~ M + A,
  M ~ A,
  exposure = "M",
  outcome = "D",
  coords = coords,
  labels = g1_lbl
)

# Check if g1_gg is a dagitty object
is.dagitty(g1b)
class(g1b)

plot(g1b)

adjustmentSets(g1b)

# Plotting in ggdaw
ggdag(g1b)

ggdag(g1b) + theme_dag()

dat_g1 <- g1b |> tidy_dagitty()

ggplot(dat_g1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point() +
  geom_dag_text(aes(label = label), col = "steelblue",
                nudge_y = -0.1) +
  theme_dag()

impliedConditionalIndependencies(g1a)


## Model 2 ----

g2 <- dagify(
  H1 ~ T + F + H0,
  F ~ T,
  exposure = "T",
  outcome = "H1",
  coords = list(
    x = c(T = 0, H1 = 2, H0 = 3, F = 1),
    y = c(T = 1, H1 = 1, H0 = 1, F = 0)
  )
)

ggdag(g2) + theme_dag()

dagitty::isCollider(g2, "T", "H1", "H0") # TRUE
dagitty::isCollider(g2, "T", "F", "H0") # FALSE

## Model 3 ----

g3_coords <- list(
  x = c(X = 0, Y = 2, A = 0, B = 2, C = 1, Z = 1),
  y = c(X = 0, Y = 0, A = -1, B = -1, C = 1, Z = -1)
)

g3 <- dagify(
  Y ~ X + C + Z + B,
  X ~ C + Z + A,
  Z ~ A + B,
  exposure = "X",
  outcome = "Y",
  coords = g3_coords
)

ggdag(g3)

# Example from the documentation of `dagify`
g3 <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  w1 ~ U,
  w2 ~ U,
  exposure = "x",
  outcome = "y"
)

plot(g3)
ggdag(g3) + theme_dag()

## Model 4 ----

g4_lbl <- c(
  "S" = "Selection status\n(Case-control)",
  "mrsa" = "MRSA\nacquisition",
  "tfreq" = "Transfer\nfrequency",
  "los" = "Length of stay",
  "procedure" = "Dx and Tx\nProcedures",
  "casespec" = "Case specialty",
  "icu" = "Critical-care ward",
  "pos_exposure" = "Exposure to MRSA+\nin-patients\nin the event spell",
  "age" = "Age",
  "gender" = "Gender",
  "ethnicity" = "Ethnicity",
  "res_status" = "Residential\nstatus",
  "hh" = "Hand hygiene\ncompliance",
  "wclass" = "Ward class",
  "wsize" = "Ward patient capacity",
  "cohort_bed" = "Wards with\ncohorting beds"
)

g4 <- dagify(    
  mrsa ~ age + gender + ethnicity + icu + pos_exposure +
    los + tfreq + procedure,
  tfreq ~ casespec,
  procedure ~ casespec + los,
  casespec ~ age + gender,
  los ~ casespec,
  pos_exposure ~ icu + hh + wsize + cohort_bed,
  res_status ~ age + ethnicity,
  wsize ~ wclass,
  hh ~ wclass + icu,
  wclass ~ res_status + gender + ethnicity,
  S ~ mrsa,
  
  labels = g4_lbl,
  latent = c("S"),
  exposure = "tfreq",
  outcome = "mrsa"
)

ggdag(g4)

# Function to colour code the nodes
node_type <- function(dag) {
  ward_vars <- c("icu", "hh", "wclass", "wsize", "cohort_bed")
  dag <- node_status(dag)
  
  dag[[1]] <- dag[[1]] %>%
    mutate(
      ward = case_when(status == "outcome" ~ 1,
                       status == "exposure" ~ 2,
                       status == "latent" ~ 3,
                       name %in% ward_vars ~ 4,
                       TRUE ~ 5),
      ward = factor(ward,
                    levels = 1:5,
                    labels = c("Outcome", "Exposure", "Latent",
                               "Ward factor", "Patient factor"))
    )
  
  dag
}

dat_g4 <- g4 |> 
  tidy_dagitty() |> 
  node_type()

set.seed(100)
g4_plot <- ggplot(dat_g4, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(aes(fill = ward, color = ward), shape = 21, size = 9.5, alpha = 0.35) +
  geom_dag_edges(edge_colour = "grey60") +
  scale_fill_manual(values = c("#EEC591", "#7EC0EE", "grey88", "grey60", "grey40")) +
  scale_color_manual(values = c("#EEC591", "#7EC0EE", "grey88", "grey60", "grey40")) +
  labs(fill = NULL, color = NULL) +
  geom_dag_text_repel(aes_string(label = "label"),
                      size = 3.5,
                      col = "blue",
                      alpha = 0.8,
                      segment.color = NA,
                      fontface = "bold",
                      box.padding = 0,
                      point.padding = 1) +
  theme_dag()

g4_plot


# Identifying adjustment sets ----

# Model 1
ggdag(g1a)
adjustmentSets(g1a)

# Model 2
ggdag(g2)
adjustmentSets(g2)

# Model 3
ggdag(g3)
adjustmentSets(g3)

# Model 4
g4_plot
adjsets <- adjustmentSets(g4)
adjsets
adjsets |> map(\(x) g4_lbl[x])
