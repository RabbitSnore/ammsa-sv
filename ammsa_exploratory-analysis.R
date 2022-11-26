################################################################################

# AMMSA Swedish Validation -- Exploratory Analysis

################################################################################

# This script assumes that the main analysis script has already been run.

# Setup ------------------------------------------------------------------------

packages <- c("tidyr")

lapply(packages, library, character.only = TRUE)

# IRMA exploration -------------------------------------------------------------

if (with_demo == TRUE) {
  
  irma <- read_csv("./data/ammsa_data_clean.csv")
  
} else if (with_demo == FALSE) {
  
  irma <- read_csv("./data/ammsa_data_clean_no-demographics.csv") 
  
}

# Remove attention check failures 

irma <- irma %>% 
  filter(att_control_8_text == "+" | att_control_8_text == "ett plustecken")

irma_scale <- irma %>% 
  select(starts_with("irma"))

# Confirmatory factor analysis

## Unidimensional model

irma_uni_model <- 
'

rma =~ irma_01_whiledrunk + irma_02_sluttycloth + irma_03_roomalone + irma_04_actslut + irma_05_nounclear + irma_06_girlinitiate + irma_07_strongdesire + irma_08_carriedaway + irma_09_sexdriveooc + irma_10_drunkunint + irma_11_didntrealize + irma_12_bothdrunk + irma_13_physicresist + irma_14_physfightbck + irma_15_bruisesmarks + irma_16_noweapon + irma_17_deosntsayno + irma_18_agreeregret + irma_19_getbckatguys + irma_20_ledonregrets + irma_21_emotionalprb + irma_22_caughtcheat


'

if (!file.exists("./rda/irma_uni_fit .rda")) {
  
  irma_uni_fit <- cfa(irma_uni_model,
                      data = irma_scale,
                      se = "bootstrap",
                      bootstrap = 5000,
                      meanstructure = TRUE)
  
  save(irma_uni_fit, file = "./rda/irma_uni_fit .rda")
  
} else {
  
  load("./rda/irma_uni_fit .rda")
  
}

irma_cfa_uni_figure <- 
  semPlot::semPaths(irma_uni_fit, 
                    what = "paths",
                    whatLabels = "std",
                    intercepts = FALSE,
                    residuals = FALSE,
                    curvePivot = TRUE,
                    layout = "circle",
                    sizeLat = 4,
                    sizeMan = 3,
                    groups = "latents",
                    nodeLabels = c(1:22, "RMA"))

## Four-factor model

irma_4_model <- 
'

afi =~ irma_01_whiledrunk + irma_02_sluttycloth + irma_03_roomalone + irma_04_actslut + irma_05_nounclear + irma_06_girlinitiate

dmt =~ irma_07_strongdesire + irma_08_carriedaway + irma_09_sexdriveooc + irma_10_drunkunint + irma_11_didntrealize + irma_12_bothdrunk

nr  =~ irma_13_physicresist + irma_14_physfightbck + irma_15_bruisesmarks + irma_16_noweapon + irma_17_deosntsayno

sl =~ irma_18_agreeregret + irma_19_getbckatguys + irma_20_ledonregrets + irma_21_emotionalprb + irma_22_caughtcheat

'

if (!file.exists("./rda/irma_4_fit .rda")) {
  
  irma_4_fit <- cfa(irma_4_model,
                    data = irma_scale,
                    se = "bootstrap",
                    bootstrap = 5000,
                    meanstructure = TRUE)
  
  save(irma_4_fit, file = "./rda/irma_4_fit .rda")
  
} else {
  
  load("./rda/irma_4_fit .rda")
  
}

irma_cfa_4_figure <- 
  semPlot::semPaths(irma_4_fit, 
                    what = "paths",
                    whatLabels = "std",
                    intercepts = FALSE,
                    residuals = FALSE,
                    curvePivot = TRUE,
                    layout = "tree2",
                    sizeLat = 4,
                    sizeMan = 3,
                    groups = "latents",
                    nodeLabels = c(1:22, "AFI", "DMT", "NR", "SL"))

## Bifactor model

irma_bi_model <- 
'

rma =~ irma_01_whiledrunk + irma_02_sluttycloth + irma_03_roomalone + irma_04_actslut + irma_05_nounclear + irma_06_girlinitiate + irma_07_strongdesire + irma_08_carriedaway + irma_09_sexdriveooc + irma_10_drunkunint + irma_11_didntrealize + irma_12_bothdrunk + irma_13_physicresist + irma_14_physfightbck + irma_15_bruisesmarks + irma_16_noweapon + irma_17_deosntsayno + irma_18_agreeregret + irma_19_getbckatguys + irma_20_ledonregrets + irma_21_emotionalprb + irma_22_caughtcheat

afi =~ irma_01_whiledrunk + irma_02_sluttycloth + irma_03_roomalone + irma_04_actslut + irma_05_nounclear + irma_06_girlinitiate

dmt =~ irma_07_strongdesire + irma_08_carriedaway + irma_09_sexdriveooc + irma_10_drunkunint + irma_11_didntrealize + irma_12_bothdrunk

nr  =~ irma_13_physicresist + irma_14_physfightbck + irma_15_bruisesmarks + irma_16_noweapon + irma_17_deosntsayno

sl =~ irma_18_agreeregret + irma_19_getbckatguys + irma_20_ledonregrets + irma_21_emotionalprb + irma_22_caughtcheat

# Covariances

rma ~~ 0*afi
rma ~~ 0*dmt
rma ~~ 0*nr
rma ~~ 0*sl

# afi ~~ 1*afi
afi ~~ 0*dmt
afi ~~ 0*nr
afi ~~ 0*sl

# dmt ~~ 1*dmt
dmt ~~ 0*nr
dmt ~~ 0*sl

# nr  ~~ 1*nr
nr  ~~ 0*sl

# sl  ~~ 1*sl

'

if (!file.exists("./rda/irma_bi_fit .rda")) {
  
  irma_bi_fit <- cfa(irma_bi_model,
                     data = irma_scale,
                     se = "bootstrap",
                     bootstrap = 5000,
                     meanstructure = TRUE,
                     std.lv = TRUE)
  
  save(irma_bi_fit, file = "./rda/irma_bi_fit .rda")
  
} else {
  
  load("./rda/irma_bi_fit .rda")
  
}

irma_cfa_bi_figure <- 
  semPlot::semPaths(irma_bi_fit, 
                    what = "paths",
                    whatLabels = "std",
                    intercepts = FALSE,
                    residuals = FALSE,
                    curvePivot = TRUE,
                    exoCov = FALSE,
                    layout = "tree2",
                    sizeLat = 4,
                    sizeMan = 3,
                    groups = "latents",
                    bifactor = "rma",
                    nodeLabels = c(1:22, "RMA", "AFI", "DMT", "NR", "SL"))

# Network model

if (!file.exists("./rda/ammsa_network_model_irma.rda")) {
  
  network_model_irma <- varcov(data = irma_scale,
                               type = "ggm",
                               omega = "Full")
  
  network_model_pruned_irma <- network_model_irma %>%
    setoptimizer(optimizer = "cpp_Nelder-Mead") %>% 
    prune(alpha = 0.05, recursive = TRUE)
  
  network_model_final_irma <- network_model_pruned_irma %>% 
    modelsearch( # This process is computationally intensive
      criterion  = "bic",
      prunealpha = .05,
      addalpha   = .05
    )
  
  save(network_model_final_irma, file = "./rda/ammsa_network_model_irma.rda")
  
} else {
  
  load("./rda/ammsa_network_model_irma.rda") 
  # Unless you absolutely need to rerun the model search procedure,
  # loading the premade skeleton is probably a good idea.
  
}

network_fit_irma <- 
  network_model_final_irma %>% 
  runmodel()

network_pars_irma    <- parameters(network_fit_irma)

network_fit_ind_irm  <- fit(network_fit_irma)

network_graph_irma <- 
  qgraph(getmatrix(network_model_final_irma, "omega"),
         labels = 1:22,
         layout = "spring",
         vsize = 4,
         edge.labels = TRUE,
         edge.label.cex = .40,
         edge.label.bg = "white",
         edge.label.position = .50,
         edge.color = "#151414",
         vTrans = 200,
         negDashed = FALSE)

cent_irma <- qgraph::centralityPlot(network_graph_irma,
                                    include = c("Strength", "Closeness", "Betweenness"))

irma_strength <- cent_irma$data %>% 
  filter(measure == "Strength") %>% 
  arrange(desc(value)) %>% 
  mutate(
    node = as.numeric(node)
  ) %>% 
  left_join(filter(item_text, scale == "irma"), by = c("node" = "item"))

irma_between <- cent_irma$data %>% 
  filter(measure == "Betweenness") %>% 
  arrange(desc(value)) %>% 
  mutate(
    node = as.numeric(node)
  ) %>% 
  left_join(filter(item_text, scale == "irma"), by = c("node" = "item"))

## Simulated persuasion

set.seed(9898)

### Baseline

# This simulation assumes mild/moderate disagreement with all items, to get a 
# distribution similar to the empirical distribution

irma_sim_base <- IsingSampler(10000, 
                               getmatrix(network_model_final_irma, "omega"), 
                               c(rep(-.25, 22)), 
                               responses = c(0, 1))

irma_sim_base <- irma_sim_base %>% 
  as.data.frame()

irma_sim_base$total <- rowSums(irma_sim_base)

irma_sim_base_hist <- 
  ggplot(irma_sim_base,
         aes(
           x = total
         )) +
  geom_histogram(
    binwidth = 1
  ) +
  scale_x_continuous(
    limits = c(-5, 27),
    breaks = seq(0, 22, 10)
  ) +
  theme_classic()

### Persuasion on Item 20

irma_sim_pers <- IsingSampler(10000, 
                              getmatrix(network_model_final_irma, "omega"), 
                              c(rep(-.25, 19), -1, rep(-.25, 2)), 
                              responses = c(0, 1))

irma_sim_pers <- irma_sim_pers %>% 
  as.data.frame()

irma_sim_pers$total <- rowSums(irma_sim_pers)

irma_sim_pers_hist <- 
  ggplot(irma_sim_pers,
         aes(
           x = total
         )) +
  geom_histogram(
    binwidth = 1
  ) +
  scale_x_continuous(
    limits = c(-5, 27),
    breaks = seq(0, 22, 10)
  ) +
  theme_classic()

# AMMSA further exploration ----------------------------------------------------

# Response profiles

## Long-form data for ammsa responses

ammsa_scale_long <- ammsa_scale %>% 
  mutate(
    total = rowMeans(.),
    id    = 1:nrow(.),
    bin   = case_when(
      total >= 1 & total < 2 ~ "1-2",
      total >= 2 & total < 3 ~ "2-3",
      total >= 3 & total < 4 ~ "3-4",
      total >= 4 & total < 5 ~ "4-5",
      total >= 5 & total < 6 ~ "5-6",
      total >= 6 & total < 7 ~ "6-7"
    )
  ) %>% 
  pivot_longer(
    cols = starts_with("ammsa"),
    names_to = "item",
    values_to = "ammsa"
  )

## Plot of response profiles

ammsa_profiles <- 
ggplot(ammsa_scale_long,
       aes(
         x = item,
         y = ammsa,
         group = as.factor(id)
       )) +
  facet_wrap(~ as.factor(bin),
             nrow = 5) +
  geom_line(
    alpha = .25,
    position = position_jitter(height = .25, width = .10)
  ) +
  scale_x_discrete(
    labels = 1:30
  ) +
  scale_y_continuous(
    breaks = 1:7
  ) +
  labs(
    y = "Response",
    x = "AMMSA Item"
  ) +
  theme_classic()


