# Load necessary libraries
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Create the DAG using DiagrammeR syntax
dag <- "
digraph DAG {
  graph [layout = dot, rankdir = TB]
  
  # Nodes
  Physician_Preference [label = 'Physician Prescribing Preference', shape = box, style = filled, fillcolor = lightblue]
  Antipsychotic_Medication [label = 'Conventional vs. Atypical Antipsychotic Medication', shape = box, style = filled, fillcolor = lightblue]
  Mortality [label = 'Mortality', shape = box, style = filled, fillcolor = lightblue]
  Age [label = 'Age (measured)', shape = box, style = filled, fillcolor = lightblue]
  Frailty [label = 'Frailty (unmeasured or poorly measured)', shape = box, style = filled, fillcolor = lightblue]
  
  # Edges
  Physician_Preference -> Antipsychotic_Medication
  Antipsychotic_Medication -> Mortality
  Age -> Mortality
  Frailty -> Mortality
  Frailty -> Antipsychotic_Medication
  Age -> Antipsychotic_Medication
}
"

# Render the DAG
graph <- grViz(dag)

# Export the DAG to an SVG file
export_svg(graph) %>% charToRaw %>% rsvg_png("dag_example.png", width = 9 * 300 / 2.54, height = 9 * 300 / 2.54)