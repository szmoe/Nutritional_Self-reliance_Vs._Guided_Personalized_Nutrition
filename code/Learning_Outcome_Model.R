library(DiagrammeR)

# Initial Model based on findings in published papers ####
DiagrammeR("graph LR
           A(Latent)--> I 
           B(Actualised)--> I
           C(Text-based)--> I
           D(Task-based)--> I
           E(Knowledge-based)--> I
           A1(Value-related belief)--> A
           A2(Emotional attitude)--> A
           B1(Degree of participation)--> B
           B2((Motivation to learn))--> L
           C1(Learning material)--> C
           D1(Activities)--> D
           E1(Prior knowledge/experience)--> E
           I((Interest in learning))--> L{Learning outcome}
           H((Learning hour))--> L
           F(Individual interest)--> H
           G(Situational interest)--> H
           A2--> F
           E1--> F
           J1--> G
           J((Teacher's attitude))--> L
           J--> I
           J--> H
           J1(Interaction/Feeling)--> J
           J2(Performance/Quality)--> J
           J1--> X2
           X((Study habit))--> L
           X1(Self-motivation)-->B2
           X2(External-motivation)-->B2
           Z((Student's creativity))--> L
           Z--> I
           
           style A fill: lightpink,  stroke:black, stroke-width:2px;
           style B fill: lightpink,  stroke:black, stroke-width:2px;
           style C fill: lightpink,  stroke:black, stroke-width:2px;
           style D fill: lightpink,  stroke:black, stroke-width:2px;
           style E fill: lightpink,  stroke:black, stroke-width:2px;
           style A1 fill: lightgrey,  stroke:black, stroke-width:2px;
           style A2 fill: lightgrey,  stroke:black, stroke-width:2px;
           style B1 fill: lightgrey,  stroke:black, stroke-width:2px;
           style B2 fill: violet, stroke:black, stroke-width:2px;
           style C1 fill: lightgrey,  stroke:black, stroke-width:2px;
           style D1 fill: lightgrey,  stroke:black, stroke-width:2px;
           style E1 fill: lightgrey,  stroke:black, stroke-width:2px;
           style I fill: pink, stroke:black, stroke-width:2px;
           style L fill: lightblue, stroke:black, stroke-width:2px
           style H fill: green, stroke:black, stroke-width:2px
           style F fill: lightgreen, stroke:black, stroke-width:2px
           style G fill: lightgreen, stroke:black, stroke-width:2px
           style J fill: yellow, stroke:black, stroke-width:2px
           style J1 fill: lightyellow, stroke:black, stroke-width:2px
           style J2 fill: lightyellow, stroke:black, stroke-width:2px
           style X fill: violet, stroke:black, stroke-width:2px
           style X1 fill: lightviolet, stroke:black, stroke-width:2px
           style X2 fill: lightviolet, stroke:black, stroke-width:2px
           style Z fill: pink, stroke:black, stroke-width:2px")

# Conceptual Framework of Intervention ####
mermaid("graph TB
        I(Interventions)-->N(Improved nutrition knowledge and practice)
        I--> R(Right food in right amount at right time)
        N--> D(Improved diet)
        R--> D
        D--> S(Improved sports performance)
        A(Nutritional self-reliance)--> I
        B(Guided personalized nutrition)--> I
        C(Food quality and safety at kitchens)--> I
        I1(Heavy nutrition education)--> A
        I2(Light nutrition education & nutritionist)--> B
      
        style I fill: lightbeige, stroke:brown, stroke-width: 3px
        style N fill: lightblue, stroke: brown, stroke-width: 3px
        style R fill: lightblue, stroke: brown, stroke-width: 3px
        style D fill: lightgreen, stroke:brown, stroke-width: 3px
        style S fill: yellow, stroke:brown, stroke-width: 3px
        style A fill: lightbeige, stroke:brown, stroke-width: 3px
        style B fill: lightbeige, stroke:brown, stroke-width: 3px
        style C fill: lightbeige, stroke:brown, stroke-width: 3px")

# Preliminary model for Intervention 1 drafted by the football players ####

# Create a node data frame (ndf)
ndf <- create_node_df(
  n         = 36,
  label     = c("improved nutrition knowledge",
                 "Trainer",
                 "Learning materials",
                 "Facility",
                 "Motivation",
                 "Learning hours",
                 "Attitude",
                 "Good interpersonal skill",
                 "Good teaching skill",
                 "Easy to understand",
                 "Easy to apply",
                 "Visual aid\n Food models",
                 "Good classroom\n (light, sound, temperature)",
                 "Refreshments",
                 "Know importance of nutrition",
                 "Low interest",
                 "Lack of energy",
                 "Long training hour",
                 "training on weekends",
                 "Giving homework",
                 "Inconvenience\n (time, location, etc)",
                 "Experience benefits",
                 "Shorter,\n frequent classes",
                 "Good attitude towards\n nutrition training",
                 "Willingness\n to learn and apply",
                 "Know long term benefits",
                 "Persistence",
                 "improved nutrition practice\n (apply knowledge)",
                 "Retain knowledge",
                 "Food",
                 "Not tasty/safe",
                 "Not enough\n (amount, variety)",
                 "Dietary restriction",
                 "Regular practice",
                 "Reminder\n (e.g. poster in dining hall)",
                 "Enough resources and\n help when needed"),
  shape     = c("hexagon", 
                "circle", 
                "circle", 
                "circle", 
                "circle",
                "circle",
                "circle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "hexagon",
                "circle",
                "circle",
                "rectangle",
                "rectangle",
                "circle",
                "rectangle",
                "rectangle",
                "rectangle" ),
  style     = "filled",
  fontsize  = 8,
  fixedsize = FALSE,
  height    = .2,
  width     = .75,
  color     = c("#988558","#00008B","#00008B","#00008B","#00008B","#00008B","#00008B","#006400",
                "#006400","#006400","#006400","#006400","#006400","#006400","#006400","#8B0000", 
                "#8B0000","#8B0000","#8B0000","#8B0000","#8B0000","#006400","#006400","#006400",
                "#006400","#006400","#006400","#5C4033","#00008B","#00008B","#8B0000","#8B0000",
                "#00008B","#006400","#006400","#006400")

)

# Create an edge data frame (edf)
edf <- create_edge_df(
  from     = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
               26,27,29,5,7,30,33,31,32,34,35,36),
  to       = c(28,1,1,1,1,1,1,2,2,3,3,3,4,4,5,5,16,16,16,16,16,5,6,7,7,7,7,28,28,
               28,28,28,30,30,29,34,34),
  label    = c("+","++","++","+","+++","++","+++","+","+","+","+","+","+","+",
                 "+","-","-","-","-","-","-","+","+","+","+","+","+","++","+++",
                 "++","","-","-","-","+","+","+"),
  fontsize = 9,
  minlen   = 1,
  color    = "#000000"
)

# Create a graph with the ndf and edf
graph <- create_graph(
  nodes_df = ndf,
  edges_df = edf
)

graph %>%
  render_graph()

# 1 improved nutrition knowledge
# 2 Good trainer ++
# 3 Good learning materials ++
# 4 Good facility +
# 5 Enough motivation +++
# 6 Enough learning hours ++
# 7 Attitude +++

# 8 Good interpersonal skill +
# 9 Good teaching skill +
# 10 Easy to understand +
# 11 Easy to apply +
# 12 Visual aid/ Food models +
# 13 Good classroom (light, sound, temperature) +
# 14 Refreshments +
# 15 Know importance of nutrition +
# 16 Low interest -
  # 17 Lack of energy -
  # 18 Long training hour -
  # 19 training on weekends -
  # 20 Giving homework -
  # 21 Inconvenience (time, location, etc) -
# 22 Experience benefits +
# 23 Shorter, frequent classes +
# 24 Good attitude towards nutrition training +
# 25 Willingness to learn and apply +
# 26 Know long term benefits +
# 27 Persistence +


#  28 improved nutrition practice (apply knowledge)

# 29 Retain knowledge ++
#  5 Enough motivation +++
#  7 Attitude ++
#  30  Food
  # 31 Not tasty/safe
  # 32 enough/n (amount, variety)
# 33 Dietary restriction -


# 34 Regular practice +
  # 35 Reminder (e.g. poster in dining hall) +
  # 36 Enough resources and help when needed +

# Preliminary model for Intervention 2 drafted by the football players ####

# Create a node data frame (ndf)
ndf <- create_node_df(
  n         = 19,
  label     = c("Improved\n nutrition knowledge",
                "Trainer",
                "Learning materials",
                "Facility",
                "Interest",
                "Convenience",
                "Good interpersonal skill",
                "Good teaching skill",
                "Easy to understand",
                "Easy to apply",
                "Visual aid",
                "Good classroom\n (light, sound, temperature)",
                "Refreshments",
                "Motivation from teacher",
                "Know importance\n of nutrition",
                "Lack of energy\n after training",
                "Short class duration",
                "Weekends class",
                "Class scheduled\n shortly after training"),
  
  shape     = c("hexagon", 
                "circle", 
                "circle", 
                "circle", 
                "circle",
                "circle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle",
                "rectangle"),
  style     = "filled",
  fontsize  = 8,
  fixedsize = FALSE,
  height    = .2,
  width     = .75,
  color     = c("#5C4033","#00008B","#00008B","#00008B","#00008B","#00008B","#006400",
                "#006400","#006400","#006400","#006400","#006400","#006400","#006400",
                "#006400","#8B0000","#006400","#8B0000","#8B0000")
  
)

# Create an edge data frame (edf)
edf <- create_edge_df(
  from     = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
  to       = c(1,1,1,1,1,2,2,3,3,3,4,4,5,5,5,5,6,6),
  label    = c("++","+","+","++","+++","+","+","+","+","+","+","+","+","+",
               "-","+","-","-"),
  fontsize = 9,
  minlen   = 2,
  color    = c("#006400","#006400","#006400","#006400","#006400","#006400",
               "#006400","#006400","#006400","#006400","#006400","#006400",
               "#006400","#006400","#8B0000","#006400","#8B0000","#8B0000",
               "#8B0000","#8B0000")
)

# Create a graph with the ndf and edf
graph <- create_graph(
  nodes_df = ndf,
  edges_df = edf
)

graph %>%
  render_graph()


# 1 improved nutrition knowledge

# 2 Trainer ++
# 3 Learning materials +
# 4 Facility +
# 5 Interest ++
# 6 Convenience +++

# 7 Good interpersonal skill +
# 8 Good teaching skill +
# 9 Easy to understand +
# 10 Easy to apply +
# 11 Visual aid +
# 12 Good classroom (light, sound, temperature) +
# 13 Refreshments +
# 14 Motivation from teacher +
# 15 Know importance of nutrition +
# 16 Lack of energy after training -
# 17 Short class duration +
# 18 Weekend class -
# 19 Class schedule after training -

# Decision wheel for learning outcome for intervention 1 ####

library(plotly)

Inter_1 <- plot_ly(
  labels = c("Learning outcome", "Motivation", "Interest", "Attitude", 
              "Material", "Learning hour", "Quality of trainer", "Communication", 
              "Study habit", "Creativity", "Facility","Other"),
  
  parents = c("","Learning outcome","Learning outcome","Learning outcome",
              "Learning outcome","Learning outcome","Learning outcome",
              "Learning outcome","Learning outcome","Learning outcome",
              "Learning outcome","Learning outcome"),
  
  values = c(100, 14, 14, 14, 8, 8, 14, 8, 4, 8, 4, 4),
  
  type = 'sunburst',
  
  branchvalues = 'total',
  
  textinfo = "label"
  
  )
Inter_1

# Decision wheel for learning outcome for intervention 2 ####

Inter_2 <- plot_ly(
  
  labels = c("Learning outcome", "Motivation", "Interest", "Convenience", "Material",
             "Learning hour", "Attitude", "Quality of trainer", "Communication",
             "Facility", "Other"),
  
  parents = c("", "Learning outcome","Learning outcome","Learning outcome",
              "Learning outcome","Learning outcome","Learning outcome",
              "Learning outcome","Learning outcome", "Learning outcome",
              "Learning outcome"),
  
  values = c(100, 12, 12, 16, 4, 4, 16, 16, 12, 4, 4),
  
  type = 'sunburst',
  
  branchvalues = 'total',
  
  textinfo = 'labels'
)

Inter_2



