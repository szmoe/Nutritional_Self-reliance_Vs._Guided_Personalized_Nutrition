library(DiagrammeR)

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
