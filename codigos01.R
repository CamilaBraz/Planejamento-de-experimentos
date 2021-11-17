install.packages("agricolae")
library(agricolae)

variedades = c("A", "B", "C", "D", "E")
replicas = 5
design.crd(trt = variedades, r = replicas)$book