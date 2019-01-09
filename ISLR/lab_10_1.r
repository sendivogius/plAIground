library(ISLR)

hc.complete = hclust(dist(USArrests), method ="complete")
hc.complete.cut = cutree (hc.complete , 3)
hc.complete.scale = hclust(dist(scale(USArrests)), method ="complete")
hc.complete.scale.cut = cutree (hc.complete.scale , 3)