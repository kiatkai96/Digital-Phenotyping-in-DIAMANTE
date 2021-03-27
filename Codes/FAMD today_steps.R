#### FAMD - Factor Analysis of Mixed Data in R ####
library("FactoMineR")
library("factoextra")
library("ggplot2")
library("corrplot")

# Read in data
df = read.csv("C:/Users/Kiat Kai/Desktop/NOTES/FYP/Full Dataset/df_for_r_today.csv")
str(df)
df = subset(df, select = -c(X,index))
df$arm = as.factor(df$arm)

# Remove the blanks in the categorical variables
which.one = which(levels(df$Week_Day) == "" )
levels(df$Week_Day)[which.one] = NA
which.one = which(levels(df$health_lit) == "" )
levels(df$health_lit)[which.one] = NA
which.one = which(levels(df$health_status) == "")
levels(df$health_status)[which.one] = NA
which.one = which(levels(df$social_rel) == "")
levels(df$social_rel)[which.one] = NA
which.one = which(levels(df$feedback) == "")
levels(df$feedback)[which.one] = NA
which.one = which(levels(df$motivational) == "")
levels(df$motivational)[which.one] = NA
which.one = which(levels(df$time_msg) == "")
levels(df$time_msg)[which.one] = NA

# Remove the missing data 
df = na.omit(df)
str(df)

# FAMD    (ncp is the number of components)
model.famd = FAMD(df, ncp=42, graph=FALSE)  

# Eigenvalues/Proportion of Variances
eigenvals = get_eigenvalue(model.famd)
eigenvals

# Plot the scree plot (% of inertia explained by each FAMD dimensions)
dev.new()
fviz_eig(model.famd, ncp = 42, addlabels=FALSE)
dev.off()

# PLot of variables
dev.new()
fviz_famd_var(model.famd, choice="var",repel=TRUE, labelsize=3, shape.var=16) 
dev.off()

################################################
## Hieracrchical clustering for 28 dimensions ##  85% cumulative variance
################################################
model.famd = FAMD(df, ncp=28, graph=FALSE)

# PLot of variables
dev.new()
fviz_famd_var(model.famd, choice="var",repel=TRUE, labelsize=3, shape.var=16) 
dev.off()

res.hcpc = HCPC(model.famd, nb.clust=-1, graph=TRUE)  # -1 means the tree cuts at suggested lvl
## Plot 
# Dendogram
dev.new()
fviz_dend(res.hcpc, show_labels = FALSE)
dev.off()

dev.new()
fviz_cluster(res.hcpc, geom= "point", main="Factor map")
dev.off()

# Principal components + tree
dev.new()
plot(res.hcpc, choice = "3D.map")
dev.off()

# Description by variable categories
res.hcpc$desc.var$category

# Description by variable categories
res.hcpc$desc.var$quanti 
