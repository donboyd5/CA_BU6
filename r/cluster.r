library(fastDummies)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms


clusdf <- caworkers %>%
  filter(casco | ((soc != "33-3012") & (level=="private"))) %>%
  select(soc, occname, pwgtp, agep, exp, mar, sex, edattain, yoschool, ltba, rwagp) %>%
  mutate(edattain=fct_drop(edattain), sex=fct_drop(sex)) %>%
  dummy_cols(select_columns = c("edattain", "mar", "sex"), remove_first_dummy=FALSE) %>%
  select(-c(pwgtp, sex, edattain, mar)) %>%
  group_by(soc, occname) %>%
  summarise(n=n(), across(-n, list(mean=wtd.mean, var=wtd.var)), .groups="drop") %>%
  filter(n > 1)
summary(clusdf)

clusdf2 <- clusdf %>%
  select(-c(soc, occname, n, contains("rwagp"))) %>%
  scale() %>%
  kmeans(centers=5)
# kclust$cluster
# augment(kclust, clusdf2)

clustered <- clusdf %>%
  mutate(cluster=clusdf2$cluster) %>%
  group_by(cluster) %>%
  mutate(cascogrp=soc=="33-3012",
         cascogrp=max(cascogrp))
summary(clustered)


cascogrp <- clustered %>%
  arrange(cluster, soc, occname) %>%
  filter(cascogrp==1) %>%
  select(cluster, soc, occname, n, contains("rwagp"), yoschool_mean, contains("edattain")) %>%
  arrange(desc(rwagp_mean))

cg2 <- cascogrp %>%
  filter(n >= 50)

# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


count(clusdf, soc, occname)

cdf2 <- clusdf %>%
  mutate(male=ifelse(sex=="male", 1, 0)) %>%
  select(pwgtp, agep, mar, male, yoschool, rwagp)
row.names(cdf2) <- clusdf$soc

cdfs <- scale(cdf2)

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

wss <- (nrow(clusdf) - 1) * sum(apply(clusdf, 2, var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")