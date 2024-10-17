# Machine Learning Notes

**Supervised machine learning** (SML) is where the algorithm has labelled example inputs, where the labels indicate the desired output. 
Two types of SML, **classification**, where the output is categorical, and **regression** where the output is numerical.

**Unsupervised machine learning** does not provide labels to the data, and the algorithm just tries to detect structure in unlabelled input data.

There is also **semi-supervised learning** which uses labeled data to inform unsupervised learning. (also known as novelty detection)

Finally, **reinforcement learning** is where the alhorithm uses feedback from its environment (real or simulated)

### Example of SML vs UML

Using the default *iris* dataset, consider the following example.

As a UML, the data would not have the species row, just the 4 features.

As an SML, the data *would* include the species row, effectively labelling the input data with a group.

## Unsupervised Learning

No labels! Two main types:

**Clustering** where the goal is to find subgroups solely based on the distance between observations.
**Dimensionality reduction** where the goal is to identify patterns in the data. Often used to facilitate visualization or as a pre-processing step to supervised learning.

### k-means clustering

The k-means clustering algorithms aim at partitioning *n* observations into a fixed number of *k* clusters. It will find homogenous clusters.

>stats::kmeans(x, centers = 3, nstart = 10)

where
 - x is a numeric data matrix
 - centers is the number of clusters
 - nstart is the number of times the model will run, for better accuracy.

### hierarchical clustering

Weird, prone to interference. Starts by assigning each point its own cluster, then merges the two nearest clusters until at the desired number of clusters. It can also be used to make dendograms.

### principal component analysis

dimensionality reduction!

> prcomp(data[, variables])