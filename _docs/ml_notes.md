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

### t-distributed stochastic neighbor embedding (t-SNE)
t-SNE is a non-linear dimensionality reduction technique particularly suited for high dimensional data

>library("Rtsne") <br>
uiris <- unique(iris[, 1:5])<br>
iristsne <- Rtsne(uiris[, 1:4])<br>
plot(iristsne$Y, col = uiris$Species) <br>

t-SNE has two important parameters: **perplexity** and **iterations**, the number of iterations before clustering stops.

## Supervised Learning

In supervised learning, the learning algorithm is presented with labelled example inputs, where the labels indicate the desired output.
Composed of **classification**, where the output is qualitative, and **regression**, where the output is quantitative.

With 2 groups, **binary classification** is used. With more than two groups, we use **multi-label classification**.

### k-nearest neighbors (kNN)
kNN works by directly measuring the distance between observations and *inferring* the class of unlabelled data from the class of its nearest neighbors.

>set.seed(12L)
train <- sample(150, 50)
test <- sample(150, 50)
library("class")
knnres <- knn(iris[train, -5], iris[test, -5], iris$Species[train])
head(knnres)

### Model Performance

Supervised learning is nice in that we can directly evaluate a model's performance based on the known data. For regression, we use the **root mean squared error** (RMSE) and in classification we use **model prediction accuracy**. 

Try not to calculate performance metrics on the data used to calculate the model. This causes in-sample error and leads to over-fitting. Instead, take your data and split it in half, then use one half to train the model and the other half to test and validate the model performace.