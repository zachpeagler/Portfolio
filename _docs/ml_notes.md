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