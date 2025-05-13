Hello! 
For a senior group project, we analyzed a dataset from a food pantry in the greater Des Moines, Iowa, USA area using Clustering. We specifically used Hierarchical clustering using Gower's Distance and Ward's Method. 
Note that the actual datafile used is not publicly available to be released. This includes a cleaning file that was used which turned all variables into factors. Most files will need the read in csvs changed to your datafile.
Some of the code in this repo was generated with ChatGPT but it was overseen by humans.
There is a total of ten files within this directory and they are:

README.txt
The file you are looking at right now! This file explains all of the other files.

Clustering.py
This file takes in the data file and outputs a column to that datafile that contains cluster numbers. Lines 61 and 62 would need to be changed to the file path of the datafile and output file path respectively. Every column was a categorical variable. 

Polk Random Forest.R
This file takes in two different clustered files and will predict the clusters for one file based on the cluster assignments of another file using a random forest. Ranger is utilized over RandomForest as RandomForest has a limited amount of levels a categorical response variable can have. This is useful for comparing clusters done on the same data but over two different spans of time i.e. pre-covid and post-covid. NOTE: If you decide to use a RandomForest with a lot of levels that the response variable can take, make sure to turn off sample with replacement as this can cause some clusters to not be represented in the final predictions

Cluster_Comparison_Mode_w_AverageVisits.R
The bulk of this file is generating a similarity score for comparing two clusters. It is comparing the mode of each variable and seeing how many of the modes the clusters have in common

Overall makeups.R
This file takes in the clustered data and calculates the quotient of the proportion of each variable level within each cluster to the overall proportion of the same variable level within the entire data. Useful for seeing what values a cluster contains relative to the overall data.

overviewpreandpost.R
Takes in the clustered data, compares each variable levels from the two time spans (pre and post COVID) and outputs a bar chart showing side by side the various levels

Bidwell bar charts.R
Compares the same group between two datafiles and makes plots showing the percent of the group in the data. Useful for after finding clusters that relate and querying the data to make sure that all of members of the group are represented in analysis. The specific comparisons made in this R file were due to findings in the next R file listed.

Bidwell cluster comparison.R
Takes in the clustered data (two datafiles). It then calculates a similar proportion of proportions to Overall makeups.R then calculates the Weighted Jaccard distance between the two datafiles' clusters. This allows the user to see similar clusters between the two datafiles. After this the code lists some examples with a way of showing the most popular values within the cluster pair.

Bidwell random forest.R
Showcases another method of random forest to predict similar clusters (see Polk Random Forest.R). After the Random Forest Implementation, there is a report that shows off the most common level (and the proportion) of each variable for every single cluster. This is nice for being able to categorize clusters as something other than "Cluster 1" i.e. "The Young Adult House Owning Cluster"

Bidwell cluster percentages by year.R
This file takes in the whole data file and was looking at the differences of cluster population over time. As part of limitations that our group experienced, we only looked at two years of data. This was to fill in the gaps in the data between the two years when looking at similar clusters.


