import pandas as pd
import numpy as np
import gower
from sklearn.cluster import AgglomerativeClustering
from scipy.sparse import lil_matrix
from tqdm import tqdm
from concurrent.futures import ThreadPoolExecutor

# Load CSV data
def load_data(file_path):
    """Load data from a CSV file."""
    data = pd.read_csv(file_path)

    # Convert string columns to object type
    for col in data.select_dtypes(include=['string']).columns:
        data[col] = data[col].astype('object')
    
    return data

# Calculate pairwise Gower distances in parallel
def calculate_pairwise_distance(i, data, dist_matrix):
    n = len(data)
    for j in range(i + 1, n):
        # Calculate Gower's distance for the pair (i, j)
        dist_matrix[i, j] = gower.gower_matrix(data.iloc[i:i+1], data.iloc[j:j+1])
        dist_matrix[j, i] = dist_matrix[i, j]  # Matrix is symmetric
    return dist_matrix

# Calculate Gower's Distance Matrix in parallel
def calculate_sparse_gower_distance(data):
    n = len(data)
    dist_matrix = lil_matrix((n, n), dtype=np.float32)

    # Use ThreadPoolExecutor to parallelize the computation
    with ThreadPoolExecutor() as executor:
        futures = []
        for i in range(n):
            futures.append(executor.submit(calculate_pairwise_distance, i, data, dist_matrix))

        # Wait for all threads to complete
        for future in tqdm(futures, desc="Calculating pairwise distances", ncols=100):
            future.result()

    return dist_matrix

# Perform clustering with Gower's distance and Ward's linkage
def cluster_csv_data(data, num_clusters=3, output_file_path='clustered_data.csv'):

    # Calculate Gower's distance matrix
    gower_dist_matrix = calculate_sparse_gower_distance(data)
    
    # Perform Agglomerative Clustering with Ward's linkage
    clustering = AgglomerativeClustering(n_clusters=num_clusters, linkage='ward')
    data['Cluster'] = clustering.fit_predict(gower_dist_matrix)  # Assign cluster labels to the data
    
    # Save the clustered data to a new CSV file
    data.to_csv(output_file_path, index=False)
    print(f"Clustered data saved to {output_file_path}")

# Example usage:
file_path = r'Input.csv'  # Replace with your actual file path
output_file_path = r'Output.csv'  # Replace with your desired output path

data = load_data(file_path)  # Load data
data = data.dropna()  # Drop NA values

cluster_csv_data(data, num_clusters=50, output_file_path=output_file_path)
