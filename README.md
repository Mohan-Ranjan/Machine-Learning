Machine Learning and Data Mining Algorithms

This explores unsupervised and supervised learning techniques for real-world data analysis and forecasting. 
The first section focuses on partitioning clustering using K-means, silhouette analysis, elbow methods, and gap statistics to determine optimal cluster counts. 
It includes PCA for dimensionality reduction and cluster validation metrics.

The second section covers energy consumption forecasting using a multi-layer neural network (MLP). 
Tasks include data preprocessing, normalization, training/testing splits, architecture experimentation (e.g., 1-2, 4-2 networks), and evaluation using metrics like RMSE, MAE, MAPE, and sMAPE.
The project combines exploratory data analysis, unsupervised learning, and deep learning to extract patterns and predict future outcomes, with visuals and full code for reproducibility

When evaluating the performance of machine learning models, several metrics are commonly used to assess the accuracy and effectiveness of predictions. Here are some of the key metrics used in this project:

RMSE (Root Mean Squared Error):
Measures the square root of the average squared differences between actual and predicted values.
Lower RMSE indicates better performance, but it’s sensitive to large errors due to the squaring of differences.

MAE (Mean Absolute Error):
Calculates the average of absolute errors between actual and predicted values.
Lower MAE is better, and it’s less sensitive to large errors compared to RMSE.

MAPE (Mean Absolute Percentage Error):
Measures the average of absolute percentage errors, indicating how far off predictions are in terms of percentage.
Lower MAPE is preferable, but it can be problematic when actual values are near zero.

SMAPE (Symmetric Mean Absolute Percentage Error):
Similar to MAPE but uses a symmetric formula that treats overestimation and underestimation equally.
Lower SMAPE indicates better predictive accuracy, and it is more stable than MAPE for small values.
