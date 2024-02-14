import pandas as pd
import numpy as np
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score

def cluster_convert(df, cluster):
    """
    Turn [1, 1, 1, 0, 0]
    into [["V4", "V5"], ["V1", "V2", "V3"]]
    """
    return [
        [
            df.columns[col_index]
            for col_index in np.where(cluster == i)[0]
        ]
        for i in np.unique(cluster)
    ]

def crossval(data, model, cv, horizon = 1, lookback = 5):
    stats = []
    for train_index, test_index in cv.split(data):
        train, test = data.iloc[train_index], data.iloc[test_index]
        model.fit(train)
        stats.append(get_stats(test, model))
    stats = pd.DataFrame(stats).T
    stats.columns.name = "fold"
    stats["mean"] = stats.mean(axis=1)
    return stats

def get_stats(test_data, fitted_model):
    yhat = fitted_model.predict(test_data)
    y = test_data.iloc[fitted_model.lookback + fitted_model.horizon - 1:]
    y.index = yhat.index

    y_flat = y.values.flatten()
    yhat_flat = yhat.values.flatten()

    return pd.Series({
        "mse": mean_squared_error(y_flat, yhat_flat),
        "mae": mean_absolute_error(y_flat, yhat_flat),
        "r2": r2_score(y_flat, yhat_flat)
    })
