from abc import ABC, abstractmethod
import numpy as np
import pandas as pd
from sklearn.linear_model import RidgeCV
import os
os.environ['R_HOME'] = '/usr/lib/R'

from rpy2.robjects import pandas2ri, r
from rpy2.robjects.packages import importr
pandas2ri.activate()

sparsevar = importr('sparsevar')
rrpack = importr('rrpack')

class Model(ABC):
    """
    Important:  make sure model.lookback and model.horizon are defined,
    these are used for the get_stats function.
    """
    @abstractmethod
    def fit(self, data):
        """
        Fits a h-step ahead model to the data.
        Forgets the previous fit if model was already fit.

        Parameters
        ----------
        data : pd.DataFrame
            The data to fit the model to.
            Column for each device (V1 ... VM)
            Row for each time period.
            `time` as index.

        horizon : int
            The number of steps ahead to forecast.
        """
        ...

    @abstractmethod
    def predict(self, data):
        """
        Predicts X_{t+h} given X_t
        for each X_t (row) in the dataframe `data`.

        Parameters
        ----------
        data : pd.DataFrame
            The data to predict on.
            Column for each device (V1 ... VM)
            Row for each time period.
            `time` as index.

        Returns
        -------
        pd.DataFrame
            The predicted values.
            Column for each device (V1 ... VM)
            Row for each time period.
            `time` as index.
        """
        ...


class ARRidgeModel(Model):
    def __init__(self, lookback=5, horizon=1):
        self.lookback = lookback
        self.horizon = horizon

    def fit(self, data):
        models = [RidgeCV() for _ in range(len(data.columns))]
        for col_index, model in enumerate(models):
            col = data.iloc[:, col_index]
            # X[t, :] = [X[t - 0], X[t - 1], ..., X[t - lookback + 1]] (`lookback` lags)
            # y[t] = X[t + horizon] (horizon steps ahead)

            y = col[self.lookback + self.horizon - 1:]
            X = pd.concat([
                col.iloc[i: -self.lookback + i].reset_index(drop=True)
                for i in range(self.lookback)
            ], axis=1, keys=[f"lag_{self.lookback - i - 1}" for i in range(self.lookback)])
            X.index = data.index[self.lookback - 1:-self.horizon]
            y.index = X.index

            model.fit(X, y)


        self.models = models

    def predict(self, data):
        out = []
        for col_index, model in enumerate(self.models):
            col = data.iloc[:, col_index]
            X = pd.concat([
                col.iloc[i: -self.lookback + i].reset_index(drop=True)
                for i in range(self.lookback)
            ], axis=1, keys=[f"lag_{self.lookback - i - 1}" for i in range(self.lookback)])
            X.index = data.index[self.lookback - 1:-self.horizon]
            out.append(model.predict(X))
        return pd.DataFrame(np.array(out).T, index=data.index[self.lookback - 1:-self.horizon], columns=data.columns)

class SparseVAR(Model):
    def __init__(self, lookback=5, horizon=1, penalty = "ENET", method = "cv", scale = True, parallel = True, ncores = 12, nlambda = 64):
        self.lookback = lookback
        self.horizon = horizon
        self.penalty = penalty
        self.method = method
        self.scale = scale
        self.parallel = parallel
        self.ncores = ncores
        self.nlambda = nlambda

    def fit(self, data):
        # each row is a time period, each column is a device

        # we fit a single sparseVAR on all data
        r_df_train = pandas2ri.py2rpy(data)
        res = sparsevar.fitVAR(r_df_train, p = self.lookback, penalty=self.penalty, method=self.method, scale = self.scale, parallel = self.parallel, ncores = self.ncores, nlambda = self.nlambda)

        self.model = res
        self.coefs = list(res.rx2("A")) # contains self.lookback matrices
        self.residuals = list(res.rx2("residuals"))

    def predict(self, data):
        num_series = data.shape[1]  # Assuming data is a DataFrame where columns are series
        lookback = self.lookback

        # Initialize an array to hold the forecasts
        forecasts = np.zeros((data.shape[0] - lookback, num_series))

        for t in range(lookback, data.shape[0]):
            # Get the most recent 'lookback' observations up to time t
            recent_data = data.to_numpy()[t-lookback:t, :].T

            # Compute the forecast for time t
            forecast_t = np.zeros(num_series)
            for k in range(lookback):
                forecast_t += self.coefs[k] @ recent_data[:, -k]

            # Store the forecast
            forecasts[t - lookback, :] = forecast_t

        return pd.DataFrame(forecasts, index=data.index[lookback:], columns=data.columns)


class PerClusterModel(Model):
    def __init__(self, model_generator, clusters):
        self.model_generator = model_generator
        self.clusters = clusters # a list of lists of strings [["V1", ...], ...]

        # ASSUMES ALL MODELS HAVE THE SAME LOOKBACK AND HORIZON
        dummy_model = model_generator()
        self.lookback = dummy_model.lookback
        self.horizon = dummy_model.horizon

    def fit(self, data):
        self.models = [self.model_generator() for _ in range(len(self.clusters))]
        for model, cluster in zip(self.models, self.clusters):
            model.fit(data[cluster])
        

    def predict(self, data):
        # We have to ensure the output is in the same order as data.columns
        out = []
        for model, cluster in zip(self.models, self.clusters):
            out.append(
                pd.DataFrame(model.predict(data[cluster]), columns=cluster)
            )

        return pd.concat(out, axis=1).reindex(data.columns, axis=1)
