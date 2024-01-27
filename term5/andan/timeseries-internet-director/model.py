import numpy as np
import pandas as pd
from scipy.optimize import minimize
from statsmodels.tsa.seasonal import seasonal_decompose


def _exp_smoothing(alpha, data, sz):
    forecasts = [data[0]]
    for i in range(1, sz):
        forecast = alpha[0] * forecasts[i - 1] + data[i] * (1 - alpha[0])
        forecasts.append(forecast)

    return np.mean(np.abs(data - forecasts))


def _polynomial_regression_with_adam(X, Y, coeffs, lr=0.2, eps=1e-3, b1=0.85, b2=0.9):
    theta = np.array(coeffs, dtype=np.float64)
    first = 10 ** 6
    second = (Y - X @ theta).T @ (Y - X @ theta)
    sz = len(theta)
    m = np.zeros(sz, dtype=np.float64)
    v = np.zeros(sz, dtype=np.float64)
    epoch = 1
    while np.abs(first - second) > eps:
        if epoch > 1000:
            break
        first = second
        derivatives = np.array([0] * sz, dtype=np.float64)
        for j in range(sz):
            summ = 0
            for i in range(len(Y)):
                summ += (Y[i] - X[i] @ theta) * X[i][j]
            derivatives[j] = summ
        m = b1 * m + (1 - b1) * derivatives
        v = b2 * v + (1 - b2) * derivatives ** 2
        mm = m / (1 - b1 ** epoch)
        vv = v / (1 - b2 ** epoch)
        theta = theta + lr / (vv ** 0.5) * mm
        second = (Y - X @ theta).T @ (Y - X @ theta)
        epoch += 1
    return theta


def fit_theta_model(data, period):
    res = seasonal_decompose(data, model="additive",
                             period=period)
    seasonal = res.seasonal[:period]
    tmp = (data - res.seasonal).to_numpy()

    alpha = minimize(lambda _alpha: _exp_smoothing(_alpha, tmp, len(tmp)), np.array([0.6]), bounds=[(0, 1)],
                     method='L-BFGS-B').x[0]

    step = data.to_numpy()[0]
    for i in range(1, len(tmp)):
        step = alpha * (step - tmp[i]) + tmp[i]

    sz = len(data)
    a = np.column_stack((np.ones(sz), np.arange(0, sz)))
    _, b0 = _polynomial_regression_with_adam(a, tmp, [0, 0])

    return alpha, step, seasonal, b0


def forecast_theta_model(data, train, test, alpha, step, seasonal, period, epoch, b0, theta=2.0):
    h = np.arange(0, epoch, dtype=np.float64)
    sz = len(seasonal)
    if alpha > 0:
        h += (1 - (1 - alpha) ** sz) / alpha

    season = np.zeros(epoch) if seasonal.min() <= 0 else np.ones(epoch)

    for i in range(epoch):
        season[i] = seasonal.iloc[(i + sz) % period]

    forecast = (theta - 1) / theta * b0 * h + step
    forecast = (forecast + season) if seasonal.min() <= 0 else (forecast * season)

    return pd.DataFrame({"INDEX": forecast},
                        index=pd.date_range(train.index[-1] + data.index.freq, periods=len(test),
                                            freq=data.index.freq.rule_code))
