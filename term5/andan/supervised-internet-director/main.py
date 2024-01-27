import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import MinMaxScaler
from sklearn.linear_model import SGDRegressor
from sklearn.metrics import mean_absolute_error

def mserror_mat(X, w, y):
    y_pred = X @ w
    return np.sum((y - y_pred) ** 2) / len(y_pred)

def gr_mserror_mat(X, w, y):
    y_pred = X @ w
    return 2/len(X)* (X.T) @ (y_pred - y)

df = pd.read_csv("data.csv", encoding='utf-8', quotechar='"', skipinitialspace=True)


df_encoded = pd.get_dummies(df, columns=['Статус'])

X = df_encoded.drop(columns=['Размер', 'Скачан']).select_dtypes(include='number').values
y = df_encoded['Скачан'].values

imputer = SimpleImputer(strategy='mean')
X_t_imputed = imputer.fit_transform(X)

scaler = MinMaxScaler()
X_t = scaler.fit_transform(X_t_imputed)

train_percent = 0.7
val_percent = 0.15
test_percent = 0.15

train_rows = int(len(X) * train_percent)
val_rows = int(len(X) * val_percent)
test_rows = int(len(X) * test_percent)

model = SGDRegressor()
model.fit(X_t[:train_rows], y[:train_rows])
model_predict = model.predict(X_t[train_rows + val_rows:])

lrs = [0.00000001, 0.0000001, 0.000001, 0.00001, 0.001, 0.01, 0.1, 0.55, 0.6, 0.7]

def fit(X, y, lr):
    eps = 0.001
    next_weights = np.zeros(X.shape[1])
    for i in range(1000):
        cur_weights = next_weights
        next_weights = cur_weights - lr * gr_mserror_mat(X, cur_weights, y)

        if np.linalg.norm(cur_weights - next_weights, ord=2) <= eps:
            break
    return next_weights

def predict(X, w):
    return X @ w

def grid_search(X, y):
    mse_val = float('inf')
    lr_res = 0
    for lr in lrs:
        w = fit(X[:train_rows], y[:train_rows], lr)
        err = mserror_mat(X[train_rows:train_rows + val_rows], w, y[train_rows:train_rows + val_rows])
        if err < mse_val:
            lr_res = lr
            mse_val = err

    return lr_res

learning_rate = grid_search(X_t, y)
weights = fit(X_t[:train_rows], y[:train_rows], learning_rate)
pr = predict(X_t[train_rows + val_rows:], weights)

my_model_mae = mean_absolute_error(pr, y[train_rows + val_rows:])
lib_model_mae = mean_absolute_error(model_predict, y[train_rows + val_rows:])

diff = abs(lib_model_mae - my_model_mae) / max(lib_model_mae, my_model_mae)
if diff <= 0.05:
    print("OK: lib_mae=" + str(lib_model_mae) + " my_mae=" + str(my_model_mae))

df['Title'] = df['Title'].astype(str)

#plt.figure(figsize=(15, 7))
plt.scatter(df['Статус'][train_rows + val_rows:], model_predict, label='Model Predictions')
plt.scatter(df['Статус'][train_rows + val_rows:], y[train_rows + val_rows:], label='Original Values', linestyle='--', color='gray')
plt.scatter(df['Статус'][train_rows + val_rows:], pr, label='Custom Predictions')
plt.xticks(rotation=90)

plt.xlabel('Original Values')
plt.ylabel('Predicted Values')
plt.title('Model vs Custom Predictions')
plt.legend()
plt.grid(True)

plt.show()
plt.scatter(pr, model_predict)
