import pandas as pd
import model
import matplotlib.pyplot as plt
from statsmodels.tsa.holtwinters import ExponentialSmoothing
from sklearn.metrics import mean_absolute_error, mean_squared_error


def gen_plot(_train, _test, gen, name):
    print(f'{name} Implementation Metrics:')
    print(f'Mean Absolute Error (MAE): {mean_absolute_error(_test, gen)}')
    print(f'Mean Squared Error (MSE): {mean_squared_error(_test, gen)}\n')

    plt.figure(figsize=(10, 6))
    plt.plot(_train, label='Train')
    plt.plot(_test, label='Test')
    plt.plot(gen, label='Generated', linestyle='dashed')
    plt.legend()
    plt.title(name)
    plt.show()


def lib_implementation(_train, _test):
    mod = ExponentialSmoothing(_train, seasonal='add', trend='add', seasonal_periods=52)
    gen = mod.fit().forecast(len(_test))
    gen_plot(_train, _test, gen, 'Lib Theta Model Generated')


def custom_implementation(_data, _train, _test):
    alpha, step, seasonal, b0 = model.fit_theta_model(_train, period=52)
    gen = model.forecast_theta_model(_data, _train, _test, alpha, step, seasonal, period=52, epoch=len(_test), b0=b0)
    gen_plot(_train, _test, gen, 'Custom Theta Model Generated')


if __name__ == '__main__':
    file_path = 'daily_sunspots_time_series_1850_2023.csv'

    data = pd.read_csv(file_path, parse_dates=['date'], index_col='date')['counts'].resample('Y').median()

    train_size = int(len(data) * 0.8)
    train, test = data[:train_size], data[train_size:]

    lib_implementation(train, test)
    custom_implementation(data, train, test)
