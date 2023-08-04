import eikon as ek
import pandas as pd
import datetime

ek.set_app_key('d3a3446b87854feeb530116eb2583ade9c0f0186')

try:
    start_date = datetime.datetime(2020, 5, 1)
    end_date = datetime.datetime(2020, 5, 6)
    data = ek.get_timeseries('HYG', interval='daily', start_date=start_date, end_date=end_date)

    if err is not None:
        print('Error: ', err)
    else:
        print(data)
except ek.EikonError as e:
    print('Eikon error: ', e)
except Exception as e:
    print('General error: ', e)