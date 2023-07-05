import api_key
import eikon
import datetime

eikon.set_app_key(api_key.my_api_key)


start_date = datetime.datetime(2023, 6, 1, 0, 0, 0)
end_date = datetime.datetime(2023, 6, 2, 23, 59, 59)

data = eikon.get_timeseries('.NDX', start_date = start_date, end_date = end_date, interval='minute')

#if err is None:
#    for timestamp in data.index:
#        value = data.loc[timestamp]['CLOSE']
#        print(f"Timestamp: {timestamp}, Value: {value}")
#else:
#    print(f"Erreur lors de la récupération des données : {err}")