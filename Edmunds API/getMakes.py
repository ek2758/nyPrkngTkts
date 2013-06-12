import requests
import json
import csv

# makes = {}

query_params = { 'api_key': 'HIDDEN',
			  	 'fmt': 'json'
			   }
			
url = 'http://api.edmunds.com//v1/api/vehicle/makerepository/findall'

request = requests.get(url, params = query_params)

data = request.json()

# print type(data)
# print data.keys()

# These steps do the same thing as below.
# w = csv.writer(open('Data/makesEdmunds.csv', 'wb+'))
# 
# for model in data['makeHolder']:
# # 	print value['id'], value['niceName']
# 	w.writerow((model['id'], model['niceName']))

# These steps use the DictWriter -- b/c the data type is a 'dict'
w = csv.DictWriter(open('nyData/makesEdmunds2.csv', 'wb+'), ('id','niceName'), extrasaction='ignore')

for model in data['makeHolder']:
	w.writerow(model)