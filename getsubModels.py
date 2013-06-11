import requests
import json
import csv
import time
import itertools
# itertools.islice? # Describes function

reader = csv.reader(open('nyData/makesEdmundsTktSubset.csv', 'rb'))

url = 'http://api.edmunds.com/v1/api/vehicle/modelrepository/findusedmodelsbymakeid?makeId=%s'

# w = csv.DictWriter(open('nyData/subModelsEdmunds.csv', 'wb+'), ('makeId','makeNiceName', 'niceName', 'subModels'), extrasaction='ignore')

query_params = { 'api_key': 'HIDDEN',
			  	 'fmt': 'json'
			   }

#nextId = reader.next()

for id in itertools.islice(reader,50):
	url2 = url % id[0]
	print("working on... ", url2)
	time.sleep(2) # Time delay of 2 seconds: API call rate limit set at 2 queries/second

	request = requests.get(url2, params = query_params)

	data = request.json()

# 	print(json.dumps(data,indent=2))
	
	makeModels = [car['subModels'] for car in data['modelHolder']]
	
	for s in makeModels:
		if 'USED' in s.keys():
			model = [(s['USED'][0]['name'], s['USED'][0]['identifier'], s['USED'][0]['styleIds'])]
			with (open('nyData/subModelsEdmunds.csv', 'a')) as w:
				writer = csv.writer(w)
				writer.writerows(model)


# Writes JSON dump to temporary file.
# To view in Chrome browser: view-source:file:///tmp/dump
# f = open("/tmp/dump", "w")
# f.write(json.dumps(data, indent = 2))
# f.close()