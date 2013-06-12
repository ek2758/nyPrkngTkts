import requests
import json
import csv
import time
import itertools
# itertools.islice? # Describes function

reader = csv.reader(open('nyData/styleIdsEdmunds.csv', 'rU')) 
#'rb' had been working but stopped ??

url = 'http://api.edmunds.com/v1/api/tmv/tmvservice/calculatetypicallyequippedusedtmv?styleid=%s'

# w = csv.DictWriter(open('nyData/subModelsEdmunds.csv', 'wb+'), ('makeId','makeNiceName', 'niceName', 'subModels'), extrasaction='ignore')

query_params = { 'zip': 'HIDDEN',
				 'api_key': 'HIDDEN',
			  	 'fmt': 'json'
			   }

#nextId = reader.next()

# for id in itertools.islice(reader,100): # 3,105 styleIds
for id in itertools.islice(reader,2907,3105):

	url2 = url % id[0]
	print("working on... ", url2)
	time.sleep(2) # Time delay of 2 seconds: API call rate limit set at 2 queries/second

	request = requests.get(url2, params = query_params)

	data = request.json()

# 	print(json.dumps(data,indent=2))
	
	tmv = [(id[0], data['tmv']['nationalBasePrice']['usedTmvRetail'])]
	with (open('nyData/tmvEdmunds.csv', 'a')) as w:
			writer = csv.writer(w)
			writer.writerows(tmv)


# Writes JSON dump to temporary file.
# To view in Chrome browser: view-source:file:///tmp/dump
# f = open("/tmp/dump", "w")
# f.write(json.dumps(data, indent = 2))
# f.close()