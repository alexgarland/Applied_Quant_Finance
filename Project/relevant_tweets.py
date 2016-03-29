import os, sys, json

directory = "/home/alex/Applied_Quant_Finance/27/"
os.chdir(directory)

filename = sys.argv[-1]

tweets = []
exceptions = 0
tries = 0

with open(filename) as f:
    for line in f:
        try:
            data = json.loads(line)
            tweet = data['text']
            if ":-)" not in tweet:
                tries += 1
                continue
            else:
                tweets.append(tweet)
                tries +=1
        except:
            exceptions += 1
            continue

os.remove(filename)

with open (filename, 'wb') as f:
    json.dump(tweets, f)
