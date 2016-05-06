import os, sys, json

directory = "/home/alex/Applied_Quant_Finance/27/"
os.chdir(directory)

filename = sys.argv[-1]

tweets = []
dates = []
total = []
exceptions = 0
tries = 0
search = ["American Express", "AXP", "AMEX",
            "Apple ", "AAPL", "iPhone",
            "AT&T",
            "Boeing",
            "Caterpillar",
            "Chevron",
            "Cisco", "CSCO",
            "Coca-Cola", "Coke",
            "DuPont",
            "Exxon", "Mobil",
            "General Electric",
            "Google", "GOOG",
            "Goldman Sachs",
            "Home Depot",
            "IBM",
            "Intel",
            "Johnson & Johnson", "Johnson and Johnson",
            "JPMorgan", "JP Morgan",
            "McDonald", "Golden Arches",
            "Merck",
            "Microsoft", "MSFT", "Winows",
            "Nike",
            "Pfizer",
            "Proctor & Gamble", "P&G", "P & G",
            "Travelers",
            "UnitedHealth", "United Health",
            "United Technologies",
            "Verizon",
            "Visa",
            "Wal-Mart", "Walmart",
            "Disney"]

with open(filename) as f:
    for line in f:
        try:
            data = json.loads(line)
            tweet = data['text']
            date = data['created_at']
            if any(term.lower() in tweet.lower() for term in search):
                tweets.append(tweet)
                dates.append(date)
                one = (tweet, date)
                total.append(one)
                tries +=1
            else:
                tries += 1
                continue
        except:
            exceptions += 1
            continue

os.remove(filename)

with open (filename, 'wb') as f:
    json.dump(total, f)

print(len(tweets))
#print(exceptions)
