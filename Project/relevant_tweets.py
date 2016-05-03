import os, sys, json

directory = "/home/alex/Applied_Quant_Finance/27/"
os.chdir(directory)

filename = sys.argv[-1]

tweets = []
exceptions = 0
tries = 0
search = ["3M", "MMM",
            "American Express", "AXP", "AMEX",
            "Apple", "AAPL", "iPhone",
            "AT&T", "ATT",
            "Boeing",
            "Caterpillar",
            "Chevron", "CVX",
            "Cisco", "CSCO",
            "Coca-Cola", "KO", "Coke",
            "DuPont", "DD",
            "Exxon", "Mobil", "XOM",
            "General Electric",
            "Google", "GOOG",
            "Goldman", "GS",
            "Home Depot", "HD",
            "IBM",
            "Intel",
            "Johnson & Johnson", "Johnson and Johnson", "JNJ",
            "JPMorgan", "JPM", "JP Morgan",
            "McDonald", "Golden Arches",
            "Merck", "MRK",
            "Microsoft", "MSFT", "Winows",
            "Nike", "NKE",
            "Pfizer", "PFE",
            "Proctor & Gamble", "P&G", "P & G", "PG",
            "Travelers", "TRV",
            "UnitedHealth", "United Health", "UNH",
            "UTX", "United Technologies",
            "Verizon",
            "Visa",
            "Wal-Mart", "Walmart", "WMT",
            "Disney"]

with open(filename) as f:
    for line in f:
        try:
            data = json.loads(line)
            tweet = data['text', 'created_at']
            if any(term in tweet for term in search):
                tweets.append(tweet)
                tries +=1
            else:
                tries += 1
                continue
        except:
            exceptions += 1
            continue

os.remove(filename)

with open (filename, 'wb') as f:
    json.dump(tweets, f)

print(len(tweets))
