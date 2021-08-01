import sys
import csv
import codecs

args = sys.argv
csv_path = (args[1])
bin_path = (args[2])

f = codecs.open(csv_path, "r")
csv_data = [ e for e in csv.reader(f)]
f.close()

flat_list = [item for sublist in csv_data for item in sublist]
csv_data_int = [int(s) for s in flat_list]
bin_temp = bytes(csv_data_int)

with open(bin_path, "wb") as f:
    f.write(bin_temp)