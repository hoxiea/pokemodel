"""
A simple program to download and parse the Gen 1 Moves on Bulbapedia.
"""

from BeautifulSoup import BeautifulSoup
import urllib2
from utils import grouper

NUM_MOVES = 165    # Mew included in Gen 1

# Download the html
url = r"http://bulbapedia.bulbagarden.net/"
url += "wiki/List_of_moves"
html_file = urllib2.urlopen(url)
html = html_file.read()
html_file.close()

# Parse with BeautifulSoup
bs = BeautifulSoup(html)
x = [td.text for td in bs.findAll("td") if 0 < len(td.text) < 30]
gen1 = [move[:-1] for move in grouper(x, 9) if move[-1] == "I"]
assert len(gen1) == NUM_MOVES

# Each move is: (Number, Name, Type, Category, Contest, PP, Power, Accuracy)
# Write them out to a file
out_path = "moves.csv"
out = open(out_path, 'w')
header = "Number, Name, Type, Category, Contest, PP, Power, Accuracy, Weird\n"
out.write(header)
for move in gen1:
    num, name, movetype, category, contest, pp, power, accuracy = move
    weird = False

    # Some moves have asteisks... flag them for later examination
    weird = False
    if "*" in name or "*" in pp or "*" in power or "*" in accuracy:
        weird = True

    # Change any unicode dashes to NA
    if u'&#8212;' in power:
        power = "NA"
    if u'&#8212;' in accuracy:
        accuracy = "NA"

    # Write this out
    row = ", ".join([num, name, movetype, category, contest, pp, power,
                    accuracy, str(weird)]) + "\n"
    row = row.replace("*", "")
    out.write(row)
out.close()
