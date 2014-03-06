"""
A simple program to download and parse the Base Stats website on Bulbapedia.
"""

from BeautifulSoup import BeautifulSoup
import urllib2
from utils import grouper

NUM_POKEMON = 151    # Mew included in Gen 1

# Download the html
url = r"http://bulbapedia.bulbagarden.net/"
url += "wiki/List_of_Pok%C3%A9mon_by_base_stats_(Generation_I)"
html_file = urllib2.urlopen(url)
html = html_file.read()
html_file.close()

# Parse with BeautifulSoup
bs = BeautifulSoup(html)
x = [td.text for td in bs.findAll("td") if 0 < len(td.text) < 20]

# All that remains are 151 sets of 9 values:
# Pokedex number, name, HP, Attack, Defense, Speed, Special, Total, Ave
assert len(x) == NUM_POKEMON * 9

# Clump them into stats, dropping Total and Ave
# PokeNumber -> (Name, HP, Attack, Defense, Speed, Special)
stats = dict()
for pokemon in grouper(x, 9):
    num, name, hp, attack, defend, speed, special, t, a = pokemon
    num, hp, attack, defend, speed, special = \
        map(int, (num, hp, attack, defend, speed, special))
    stats[num] = (name, hp, attack, defend, speed, special)
assert len(stats) == NUM_POKEMON

# Test a few based on the numbers on the website
assert stats[1] == ("Bulbasaur", 45, 49, 49, 45, 65)
assert stats[58] == ("Growlithe", 55, 70, 45, 60, 50)
assert stats[113] == ("Chansey", 250, 5, 5, 50, 105)
assert stats[146] == ("Moltres", 90, 100, 90, 90, 125)

# Fix names with Unicode symbols in them
stats[29] = ("Nidoran F",) + stats[29][1:]
stats[32] = ("Nidoran M",) + stats[29][1:]

# Write these out into a file for analysis and import into a faster language
out_path = "base_stats.csv"
out = open(out_path, 'w')
out.write("Number, Name, HP, Attack, Defense, Speed, Special\n")
for key in xrange(1, NUM_POKEMON + 1):
    row = ", ".join([str(key)] + map(str, stats[key])) + "\n"
    out.write(row)
out.close()
