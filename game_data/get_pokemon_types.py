# -*- coding: utf-8 -*-

"""
A simple program to download and parse the Gen 1 Pokemon types from Bulbapedia.
"""

from BeautifulSoup import BeautifulSoup
import urllib2
from utils import grouper

NUM_POKEMON = 151

# Download the html
url = r"http://bulbapedia.bulbagarden.net/"
url += "wiki/List_of_Pokémon_by_index_number_(Generation_I)"
html_file = urllib2.urlopen(url)
html = html_file.read()
html_file.close()

# Parse with BeautifulSoup, grab the types
bs = BeautifulSoup(html)
x = [td.text for td in bs.findAll("td") if 0 < len(td.text) < 60]
pokemon_types = dict()
for data in grouper(x, 5):
    hexx, weirdno, name, type1, type2 = data
    if "Trainer" in name:
        break  # just eyeballed the data to find this break point
    if "Missingno" in name:
        continue  # lots of glitch Pokemon, just skip them
    pokemon_types[name] = (type1, type2)
assert len(pokemon_types) == NUM_POKEMON


# Merge this with existing Pokemon data
basestats = dict()
basestats_file = open("base_stats.csv")
basestats_file.readline()  # skip the header
for line in basestats_file:
    number, name, hp, attack, defense, speed, special = \
        line.strip().split(", ")
    basestats[name] = [number, hp, attack, defense, speed, special]
basestats_file.close()

# Look for naming discrepancies
for p in pokemon_types:
    if p not in basestats:
        print p

# Just the Nidorans with their stupid male/female signs
# Switch to "Nidoran M" and "Nidoran F" in pokemon_types
pokemon_types["Nidoran M"] = pokemon_types[u'Nidoran\u2640']
pokemon_types["Nidoran F"] = pokemon_types[u'Nidoran\u2642']
del pokemon_types[u'Nidoran\u2640']
del pokemon_types[u'Nidoran\u2642']
assert len(pokemon_types) == NUM_POKEMON
assert set(pokemon_types.keys()) == set(basestats.keys())

# Add the types to basestats
for p in basestats:
    basestats[p].extend(pokemon_types[p])

# Get the data in basestats into order for writing
stats = []
for p in basestats:
    number, hp, attack, defense, speed, special, type1, type2 = basestats[p]
    stats.append([number, p, hp,
                  attack, defense, speed, special, type1, type2])
stats.sort(cmp=lambda x, y: int(x[0]) - int(y[0]))


# Write out a new basestats file, that also includes types
out_path = "base_stats_types.csv"
out = open(out_path, 'w')
out.write("Number, Name, HP, Attack, Defense, Speed, Special, Type1, Type2\n")
for stat in stats:
    line = ", ".join(stat) + "\n"
    out.write(line)
out.close()
