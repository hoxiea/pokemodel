# -*- coding: utf-8 -*-

"""
A simple program that:
- downloads and parses the Base Stats website on Bulbapedia,
- downloads and parses the Pokemon types on Bulbapedia,
- combines them with name disambiguation, and
- writes out a data file that can be used to create Pokemon and analyze data
"""

from BeautifulSoup import BeautifulSoup
import urllib2
from utils import grouper

NUM_POKEMON = 151    # Mew included in Gen 1

#### BASE STATS ####
# Download the base stats html
stats_url = r"http://bulbapedia.bulbagarden.net/"
stats_url += "wiki/List_of_Pok%C3%A9mon_by_base_stats_(Generation_I)"
html_file = urllib2.urlopen(stats_url)
stats_html = html_file.read()
html_file.close()

# Parse base stats with BeautifulSoup
bs = BeautifulSoup(stats_html)
x = [td.text for td in bs.findAll("td") if 0 < len(td.text) < 20]

# All that remains are 151 sets of 9 values:
#   Pokedex number, name, HP, Attack, Defense, Speed, Special, Total, Ave
assert len(x) == NUM_POKEMON * 9

# Clump them into stats, dropping Total and Ave
# PokeNumber -> (Name, HP, Attack, Defense, Speed, Special)
stats = dict()
for data in grouper(x, 9):
    num, name, hp, attack, defend, speed, special, t, a = data
    num, hp, attack, defend, speed, special = \
        map(int, (num, hp, attack, defend, speed, special))
    stats[num] = (name, hp, attack, defend, speed, special)
assert len(stats) == NUM_POKEMON

# Fix names with Unicode symbols in them
stats[29] = ("Nidoran F",) + stats[29][1:]
stats[32] = ("Nidoran M",) + stats[29][1:]

# Test a few based on the numbers on the website
assert stats[1] == ("Bulbasaur", 45, 49, 49, 45, 65)
assert stats[58] == ("Growlithe", 55, 70, 45, 60, 50)
assert stats[113] == ("Chansey", 250, 5, 5, 50, 105)
assert stats[146] == ("Moltres", 90, 100, 90, 90, 125)


#### TYPES ####
# Download the types html
types_url = r"http://bulbapedia.bulbagarden.net/"
types_url += "wiki/List_of_Pokémon_by_index_number_(Generation_I)"
html_file = urllib2.urlopen(types_url)
types_html = html_file.read()
html_file.close()

# Parse with BeautifulSoup, grab the types
bs = BeautifulSoup(types_html)
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

# The Nidorans have their stupid male/female signs
# Switch to "Nidoran M" and "Nidoran F" in pokemon_types
pokemon_types[u"Nidoran M"] = pokemon_types[u'Nidoran\u2640']
pokemon_types[u"Nidoran F"] = pokemon_types[u'Nidoran\u2642']
del pokemon_types[u'Nidoran\u2640']
del pokemon_types[u'Nidoran\u2642']
assert len(pokemon_types) == NUM_POKEMON

#### COMBINE THE TWO ####
# stats: Num -> (Name, HP, Attack, Defense, Speed, Special)
# pokemon_types: Name -> (Type1, Type2)

# Make sure the names all line up
assert set(pokemon_types.keys()) == set([v[0] for v in stats.values()])

# Add types to stats
for n in stats:
    stats[n] = list(stats[n])
    name = stats[n][0]
    stats[n].extend(pokemon_types[name])

# Get them into a writable configuration
final = []
for number in stats:
    name, hp, attack, defense, speed, special, type1, type2 = \
        map(str, stats[number])
    final.append([str(number), name, hp,
                  attack, defense, speed, special, type1, type2])
final.sort(cmp=lambda x, y: int(x[0]) - int(y[0]))

# Write out a new basestats file, that also includes types
out_path = "base_stats_types.csv"
out = open(out_path, 'w')
out.write("Number, Name, HP, Attack, Defense, Speed, Special, Type1, Type2\n")
for stat in final:
    line = ", ".join(stat) + "\n"
    out.write(line)
out.close()
