# -*- coding: utf-8 -*-
# Scrape the learnset for each Pokemon, then merge those with move list to get
# ID numbers

from BeautifulSoup import BeautifulSoup
import urllib2
from utils import grouper

NUM_POKEMON = 151    # Mew included in Gen 1
NUM_MOVES = 165

# Learnsets live on webpages with the following form:
# http://bulbapedia.bulbagarden.net/wiki/
#   Charmander_(Pok%C3%A9mon)/Generation_I_learnset
# But of course, that's only for Charmander. So let's get a list of Pokemon
# names to use
pokemon = dict()
pokemon_file = open("base_stats_types.csv")
for line in pokemon_file:
    if line.startswith("Number"):
        continue
    line = line.strip().split(", ")
    pokemon_index = int(line[0])
    pokemon_name = line[1]
    pokemon[pokemon_index] = pokemon_name
pokemon_file.close()

# I wrote a simple loop and try/except block to figure out which URLs weren't
# going to work. Only the Nidorans cause problems, as usual. Fix them:
pokemon[29] = ("Nidoran♀",)
pokemon[32] = ("Nidoran♂",)

# Each webpage lists the move names, but I'm interested in the move numbers
# Those are in moves.csv. Except there are all these weird formatting issues
# for moves whose formatting they changed slightly. So let's just convert
# everything to lowercase, collapse spaces and hyphens, etc.


def standardize(s):
    return s.replace(" ", "").replace("-", "").lower()


movename_to_num = dict()
move_file = open("moves.csv")
for line in move_file:
    if line.startswith("Number"):
        continue
    line = line.strip().split(", ")
    move_index = int(line[0])
    move_name = line[1]
    move_name = standardize(move_name)
    movename_to_num[move_name] = move_index
move_file.close()

# Build a data structure to capture the moves learnable by each Pokemon
learnset = dict()

# Form the URL of each Pokemon, then download and process its data
for index in pokemon:
    # it was Mr._Mine, so replace spaces with underscores
    # But skip the Nidorans, since they cause it to crash
    if index != 29 and index != 32:
        pname = pokemon[index].replace(" ", "_")

    learnset_url = r"http://bulbapedia.bulbagarden.net/wiki/"
    learnset_url += "{0}_(Pok%C3%A9mon)/Generation_I_learnset".format(pname)

    html_file = urllib2.urlopen(learnset_url)
    learnset_html = html_file.read()
    html_file.close()

    bs = BeautifulSoup(learnset_html)
    x = [td.text for td in bs.findAll("td")
         if 0 < len(td.text) < 60]
    # if td.text in movename_to_num.values()] worked pretty well, but...
    # Just grabbing everything that appears anywhere and is a valid move
    # name will grab Psychic, when those characters only appeared to indicate
    # the type of a move and not the Move Psychic

    # So instead, group them into clumps... it seems to group very consistently
    grouped = list(grouper(x, 6))

    # Pikachu had a weird move: Light Screen, which he learns at Level 50 in
    # Pokemon Yellow, but never in Red/Blue. So just grabbing the values in the
    # table that are valid moves would actually lead us to believe that Pikachu
    # can learn Light Screen, which he can, but it doesn't have a TM until Gen
    # 3. Instead, let's group the entries, drop the ones from Pokemon Yellow,
    # and then grab the remaining moves
    not_yellow = [entry for entry in grouped if not entry[0].endswith("Y")]
    moves = [standardize(entry[1]) for entry in not_yellow
             if entry[1] is not None]
    moves = list(set(moves))    # get rid of duplicates
    try:
        indices = [movename_to_num[n] for n in moves if n in movename_to_num]
        learnset[index] = zip(moves, indices)
        print "Done with {0}".format(pokemon[index])
    except:
        print "Failed on {0}".format(pokemon[index])

assert len(learnset) == 151

# Verified more than a few by hand/eye, including troublemakers like Pikachu
# Everything looked good


def format_learnset(ls):
    string_array = ["{0}|{1}".format(item[0], str(item[1])) for item in ls]
    return ", ".join(string_array)


learnset_file = open("learnsets.csv", "w")
for index in learnset:
    s = str(index) + ": " + format_learnset(learnset[index])
    learnset_file.write(s + "\n")
learnset_file.close()
