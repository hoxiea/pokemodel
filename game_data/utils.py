"""
Random collection of utility functions that I used while scraping data.
"""

from itertools import izip_longest

def grouper(iterable, n, fillvalue=None):
    """
    Collect data into fixed-length chunks or blocks
    from http://docs.python.org/2/library/itertools.html#recipes
    """
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx
    args = [iter(iterable)] * n
    return izip_longest(fillvalue=fillvalue, *args)

