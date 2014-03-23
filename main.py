#!/usr/bin/python3

"""Main - run the casting simulation

main() generates a list of traits, which are each passed a function to
get a variate. It gives the number of people, and the list of traits
to Pool(), which generates a pool.

"""

import sys

import numpy as np

from generator import Generator
from pool import Pool
from trait import Trait


def main():
    # The group sizes are specified by the command line arguments
    number_of_generations = int(sys.argv[1])
    group_sizes = (int(a) for a in sys.argv[2:])

    # This is a trait that is normally distributed
    normal_trait = Trait()
    pool = Pool(group_sizes, normal_trait)
    pool.mate()
    gen = Generator(pool, number_of_generations)

    # [[[print(t.name, v) for t,v in a] for a in g] for g in pool]


if __name__ == '__main__':
    main()
