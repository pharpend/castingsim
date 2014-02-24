#!/usr/bin/python3

"""Main - run the casting simulation

main() generates a list of traits, which are each passed a function to
get a variate. It gives the number of people, and the list of traits
to Pool(), which generates a pool.

"""

import sys
import numpy as np
from trait import Trait
from pool import Pool


def main():
    # The group sizes are specified by the command line arguments
    group_sizes = (int(a) for a in sys.argv[1:])

    # This is a trait that is normally distributed
    normal_trait = Trait(np.random.normal)
    one_trait = Trait(lambda: 1)

    pool = Pool(group_sizes, normal_trait=normal_trait, one_trait=one_trait)

    [[print(agent.normal_trait) for agent in group] for group in pool]

if __name__ == '__main__':
    main()
