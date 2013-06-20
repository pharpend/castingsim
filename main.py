#!/usr/bin/env python3

'''
Casting Simulator - This program simulates genetic casting in a meritocracy
Any copyright is dedicated to the Public Domain.
'''

'''
This program is a discrete process.

1. Put everyone in one pool (n people).
2. Put people into (g) discrete groups of size (n/g).
3. Have everyone in each group mate and die such that the population remains at
(n/g).
4. Run whatever intermittent data collection is wanted.
5. Repeat this process T times.
'''

import sys
import traceback
import matplotlib.pyplot as plt
import numpy as np
import pool


def intermittent_process(iteration_num: int):
    '''
    This is the code that runs between the groups getting split up, and then put
    back together. The idea is, people can put whatever stat-collecting code in
    here.
    '''

    global general_pool
    global individual_groups
    global number_of_hist_bins
    global text_only

    iqs = general_pool.get_distribution()
    if not text_only:
        # Make a histogram. This code was stolen from Stack Overflow
        # <http://stackoverflow.com/questions/5328556/histogram-matplotlib>.
        number_of_hist_bins = 250
        hist, bins = np.histogram(iqs, bins=number_of_hist_bins)
        width = 0.7 * (bins[1] - bins[0])
        center = (bins[:-1] + bins[1:]) / 2
        plt.bar(center, hist, align='center', width=width)
        plt.show()

    # This code prints out the various statistics
    print('\nIteration: %d' % iteration_num)
    # print('==General Pool==')
    # print('Mean IQ: %.2f' % np.mean(iqs))
    # print('Stdv IQ: %.2f' % np.std(iqs))
    group_num = -1
    for group in individual_groups:
        group_num += 1
        dist = group.get_distribution()

        print('==Group %d==' % group_num)
        print('Mean IQ: %.2f' % np.mean(dist))
        print('Stdv IQ: %.2f' % np.std(dist))

    # Each group should mate
    individual_groups = [group.poisson_mate() for group in individual_groups]
# end of intermittent_process()


def get_group_sizes(f: str):
    '''
    This returns an array of group sizes, from a file.
    '''

    # Obfuscated code entry??? It reads everything from a file, eliminates
    # whitespace, converts it to floating-point form, and puts it into a nice
    # little list for us.
    return [float(i) for i in open(f, 'r').read().split()]


def main():
    '''
    This parses command line arguments, then runs sim().
    '''

    global number_of_runs
    global mean
    global standard_deviation
    global heritability
    global text_only
    global group_sizes

    try:
        number_of_runs = int(sys.argv[1])
        mean = float(sys.argv[2])
        standard_deviation = float((sys.argv[3]))
        heritability = float(sys.argv[4])
        text_only = float(sys.argv[5])
        group_sizes = get_group_sizes(sys.argv[6])

        print('Number of runs: ', number_of_runs)
        print('Mean: ', mean)
        print('Standard deviation: ', standard_deviation)
        print('Heritability: ', heritability)
        print('Text-only output? ', text_only)
        print('Group sizes: ', group_sizes)

        sim()

    # If the arguments are invalid or don't exist
    except Exception:
        print(traceback.format_exc())

        help_message = 'Usage: python main.py r m s h t g\n'
        help_message += 'r = number of runs\n'
        help_message += 'm = mean\n'
        help_message += 's = standard deviation\n'
        help_message += 'h = heritability\n'
        help_message += 't = text output only?\n'
        help_message += 'g = groups file\n'

        print(help_message)
        exit()
# end of main()


def make_pool():
    '''
    This function makes, and returns a Pool object.
    '''

    global number_of_runs
    global mean
    global standard_deviation
    global heritability
    global group_sizes
    global genetic_population
    global env_population

    # The genetic std is a bit harder
    variance = standard_deviation ** 2
    genetic_variance = heritability * variance
    genetic_standard_deviation = genetic_variance ** 0.5

    number_of_people = np.sum(group_sizes)

    # The environmential variables are whatever is left
    env_variance = variance - genetic_variance
    env_standard_deviation = env_variance ** 0.5

    # First, make the population according to a normal distribution
    genetic_population = np.random.normal(mean,
                                          genetic_standard_deviation,
                                          number_of_people)
    env_population = np.random.normal(0,
                                      env_standard_deviation,
                                      number_of_people)

    return pool.Pool(genetic_population, env_population)
# end of make_pool()


def sim():
    '''
    This function actually runs the simulation.

    So, assuming the heritability is h, and the trait we are measuring is IQ,
    the genetic mean for IQ is h*(mean_iq).

    The variance for IQ would be standard_deviation**2. So, the genetic standard
    deviation is sqrt(standard_deviation**2)*.8)
    '''

    # Global variables
    global number_of_runs
    global mean
    global standard_deviation
    global heritability
    global group_sizes
    global general_pool
    global individual_groups

    # Make the initial people objects
    general_pool = make_pool()

    # Run the simulation
    for i in range(number_of_runs):
        # Split up the groups
        individual_groups = general_pool.partition(group_sizes)

        # Run whatever is in intermittent_process()
        intermittent_process(i)

        # Conglomerate the pools
        general_pool.drain()
        for group in individual_groups:
            general_pool += group

    # end of for

# end of make()


if __name__ == '__main__':
    main()
