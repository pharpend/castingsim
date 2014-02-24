#!/usr/bin/python3

"""Pool - class for a pool

Takes a number of agents and a list of traits. Generates agents.

"""

from group import Group


class Pool:
    def __init__(self, group_sizes, **traits):
        self.group_sizes = group_sizes
        self.traits = traits

        self.groups = (Group(n, self.traits) for n in self.group_sizes)

    def __iter__(self):
        return self.groups
