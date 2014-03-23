#!/usr/bin/python3

"""Pool - class for a pool

Takes a number of agents and a list of traits. Generates agents.

"""

from agent import Agent
from group import Group


class Pool:
    def __init__(self, group_sizes, *traits):
        self.group_sizes = group_sizes
        self.n = sum(group_sizes)
        self.traits = traits

        self.agents = [Agent(None, {trait: trait.get_variate() for
                                    trait in traits}) for i in range(self.n)]
        self.agents.sort()
        self.agents_groups = self.agents
        self.groups = [Group(n, self.traits) for n in self.group_sizes]

    def __iter__(self):
        for g in self.groups:
            yield g

    def mate(self):
        for group in self.groups:
            group.mate()
