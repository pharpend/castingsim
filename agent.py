#!/usr/bin/python3

"""Agent - class for an agent

Each agent has a list of functions that get its values
"""


class Agent:
    def __init__(self, parents, traits):
        self.parents = parents
        self.traits = traits

    def __iter__(self):
        for tn, tv in self.traits.items():
            yield (tn, tv)

    def __lt__(self, other):
        """determine which is less than the other"""

        return True
