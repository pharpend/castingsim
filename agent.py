#!/usr/bin/python3

"""Agent - class for an agent

Each agent has a list of functions that get its values
"""


class Agent:
    def __init__(self, **traits):
        self.traits = traits

        for tn, tv in traits.items():
            setattr(self, tn, tv)

    def __iter__(self):
        for tn, tv in self.traits.items():
            yield tv
