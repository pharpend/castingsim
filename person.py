#!/usr/bin/python

"""Person - code for a person

Any copyright is dedicated to the public domain.

"""


class Person:
    "Code for a person"

    def __init__(self, parents, genetic_iq, env_iq):
        "Make a person."

        self.parents = parents
        self.genetic_iq = genetic_iq
        self.env_iq = env_iq
        self.iq = genetic_iq + env_iq

    def __lt__(self, other):
        "Compare two persons."

        return self.iq < other.iq
