# Introduction

The code in this repository simulates casting in a meritocracy.

# Installation

To get the latest version of the source code, pop open a terminal, and
run the following commands.

```
$ git clone git@github.com:pharpend/castingsim.git
$ cd castingsim
```

Of course, if you already have it installed, you can always update your
local version with `git pull`.

For this program to work, you need Python>=3.3, Numpy>=1.7, and MatPlotLib>=1.2

# Usage

`main.py` is executable. To run a simulation, run

```
$ ./main.py r m s h t &rest g
```

* `r` is the number of runs; that is, the number of generations over
  which the simulation will be run.
* `m` is the mean merit in the general population.
* `s` is the standard deviation of said merit. `h` is the heritability
  factor of said merit.
* `t` is either `0` or `1`, depending on whether you want graphs or not,
  irrespectively.
* `g` is the list of group sizes
  

# Modifying the code

## Copying it, Modifying it, Redistributing it, etc.

The code in this repo is in the public domain, so you are free to do
with it as you choose.

## Modification for personal use

I built the program in such a manner that it *should* be easy to modify
for your own purposes. I imagine most stat-collecting will take place in
the time period between the groups being split apart, and being put back
together.

Between those two actions is a function called
`intermittent_process()`. It is at the top of `main.py`. Unless you are
doing something super fancy, your code should probably go there.

## Submitting patches/fixes/features

Any contributions are welcome. Clone the code, modify it, upload it,
file a pull
request. [You know the drill.](https://help.github.com/articles/fork-a-repo)
