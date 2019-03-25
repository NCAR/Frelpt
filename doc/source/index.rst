.. frelpt documentation master file, created by
   sphinx-quickstart on Mon Mar 25 14:23:20 2019.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Frelpt Documentation
==================================

Frelpt is a loop pushdown translation tool written in Python.

Loop pushdown is a type of inter-procedural loop transformations that *pushes* a loop down to terminal
Fortran statements in the bottom of Fortran call stack.

Motivation of this transformation is to transform legacy Fortran code to run fast on modern processors
such Graphical Processing Unit(GPU) and many-core Central Processing Unit(CPU). To be utilized fully, 
the modern processing units require a plenty of parallelization and/or vectorizatin opportunities 
concentrated on a relatively small code region. For example, to utilize GPU well, it is essential
to create a computation intensive kernel and run the kernel on GPU as long as possible.

Loop pushdown effectively transforms a program structure such that loops are located in the bottom of
Fortran call stack so that computation intensitive increases locally at the bottom of call stack.


.. toctree::
   :maxdepth: 2

   getting_started


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
