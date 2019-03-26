.. -*- coding: utf-8 -*-


.. _getting_started:

Getting Started
================

:ref:`Frelpt` is a loop pushdown translation tool written in Python.

Frelpt is mainly tested on Python 2.7 and Python 3.6. As of this version, Frelpt supports Linux OS only.

Installation
--------------

First install Frelpt using pip:

.. code-block:: bash

    >>> pip install frelpt
    >>> frelpt --version
    frelpt version 0.0.1

To run Frelpt properly, following utilities are also required:

    * make   : make utility
    * cpp    : C preprocessor
    * strace : trace system calls and signals
