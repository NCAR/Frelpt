# -*- coding: utf-8 -*-

import os

import pyloco

class AppBuildAnalyzer(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("clean", help="Linux command to clean a target application")
        self.add_data_argument("build", help="Linux command to compile a target application")

        self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")

        self.register_forward("macros", help="macro definitions used during compilation")
        self.register_forward("includes", help="include directories used during compilation")
        #self.register_forward("macros", type=dict, help="macro definitions used during compilation")
        #self.register_forward("includes", type=dict, help="include directories used during compilation")


    def perform(self, targs):

        # for dev on my Macpro only
        macros, includes = {}, {}
        apppath = "/Users/youngsun/repos/github/Frelpt/tests/sca/complex1"
        for fname in ("main.f90", "mo_column.f90", "mo_column_extra.f90"):
            macros["%s/%s"%(apppath, fname)] = {} 
            includes["%s/%s"%(apppath, fname)] = [ apppath ]

        # NOTE: all paths should be realpath and abspath
        self.add_forward(macros=macros, includes=includes)
