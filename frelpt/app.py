# -*- coding: utf-8 -*-

import os
import glob

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

        for test in ["complex1", "complex2", "complex3"]:
            apppath = "/Users/youngsun/repos/github/Frelpt/tests/sca/%s/org" % test
            for fname in ("main.f90", "mo_column.f90", "mo_column_extra.f90"):
                macros["%s/%s"%(apppath, fname)] = {} 
                includes["%s/%s"%(apppath, fname)] = [ apppath ]

        #claw_sca_tests = "/Users/youngsun/repos/github/claw-compiler/test/claw/sca"
        claw_sca_test = os.path.realpath(os.path.join(targs.outdir, "..", "org"))

        for fname in ("main.f90", "mo_column.f90"):
            macros["%s/%s"%(claw_sca_test, fname)] = {} 
            includes["%s/%s"%(claw_sca_test, fname)] = [ claw_sca_test ]

        if os.path.isfile(os.path.join(claw_sca_test, "mo_column_extra.f90")):
            macros["%s/mo_column_extra.f90"%claw_sca_test] = {} 
            includes["%s/mo_column_extra.f90"%claw_sca_test] = [ claw_sca_test ]

#        for test in glob.glob(claw_sca_tests + "/sca*"):
#            for fname in ("main.f90", "mo_column.f90"):
#                macros["%s/%s"%(test, fname)] = {} 
#                includes["%s/%s"%(test, fname)] = [ test ]
#
#            if os.path.isfile(os.path.join(test, "mo_column_extra.f90")):
#                macros["%s/mo_column_extra.f90"%test] = {} 
#                includes["%s/mo_column_extra.f90"%test] = [ test ]

        # NOTE: all paths should be realpath and abspath
        self.add_forward(macros=macros, includes=includes)
