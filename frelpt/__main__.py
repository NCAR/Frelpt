# -*- coding: utf-8 -*-

import sys
import os
import pyloco

from .error import FreError
from .app import AppBuildAnalyzer
from .control import FrelptController

class FrelptTask(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("target", help="filepath to the source file having 'pushdown' frelpt directive")
        self.add_data_argument("build", help="Linux command to compile a target application")
        self.add_data_argument("clean", help="Linux command to clean a target application")

        self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")

    def perform(self, targs):

        retval = 0

        try:

            # check if target file exist
            if not os.path.isfile(targs.target):
                raise FreError("Failed target argument of '%s'"%str(targs.target))

            parent = self.get_proxy()

            # run application build analyzer
            app_analyzer = AppBuildAnalyzer(parent)
            argv = [targs.build, targs.clean, "--outdir", targs.outdir]
            retval, _forward = app_analyzer.run(argv)

            # run frelpt controller
            ctrl = FrelptController(parent)
            argv = [targs.target , "--outdir", targs.outdir]
            retval, _ = ctrl.run(argv, forward=_forward)

        except FreError as err:
            raise

        except Exception as err:
            raise

        return retval

def main(argv=None):

    if argv is None:
        argv = sys.argv

    return pyloco.perform(FrelptTask, argv=argv)

if __name__ == "__main__":
    main()
