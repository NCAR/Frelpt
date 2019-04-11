# -*- coding: utf-8 -*-

import sys
import pyloco

from .main import FrelptTask

def main(argv=None):


    if argv is None:
        argv = sys.argv

    return pyloco.perform(FrelptTask, argv=argv)

if __name__ == "__main__":
    main()
