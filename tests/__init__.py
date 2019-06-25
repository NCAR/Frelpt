import unittest

from .test_basics import test_classes as basic_tests
from .test_clawsca import test_classes as clawsca_tests

def frelpt_unittest_suite():

    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    all_tests = basic_tests + clawsca_tests

    for test_class in all_tests:
        tests = loader.loadTestsFromTestCase(test_class)
        suite.addTests(tests)

    return suite
