import unittest

from .test_units import test_classes as unit_tests

def frelpt_unittest_suite():

    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    all_tests = unit_tests

    for test_class in all_tests:
        tests = loader.loadTestsFromTestCase(test_class)
        suite.addTests(tests)

    return suite
