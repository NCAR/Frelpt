
import os
import sys 
import unittest
import tempfile
import shutil
import runpy

import pyloco
import frelpt

_here = os.path.dirname(__file__)

_complex1 = os.path.join(_here, "sca", "complex1", "org")
_target1 = os.path.join(_complex1, "main.f90")
_clean_cmd1 = "cd %s; make clean"%_complex1
_build_cmd1 = "cd %s; make org"%_complex1

_complex2 = os.path.join(_here, "sca", "complex2", "org")
_target2 = os.path.join(_complex2, "main.f90")
_clean_cmd2 = "cd %s; make clean"%_complex2
_build_cmd2 = "cd %s; make org"%_complex2


_complex3 = os.path.join(_here, "sca", "complex3", "org")
_target3 = os.path.join(_complex3, "main.f90")
_clean_cmd3 = "cd %s; make clean"%_complex3
_build_cmd3 = "cd %s; make org"%_complex3


class BasicTests(unittest.TestCase):

    def setUp(self):
        self.tempdir = tempfile.mkdtemp()
        shutil.copy(os.path.join(_complex3, "Makefile"), self.tempdir)

    def tearDown(self):
        shutil.rmtree(self.tempdir)

    def test_basic1(self):

        retval, forward = pyloco.perform(frelpt.FrelptTask, argv=[
            _target1,
            _clean_cmd1,
            _build_cmd1,
            "--outdir", str(self.tempdir),
            "--log", "basictests",
            "--debug",
        ])
 
        out1 = pyloco.system("make", cwd=_complex1)
        out2 = pyloco.system("make", cwd=self.tempdir)

        self.assertEqual(out1[0], 0) 
        self.assertEqual(out2[0], 0) 
        self.assertEqual(out1[1].split()[-1], '195520.172') 
        self.assertEqual(out2[1].split()[-1], '195520.172') 

    def test_basic2(self):

        retval, forward = pyloco.perform(frelpt.FrelptTask, argv=[
            _target2,
            _clean_cmd2,
            _build_cmd2,
            "--outdir", str(self.tempdir),
            "--log", "basictests",
            "--debug",
        ])
 
        out1 = pyloco.system("make", cwd=_complex2)
        out2 = pyloco.system("make", cwd=self.tempdir)

        self.assertEqual(out1[0], 0) 
        self.assertEqual(out2[0], 0) 
        self.assertEqual(out1[1].split()[-1], '195520.172') 
        self.assertEqual(out2[1].split()[-1], '195520.172') 

    def test_basic3(self):

        retval, forward = pyloco.perform(frelpt.FrelptTask, argv=[
            _target3,
            _clean_cmd3,
            _build_cmd3,
            "--outdir", str(self.tempdir),
            "--log", "basictests",
            "--debug",
        ])
 
        out1 = pyloco.system("make", cwd=_complex3)
        out2 = pyloco.system("make", cwd=self.tempdir)

        self.assertEqual(out1[0], 0) 
        self.assertEqual(out2[0], 0) 
        self.assertEqual(out1[1].split()[-1], '38042808.0') 
        self.assertEqual(out2[1].split()[-1], '38042808.0') 


test_classes = (BasicTests,)

