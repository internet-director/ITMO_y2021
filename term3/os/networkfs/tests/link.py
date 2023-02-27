"""Tests for the link task."""

import logging
import os
import unittest
import random
import string

from tests.constants import MOUNTPOINT
from tests.nfs import NfsObject

logging.basicConfig(
        level=logging.INFO,
        format='[%(asctime)s] %(filename)s:%(lineno)d: %(message)s'
)

class LinkTestCases(unittest.TestCase):
    """Tests for the link task"""
    def test_link1(self):
        """First link test"""
        with NfsObject() as no:
            os.system("ln " + MOUNTPOINT + "/file1 " + MOUNTPOINT + "/link_file")
            expected = no.read("/file1")
            actual = no.read("/link_file")
            self.assertEqual(expected, actual)

    def test_link2(self):
        """Second link test"""
        with NfsObject() as no:
            no.create("/dir", "directory")
            os.system("ln " + MOUNTPOINT + "/file1 " + MOUNTPOINT + "/dir/link_file")
            expected = no.read("/file1")
            os.system("rm " + MOUNTPOINT + "/file1")
            actual = no.read("/dir/link_file")
            self.assertEqual(expected, actual)
            
