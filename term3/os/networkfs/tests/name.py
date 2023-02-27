"""Tests for the bonus-name task."""

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

class NameTestCases(unittest.TestCase):
    def test_1(self):
        """Creating "very important file.txt"."""
        with NfsObject() as no:
            no.clear()
            os.system("touch '" + MOUNTPOINT + "/very important file.txt'")
            expected = ["very important file.txt"]
            actual = no.list("/")
            self.assertEqual(expected, actual)
    def test_2(self):
        """Test from the statement"""
        with NfsObject() as no:
            no.clear();
            os.system("touch '" + MOUNTPOINT + "/!@#$%^&*()-+ '")
            expected = ["!@#$%^&*()-+ "]
            actual = no.list("/")
            self.assertEqual(expected, actual)
    def test_3(self):
        """All symbols from 1 to 127 except /"""
        with NfsObject() as no:
            no.clear()
            filename = ''.join(chr(i) for i in range(32, 127))
            filename = filename.replace("/", "")
            filename = filename.replace("'", "")
            os.system("touch '" + MOUNTPOINT + "/" + filename + "'")
            expected = [filename]
            actual = no.list("/")
            self.assertEqual(expected, actual)
