"""Tests for the read-write task."""

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

class WRTestCases(unittest.TestCase):
    """Test for the read-write task."""
    def test_read_default(self):
        """Read default file1 and file2"""
        with NfsObject() as no:
            with open(MOUNTPOINT + "/file1", "r") as f:
                actual = f.read()
                self.assertEqual("hello world from file1", actual)
            with open(MOUNTPOINT + "/file2", "r") as f:
                actual = f.read()
                self.assertEqual("file2 content here", actual)

    def test_read_all_symbols(self):
        """Read all symbols from 0 to 127 inclusive"""
        with NfsObject() as no:
            content = ''.join(chr(i) for i in range(0, 128))
            content = content.replace("\r", "")
            no.write("/file1", content)
            with open(MOUNTPOINT + "/file1", "r") as f:
                actual = f.read()
                self.assertEqual(content, actual)

    def test_write_to_file(self):
        """Simple writing to file"""
        with NfsObject() as no:
            with open(MOUNTPOINT + "/file1", "w") as f:
                f.write("new_content")
            actual = no.read("/file1")
            self.assertEqual("new_content", actual)

    def test_write_all_symbols(self):
        """Write all symbols from 0 to 127 inclusive"""
        with NfsObject() as no:
            content = ''.join(chr(i) for i in range(0, 128))
            content = content.replace("\r", "")      
            with open(MOUNTPOINT + "/file1", "w") as f:
                f.write(content)
            actual = no.read("/file1")
            self.assertEqual(content, actual)

    def test_read_and_write_many_symbols(self):
        content = 'a' * 511
        with NfsObject() as no:
            with open(MOUNTPOINT + "/file1", "w") as f:
                f.write(content)
            with open(MOUNTPOINT + "/file1", "r") as f:
                actual = f.read()
                self.assertEqual(content, actual)
