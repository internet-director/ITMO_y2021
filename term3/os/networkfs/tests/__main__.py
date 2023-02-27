"""Tests runner."""

import unittest

from tests.fs import TestPreparer

from tests.basic import BasicTestCases
from tests.name import NameTestCases
from tests.wr import WRTestCases
from tests.link import LinkTestCases

if __name__ == '__main__':
    preparer = TestPreparer()
    try:
        preparer.prepare()
        unittest.main()
    finally:
        preparer.release()
