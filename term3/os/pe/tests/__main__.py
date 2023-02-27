"""Tests runner."""

import unittest

from tests.validating import ValidatingPeTestCases
from tests.import_dll import ImportDllTestCases
from tests.import_functions import ImportFunctionTestCases
from tests.export_functions import ExportFunctionTestCases

if __name__ == '__main__':
    unittest.main()
