"""Tests for the main task."""

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

class BasicTestCases(unittest.TestCase):
    """Tests for the main task."""
    
    def test_list_empty(self):
        """ls of empty dir"""
        with NfsObject() as no:
            no.clear()
            actual = os.listdir(MOUNTPOINT)
            self.assertEqual([], actual)
    
    def test_list_default(self):
        """file1 & file2"""
        with NfsObject() as no:
            actual = os.listdir(MOUNTPOINT)
            actual.sort()
            self.assertEqual(["file1", "file2"], actual)
    
    def test_list_many_files(self):
        """16 files in root directory"""
        with NfsObject() as no:
            no.clear()
            for i in range(1, 17):
                no.create("file" + str(i), "file")
            expected = no.list("/")
            actual = os.listdir(MOUNTPOINT)
            expected.sort();
            actual.sort()
            self.assertEqual(expected, actual)
    
    def test_list_nested_dir(self):
        """ls files in /dir directory"""
        with NfsObject() as no:
            no.clear()
            no.create("/dir", "directory")
            no.create("/dir/file1", "file")
            no.create("/dir/file2", "file")
            no.create("/dir/file3", "file")
            expected = no.list("/dir/")
            actual = os.listdir(MOUNTPOINT + "/dir/")
            actual.sort()
            self.assertEqual(expected, actual)
    
    def test_list_stress(self):
        """stress ls"""
        with NfsObject() as no:
            no.clear()
            dirs = []
            while len(dirs) < 5:
                dir_name = ''.join(random.choice(string.ascii_lowercase) for _ in range(255))
                if dir_name not in dirs:
                    dirs.append(dir_name)
                no.create(dir_name, "directory")
            files = []
            while len(files) < 11:
                file_name = ''.join(random.choice(string.digits) for _ in range(255))
                if file_name not in files:
                    files.append(file_name)
                no.create(file_name, "file")
            expected = no.list("/")
            expected.sort();
            actual = os.listdir(MOUNTPOINT)
            actual.sort()
            self.assertEqual(expected, actual)
            for dir in dirs:
                for _ in range(16):
                    file_name = ''.join(random.choice(string.digits) for _ in range(255))
                    no.create("/" + dir + "/" + file_name, "file")
                expected = no.list("/" + dir + "/")
                actual = os.listdir(MOUNTPOINT + "/" + dir + "/")
                expected.sort()
                actual.sort()
                self.assertEqual(expected, actual)
    
    def test_create_file(self):
        """create file"""
        with NfsObject() as no:
            no.clear()
            os.system("touch " + MOUNTPOINT + "/file")
            expected = ["file"]
            actual = no.list("/")
            self.assertEqual(expected, actual)

    def test_create_dir(self):
        """create dir"""
        with NfsObject() as no:
            no.clear();
            os.system("mkdir " + MOUNTPOINT + "/dir")
            expected = ["dir"]
            actual = no.list("/")
            self.assertEqual(expected, actual)

    def test_create_nested_dir(self):
        """create dir, then create /dir/file"""
        with NfsObject() as no:
            no.clear();
            os.system("mkdir " + MOUNTPOINT + "/dir/")
            expected = ["dir"]
            actual = no.list("/")
            self.assertEqual(expected, actual)
            os.system("touch " + MOUNTPOINT + "/dir/file")
            expected = ["file"]
            actual = no.list("/dir/")
            self.assertEqual(expected, actual)

    def test_create_stress(self):
        """stress create"""
        with NfsObject() as no:
            no.clear()
            dirs = []
            while len(dirs) < 5:
                dir_name = ''.join(random.choice(string.ascii_lowercase) for _ in range(255))
                if dir_name not in dirs:
                    dirs.append(dir_name)
                os.system("mkdir " + MOUNTPOINT + "/" + dir_name)
            files = []
            while len(files) < 11:
                file_name = ''.join(random.choice(string.digits) for _ in range(255))
                if file_name not in files:
                    files.append(file_name)
                os.system("touch " + MOUNTPOINT + "/" + file_name)
            expected = [*dirs, *files]
            expected.sort()
            actual = no.list("/")
            actual.sort()
            self.assertEqual(expected, actual)
            for dir in dirs:
                file_name = ''.join(random.choice(string.digits) for _ in range(255))
                dir_name = ''.join(random.choice(string.ascii_lowercase) for _ in range(255))
                os.system("touch " + MOUNTPOINT + "/" + dir + "/" + file_name)
                os.system("mkdir " + MOUNTPOINT + "/" + dir + "/" + dir_name)
                expected = [file_name, dir_name]
                expected.sort()
                actual = no.list("/" + dir + "/")
                actual.sort()
                self.assertEqual(expected, actual)

    def test_rm_file(self):
        """remove file1"""
        with NfsObject() as no:
            os.system("rm " + MOUNTPOINT + "/file1")
            expected = ["file2"]
            actual = no.list("/")
            self.assertEqual(expected, actual)

    def test_rmdir_dir(self):
        """remove dir"""
        with NfsObject() as no:
            no.clear()
            no.create("/dir", "directory")
            expected = ["dir"]
            actual = os.listdir(MOUNTPOINT)
            self.assertEqual(expected, actual)
            os.system("rm -r " + MOUNTPOINT + "/dir")
            actual = no.list("/")
            self.assertEqual([], actual)


    def test_rm_file_and_dir(self):
        """remove file and dir"""
        with NfsObject() as no:
            no.clear()
            no.create("/dir", "directory")
            no.create("/dir/file", "file")
            os.system("rm -r " + MOUNTPOINT + "/dir")
            actual = no.list("/")
            self.assertEqual([], actual)
