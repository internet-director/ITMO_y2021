"""Service class for preparing test runner."""
import logging
import os
import subprocess

from tests.constants import MODULE, MOUNTPOINT

logging.basicConfig(
    level=logging.INFO,
    format='[%(asctime)s] %(filename)s:%(lineno)d: %(message)s'
)


class TestPreparer:
    """Generic tests, _send_and_assert should be replaced."""

    class NoModuleFound(Exception):
        """No module presented in current directory."""
        def __init__(self):
            super().__init__("Ensure that " + MODULE + ".ko is present in the current directory.")

    class OperationNotPermitted(Exception):
        """Not enough privileges."""
        def __init__(self):
            super().__init__("Ensure that tests are run with root privileges.")

    class UnexpectedError(Exception):
        """Some unexpected error."""

    class InvalidDevice(Exception):
        """Device doesn't match with expected characteristics."""

    module_loaded = False
    mountpoint_created = False

    def prepare(self):
        """Loads the module and enables filesystem."""
        if not os.path.exists(MODULE + ".ko"):
            raise self.NoModuleFound()

        try:
            subprocess.run(
                ["insmod", MODULE + ".ko"],
                check=True,
                stderr=subprocess.PIPE
            )
            self.module_loaded = True
        except subprocess.CalledProcessError as exc:
            if b"Operation not permitted" in exc.stderr:
                raise self.OperationNotPermitted() from exc
            elif b"File exists" in exc.stderr:
                # Module is already present, good job
                logging.info("Using already loaded module.")
            else:
                raise

        if not os.path.exists(MOUNTPOINT):
            self.mountpoint_created = True
            os.makedirs(MOUNTPOINT)

        with open("/proc/mounts", "r") as mountfile:
            mounts = mountfile.read()

        if MOUNTPOINT + " " + MODULE + " rw" in mounts:
            logging.error("Filesystem is already mounted. Unmount it to run tests from scratch.")
            raise FileExistsError("Filesystem is already mounted.")

    def release(self):
        """Cleans every loaded thing."""
        if self.mountpoint_created:
            try:
                os.rmdir(MOUNTPOINT)
            except OSError as exc:
                logging.warning("Can't remove mountpoint", exc_info=exc)

        if self.module_loaded:
            try:
                subprocess.run(["rmmod", MODULE], check=True, stderr=subprocess.PIPE)
            except subprocess.CalledProcessError as exc:
                logging.warning("Can't unload module", exc_info=exc)
