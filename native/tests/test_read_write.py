import pytest
import tempfile
import os
from io import TextIOWrapper
from dataclasses import asdict

from job_search.read_write import StdReadWriter, ReadWriter
from job_search.messages import (
    InitialConfigurationMessage,
    JobOfferListMessage,
    JobAddedMessage,
    JobAlreadyExistsMessage,
    LogMessage,
)


def fake_std():
    (fd, file_path) = tempfile.mkstemp(prefix="job_search")
    yield file_path
    os.remove(file_path)


@pytest.fixture
def stdin():
    yield from fake_std()


@pytest.fixture
def stdout():
    yield from fake_std()


@pytest.fixture
def stdin_read(stdin):
    with open(stdin, "rb") as fake:
        yield TextIOWrapper(fake)


@pytest.fixture
def stdin_write(stdin):
    with open(stdin, "wb") as fake:
        yield TextIOWrapper(fake)


@pytest.fixture
def stdout_write(stdout):
    with open(stdout, "wb") as fake:
        yield TextIOWrapper(fake)


@pytest.fixture
def stdout_read(stdout):
    with open(stdout, "rb") as fake:
        yield TextIOWrapper(fake)


@pytest.fixture
def read_writer(stdin_read, stdout_write):
    return ReadWriter(stdin_read, stdout_write)


class TestStdReadWriter:
    def test_get_message(self, stdin_read, stdin_write):
        simple_message = {"test": "pouet"}

        msg = StdReadWriter.encode_message(simple_message)
        StdReadWriter.send_message_on(stdin_write, msg)

        assert StdReadWriter.read_message(stdin_read) == simple_message

    def test_send(self, stdout_write, stdout_read):
        simple_message = {"test": "pouet"}

        msg = StdReadWriter.encode_message(simple_message)
        StdReadWriter.send_message_on(stdout_write, msg)

        assert StdReadWriter.read_message(stdout_read) == simple_message


class TestReadWriter:
    @pytest.fixture(
        params=[
            (
                {"tag": "NativeMessageInitialConfiguration", "values": [{"jobsPath": "jobs_path"}]},
                InitialConfigurationMessage(jobs_path="jobs_path"),
            )
        ]
    )
    def input_message(self, request):
        (ext_message_as_dict, message) = request.param

        encoded = StdReadWriter.encode_message(ext_message_as_dict)
        return message, encoded

    def test_get_message(self, read_writer, stdin_write, input_message):
        expected_message, encoded = input_message
        StdReadWriter.send_message_on(stdin_write, encoded)

        assert read_writer.get_message() == expected_message

    @pytest.fixture(
        params=[
            (
                JobOfferListMessage(job_offers=[{"id": "job_offer_1"}, {"id": "job_offer_2"}]),
                {"tag": "NativeMessageJobOfferList", "values": [[{"id": "job_offer_1"}, {"id": "job_offer_2"}]]},
            ),
            (
                JobAddedMessage(job_id="job_id"),
                {"tag": "NativeMessageJobAdded", "values": [{"job_id": "job_id"}]},
            ),
            (
                JobAlreadyExistsMessage(job_id="job_id"),
                {"tag": "NativeMessageJobAlreadyExists", "values": [{"job_id": "job_id"}]},
            ),
            (
                LogMessage.debug(content="debug_content"),
                {"tag": "NativeMessageLog", "values": [{"level": "debug", "content": "debug_content"}]},
            ),
            (
                LogMessage.info(content={"message": "info"}),
                {"tag": "NativeMessageLog", "values": [{"level": "info", "content": {"message": "info"}}]},
            ),
            (
                LogMessage.error(content="error_content"),
                {"tag": "NativeMessageLog", "values": [{"level": "error", "content": "error_content"}]},
            ),
        ]
    )
    def output_message(self, request):
        return request.param

    def test_send_message(self, read_writer, stdout_read, output_message):
        original_message, expected_json = output_message

        read_writer.send_message(original_message)
        assert StdReadWriter.read_message(stdout_read) == expected_json
