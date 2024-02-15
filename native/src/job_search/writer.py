import sys
import traceback
from pathlib import Path
from dataclasses import asdict

from job_search.read_write import ReadWriter
from job_search.job_storage import JobStorage
from job_search.messages import (
    AddJobMessage,
    InitialConfigurationMessage,
    StorageReadyMessage,
    ListJobsRequestMessage,
    JobOfferListMessage,
    LogMessage,
    Message,
    JobAddedMessage,
    JobAlreadyExistsMessage,
)


class Application:
    def __init__(self, stdin, stdout):
        self.read_writer = ReadWriter(stdin, stdout)
        self.job_storage = None

    def handle_message(self, message: Message):
        match message:
            case AddJobMessage():
                try:
                    self.job_storage.insert_record("job_offer", asdict(message))
                    self.read_writer.send_message(JobAddedMessage(message.id))
                except FileExistsError as e:
                    self.read_writer.send_message(JobAlreadyExistsMessage(message.id))

            case ListJobsRequestMessage():
                offers = list(self.job_storage.read_all().values())
                self.read_writer.send_message(JobOfferListMessage(offers))

            case InitialConfigurationMessage(jobs_path):
                self.job_storage = JobStorage(base_dir=Path(jobs_path))
                self.read_writer.send_message(StorageReadyMessage())

    def loop(self):
        while True:
            try:
                received_message = self.read_writer.get_message()
                self.handle_message(received_message)

            except Exception as e:
                exc_info = sys.exc_info()
                tb = "".join(traceback.format_exception(*exc_info))
                self.read_writer.send_message(LogMessage.error(content=tb))


if __name__ == "__main__":
    app = Application(sys.stdin, sys.stdout)
    app.loop()
