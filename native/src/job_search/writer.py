import sys
import traceback
from pathlib import Path

from job_search.read_write import ReadWriter
from job_search.job_storage import JobStorage
from job_search.messages import (
    VisitedLinkedInJobPageMessage,
    InitialConfigurationMessage,
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
            case VisitedLinkedInJobPageMessage():
                offer = message.extract_job_offer()

                try:
                    self.job_storage.add_job(offer)
                    self.read_writer.send_message(JobAddedMessage(offer))
                except FileExistsError as e:
                    self.read_writer.send_message(JobAlreadyExistsMessage(offer.id))

            case InitialConfigurationMessage(jobs_path):
                self.job_storage = JobStorage(base_dir=Path(jobs_path))

    def loop(self):
        while True:
            try:
                received_message = self.read_writer.get_message()
                self.handle_message(received_message)

                if self.job_storage:
                    offers = list(self.job_storage.read_all().values())
                    self.read_writer.send_message(JobOfferListMessage(offers))

            except Exception as e:
                exc_info = sys.exc_info()
                tb = "".join(traceback.format_exception(*exc_info))
                self.read_writer.send_message(LogMessage.error(content=tb))


if __name__ == "__main__":
    app = Application(sys.stdin, sys.stdout)
    app.loop()
