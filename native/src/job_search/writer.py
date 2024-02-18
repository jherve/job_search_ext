import sys
import traceback
import asyncio
from pathlib import Path
from dataclasses import asdict
from watchfiles import awatch

from job_search.read_write import ReadWriter
from job_search.job_storage import JobStorage
from job_search.messages import (
    AddJobMessage,
    AddCompanyMessage,
    InitialConfigurationMessage,
    StorageReadyMessage,
    StorageUpdatedMessage,
    ListJobsRequestMessage,
    JobOfferListMessage,
    LogMessage,
    Message,
    JobAddedMessage,
    JobAlreadyExistsMessage,
    CompanyAddedMessage,
    CompanyAlreadyExistsMessage,
)


class Application:
    def __init__(self, stdin, stdout):
        self.read_writer = ReadWriter(stdin, stdout)
        self.job_storage = None

    def handle_message(self, message: Message):
        match message:
            case AddJobMessage(values=values):
                try:
                    self.job_storage.insert_record("job_offer", values)
                    self.read_writer.send_message(JobAddedMessage(values["id"]))
                except FileExistsError as e:
                    self.read_writer.send_message(JobAlreadyExistsMessage(values["id"]))

            case AddCompanyMessage(values=values):
                try:
                    self.job_storage.insert_record("company", values)
                    self.read_writer.send_message(CompanyAddedMessage(values["name"]))
                except FileExistsError as e:
                    self.read_writer.send_message(CompanyAlreadyExistsMessage(values["name"]))

            case ListJobsRequestMessage():
                offers = list(self.job_storage.read_all().values())
                self.read_writer.send_message(JobOfferListMessage(offers))

            case InitialConfigurationMessage(jobs_path):
                self.job_storage = JobStorage(base_dir=Path(jobs_path))
                self.read_writer.send_message(StorageReadyMessage())

            case _:
                self.read_writer.send_message(
                    LogMessage.error(content=f"Received unhandled message : {message}")
                )

    async def loop_on_messages(self):
        loop = asyncio.get_running_loop()
        while True:
            try:
                received_message = await loop.run_in_executor(
                    None, self.read_writer.get_message
                )
                self.handle_message(received_message)

            except Exception as e:
                exc_info = sys.exc_info()
                tb = "".join(traceback.format_exception(*exc_info))
                self.read_writer.send_message(LogMessage.error(content=tb))

    async def watch_changes(self):
        while True:
            await asyncio.sleep(1)
            if self.job_storage:
                async for changes in awatch(self.job_storage.rec_file_path):
                    self.read_writer.send_message(StorageUpdatedMessage())

    async def aloop(self):
        async with asyncio.TaskGroup() as tg:
            task1 = tg.create_task(self.loop_on_messages())
            task2 = tg.create_task(self.watch_changes())


if __name__ == "__main__":
    app = Application(sys.stdin, sys.stdout)
    asyncio.run(app.aloop())
