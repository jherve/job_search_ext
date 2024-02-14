import re
from datetime import date
from dataclasses import dataclass, asdict
from enum import Enum
from typing import Optional, Any
from job_search.job_storage import (
    JobOffer,
    ApplicationProcess,
    JobOfferOrigin,
    Flexibility,
)


def to_snake_case(string):
    return "".join("_" + c.lower() if c.isupper() else c for c in string)


class Message:
    ...


class BackgroundScriptMessage(Message):
    @staticmethod
    def interpret(message):
        if not isinstance(message, dict):
            raise TypeError(f"message should be a dict, got {type(message)}")

        try:
            tag = message.pop("tag")
            [values] = message.pop("values")
        except KeyError:
            raise ValueError("messages from background script should contain a tag and values")

        values = {to_snake_case(k): v for k, v in values.items()}

        match tag:
            case "NativeMessageVisitedJobPage":
                return VisitedLinkedInJobPageMessage(**values)
            case "NativeMessageInitialConfiguration":
                return InitialConfigurationMessage(**values)
            case _:
                raise ValueError(f"Got message with unknown tag {tag}")


class NativeMessage(Message):
    def serialize(self):
        raise NotImplementedError(f"No tag was associated to {type(self)} for serialization")


@dataclass
class VisitedLinkedInJobPageMessage(BackgroundScriptMessage):
    url: str
    job_title: str
    page_title: str
    company: str
    location: str
    has_simplified_process: bool
    company_url: str
    flexibility: Optional[str] = None
    company_domain: Optional[str] = None

    def extract_job_offer(self):
        application_process = (
            ApplicationProcess.LINKED_IN_SIMPLIFIED
            if self.has_simplified_process
            else ApplicationProcess.REGULAR
        )

        if isinstance(self.flexibility, str):
            flexibility = Flexibility(self.flexibility)
        elif self.flexibility is None:
            flexibility = None

        return JobOffer(
            url=self.url,
            title=self.job_title,
            company=self.company,
            origin=JobOfferOrigin.LINKED_IN,
            application_process=application_process,
            location=self.location,
            company_domain=self.company_domain,
            company_url=self.company_url,
            flexibility=flexibility,
        )


@dataclass
class InitialConfigurationMessage(BackgroundScriptMessage):
    jobs_path: str


@dataclass
class JobOfferListMessage(NativeMessage):
    job_offers: list[JobOffer]

    def serialize(self):
        return {"tag": "NativeMessageJobOfferList", "values": [self.job_offers]}


@dataclass
class JobAddedMessage(NativeMessage):
    job: JobOffer

    def serialize(self):
        return {"tag": "NativeMessageJobAdded", "values": [asdict(self)]}


@dataclass
class JobAlreadyExistsMessage(NativeMessage):
    job_id: str

    def serialize(self):
        return {"tag": "NativeMessageJobAlreadyExists", "values": [asdict(self)]}


class LogLevel(Enum):
    DEBUG = "debug"
    INFO = "info"
    ERROR = "error"


@dataclass
class LogMessage(NativeMessage):
    level: LogLevel
    content: Any

    @staticmethod
    def debug(**kwargs):
        return LogMessage(level=LogLevel.DEBUG, **kwargs)

    @staticmethod
    def info(**kwargs):
        return LogMessage(level=LogLevel.INFO, **kwargs)

    @staticmethod
    def error(**kwargs):
        return LogMessage(level=LogLevel.ERROR, **kwargs)

    def serialize(self):
        return {"tag": "NativeMessageLog", "values": [asdict(self)]}
