from dataclasses import dataclass, asdict
from enum import Enum
from typing import Any


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
            match message.pop("values"):
                case []:
                    values = {}
                case [v]:
                    values = v
        except KeyError:
            raise ValueError("messages from background script should contain a tag and values")

        values = {to_snake_case(k): v for k, v in values.items()}

        match tag:
            case "NativeMessageInitialConfiguration":
                return InitialConfigurationMessage(**values)
            case "NativeMessageAddJob":
                return AddJobMessage(values=values)
            case "NativeMessageAddCompany":
                return AddCompanyMessage(values=values)
            case "NativeMessageListJobsRequest":
                return ListJobsRequestMessage(**values)
            case _:
                raise ValueError(f"Got message with unknown tag {tag}")


class NativeMessage(Message):
    def serialize(self):
        raise NotImplementedError(f"No tag was associated to {type(self)} for serialization")


@dataclass
class AddJobMessage(BackgroundScriptMessage):
    """
    For this message we trust the frontend to send data in an appropriate
    format.
    """
    values: dict

    def serialize(self):
        return {"tag": "NativeMessageAddJob", "values": [self.values]}


@dataclass
class AddCompanyMessage(BackgroundScriptMessage):
    """
    For this message we trust the frontend to send data in an appropriate
    format.
    """
    values: dict

    def serialize(self):
        return {"tag": "NativeMessageAddCompany", "values": [self.values]}


@dataclass
class ListJobsRequestMessage(BackgroundScriptMessage):
    pass


@dataclass
class InitialConfigurationMessage(BackgroundScriptMessage):
    jobs_path: str


@dataclass
class JobOfferListMessage(NativeMessage):
    job_offers: list[dict]

    def serialize(self):
        return {"tag": "NativeMessageJobOfferList", "values": [self.job_offers]}


@dataclass
class StorageReadyMessage(NativeMessage):
    def serialize(self):
        return {"tag": "NativeMessageStorageReady", "values": []}


@dataclass
class StorageUpdatedMessage(NativeMessage):
    def serialize(self):
        return {"tag": "NativeMessageStorageUpdated", "values": []}


@dataclass
class JobAddedMessage(NativeMessage):
    job_id: str

    def serialize(self):
        return {"tag": "NativeMessageJobAdded", "values": [asdict(self)]}

@dataclass
class JobAlreadyExistsMessage(NativeMessage):
    job_id: str

    def serialize(self):
        return {"tag": "NativeMessageJobAlreadyExists", "values": [asdict(self)]}


@dataclass
class CompanyAddedMessage(NativeMessage):
    name: str

    def serialize(self):
        return {"tag": "NativeMessageCompanyAdded", "values": [asdict(self)]}


@dataclass
class CompanyAlreadyExistsMessage(NativeMessage):
    name: str

    def serialize(self):
        return {"tag": "NativeMessageCompanyAlreadyExists", "values": [asdict(self)]}


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
