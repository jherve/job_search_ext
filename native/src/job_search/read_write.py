import json
import struct
from enum import Enum
from dataclasses import dataclass, is_dataclass, asdict
from datetime import date
from io import TextIOWrapper

from job_search.messages import Message, BackgroundScriptMessage


class EnhancedJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, Message):
            return o.serialize()
        elif is_dataclass(o):
            return asdict(o)
        elif isinstance(o, Enum):
            return o.value
        elif isinstance(o, date):
            return o.isoformat()
        return super().default(o)


class StdReadWriter:
    @staticmethod
    def read_message(stream):
        raw_length = stream.buffer.read(4)
        if len(raw_length) == 0:
            exit(0)
        message_length = struct.unpack("@I", raw_length)[0]
        message = stream.buffer.read(message_length).decode("utf-8")
        return json.loads(message)

    @staticmethod
    def encode_message(message_content):
        encoded_content = json.dumps(
            message_content, separators=(",", ":"), cls=EnhancedJSONEncoder
        ).encode("utf-8")
        encoded_length = struct.pack("@I", len(encoded_content))
        return {"length": encoded_length, "content": encoded_content}

    @staticmethod
    def send_message_on(stream, encoded_message):
        stream.buffer.write(encoded_message["length"])
        stream.buffer.write(encoded_message["content"])
        stream.buffer.flush()


@dataclass
class ReadWriter:
    stdin: TextIOWrapper
    stdout: TextIOWrapper

    def get_message(self):
        message = StdReadWriter.read_message(self.stdin)
        return BackgroundScriptMessage.interpret(message)

    def send_message(self, message):
        encoded = StdReadWriter.encode_message(message)
        return StdReadWriter.send_message_on(self.stdout, encoded)
