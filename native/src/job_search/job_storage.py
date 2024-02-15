import re
import subprocess
from pathlib import Path
from urllib.parse import urlparse, ParseResult
from dataclasses import dataclass, field, asdict
from enum import Enum
from datetime import date, datetime
from email.utils import parsedate_to_datetime


class JobOfferOrigin(Enum):
    LINKED_IN = "linked_in"
    OTHER = "other"


class CompanyKind(Enum):
    SSII = "ssii"
    START_UP = "start_up"
    HEAD_HUNTER = "head_hunter"
    REGULAR = "regular"


class ApplicationProcess(Enum):
    LINKED_IN_SIMPLIFIED = "linked_in_simplified"
    REGULAR = "regular"
    CAREER_SITE = "career_site"
    SPURIOUS = "spurious"


class ContractType(Enum):
    CDI = "CDI"
    CDD = "CDD"
    NOT_A_JOB = "not_a_job"


class Flexibility(Enum):
    ON_SITE = "on_site"
    HYBRID = "hybrid"
    FULL_REMOTE = "full_remote"


def convert_to_parse_result(url):
    if isinstance(url, str):
        return urlparse(url)._replace(query=None)
    elif isinstance(url, ParseResult):
        return url

def convert_to_bool(s: str) -> bool:
    return s == "true" or s == "yes" or s == "1"


@dataclass
class JobOffer:
    id: str = field(init=False)
    url: str = field(repr=False)
    title: str
    company: str
    origin: JobOfferOrigin
    location: str
    application_process: ApplicationProcess | None = None
    company_url: str = ""
    description: str = ""
    company_kind: CompanyKind | None = None
    company_domain: str = ""
    comment: str = ""
    tags: list[str] = field(default_factory=list)
    skills: list[str] = field(default_factory=list)
    publication_date: date = None
    xp_required: int | None = None
    first_seen_date: datetime | None = None
    application_considered: bool | None = None
    application_date: date | None = None
    application_rejection_date: date | None = None
    contract_type: ContractType | None = ContractType.CDI
    flexibility: Flexibility | None = None
    alternate_url: str = None
    _url: ParseResult = field(init=False, repr=False)
    _company_url: ParseResult = field(init=False, repr=False)
    _alternate_url: ParseResult = field(init=False, repr=False, default=None)

    def __post_init__(self):
        self._url = convert_to_parse_result(self.url)
        self.url = self._url.geturl()

        self._company_url = convert_to_parse_result(self.company_url)
        self.company_url = self._company_url.geturl()

        if self.alternate_url:
            self._alternate_url = convert_to_parse_result(self.alternate_url)
            self.alternate_url = self._alternate_url.geturl()

        if self.origin == JobOfferOrigin.LINKED_IN:
            path = Path(self._url.path)
            self.id = f"linked_in_{path.name}"

    def to_storage(self):
        return {
            k: v
            for k, v in asdict(self).items()
            if k not in ["_url", "_company_url", "_alternate_url"]
        }

    @staticmethod
    def from_storage(dict: dict):
        id = dict.pop("id")

        for field, converter in [
            ("origin", JobOfferOrigin),
            ("application_process", ApplicationProcess),
            ("company_kind", CompanyKind),
            ("contract_type", ContractType),
            ("flexibility", Flexibility),
            ("xp_required", int),
            ("first_seen_date", parsedate_to_datetime),
            ("publication_date", date.fromisoformat),
            ("application_considered", convert_to_bool),
            ("application_date", date.fromisoformat),
            ("application_rejection_date", date.fromisoformat),
        ]:
            if field in dict:
                dict[field] = converter(dict[field])

        # For now we simply ignore application-related fields
        # read from the storage.
        for k in [
            "application_first_seen_date",
            "application_first_response_date",
            "application_cv_version",
            "application_appointments",
            "application_message",
            "application_questions",
            "application_url",
            "application_contacts",
        ]:
            try:
                del dict[k]
            except KeyError:
                pass

        return JobOffer(**dict)


def remove_whitespace(s):
    s = re.sub(r"[^\w\s]", "", s)
    s = re.sub(r"\s+", "_", s)
    return s


@dataclass
class JobStorage:
    base_dir: Path
    rec_file_path: Path = field(init=False, repr=False)

    def __post_init__(self):
        if not self.base_dir.is_absolute():
            raise ValueError(
                f"The base dir path should be absolute, got '{self.base_dir}'"
            )

        self.rec_file_path = self.base_dir / "jobs.rec"

        # Create the rec file if it does not exist yet, otherwise
        # leave it as-is.
        try:
            f = open(self.rec_file_path)
            f.close()
        except FileNotFoundError:
            with open(self.rec_file_path, "w+") as f:
                f.write("%rec: job_offer\n")
                f.write("%key: id\n")
                f.write("%type: publication_date date\n")
                f.write("%type: company_kind enum regular head_hunter ssii start_up\n")
                f.write(
                    "%type: application_process enum regular linked_in_simplified\n"
                )
                f.write("%type: xp_required range 0 MAX\n")
                f.write("%type: origin enum linked_in other\n")
                f.write("%type: contract_type enum CDI CDD not_a_job\n")
                f.write("%type: flexibility enum on_site hybrid full_remote\n")
                f.write("%type: first_seen_date date\n")
                f.write("%auto: first_seen_date\n")

    def read_all(self) -> dict[str, JobOffer]:
        return {r["id"]: JobOffer.from_storage(r) for r in self.select_all("job_offer")}

    def add_job(self, offer: JobOffer):
        self.insert(offer)

    def insert(self, offer: JobOffer):
        self.insert_record("job_offer", offer.to_storage())

    def insert_record(self, type_, fields):
        cmd_args = [
            arg
            for k, v in self.into_args(fields)
            for arg in ["-f", k, "-v", v]
            if v is not None and v != [] and v != ""
        ]
        cmd = (
            ["recins", "--verbose", "-t", type_] + cmd_args + [str(self.rec_file_path)]
        )
        process = subprocess.run(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True
        )

        if (code := process.returncode) != 0:
            error_lines = process.stderr.splitlines()
            first = error_lines[0]

            if "error: invalid enum value" in first:
                raise ValueError(f"Found invalid enum value in {fields}")
            elif "error: duplicated key value in field 'id' in record" in first:
                raise FileExistsError(f"Duplicate value {fields['id']}")
            else:
                raise ValueError(
                    f"insert command failed with code {code} :\n{process.stderr}"
                )

    @staticmethod
    def into_args(fields: dict) -> list[tuple]:
        args = []
        for k, v in fields.items():
            if isinstance(v, list):
                args += [(k, item) for item in v]
            elif isinstance(v, int):
                args += [(k, str(v))]
            elif isinstance(v, Enum):
                args += [(k, v.value)]
            elif isinstance(v, date):
                args += [(k, v.isoformat())]
            else:
                args += [(k, v)]
        return args

    def select_all(self, type_):
        cmd = ["recsel", "-t", type_, str(self.rec_file_path)]
        process = subprocess.run(cmd, stdout=subprocess.PIPE, universal_newlines=True)

        if (code := process.returncode) != 0:
            raise ValueError(f"select command failed with code {code}")

        dict = {}

        records = []
        for r in process.stdout.split("\n\n"):
            dict = {"skills": [], "tags": []}

            lines = re.split(r"\n(?!\+)", r)[:-1]
            for l in lines:
                # We assume fields are not empty
                [field, value] = l.split(": ", 1)

                # Handle multiline records. This will not work if the optional space if not present
                # after the PLUS sign.
                value = "\n".join(value.split("\n+ "))

                if field in ["skills", "tags"]:
                    dict[field].append(value)
                else:
                    dict[field] = value

            if lines != []:
                records.append(dict)

        return records
