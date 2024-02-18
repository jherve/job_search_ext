import re
import subprocess
from pathlib import Path
from urllib.parse import urlparse, ParseResult
from dataclasses import dataclass, field, asdict
from enum import Enum
from datetime import date, datetime


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
                f.write("\n")
                f.write("%rec: company")
                f.write("%key: name")
                f.write("%allowed: kind url domain")
                f.write("%type: kind enum regular head_hunter ssii start_up")
                f.write("%unique: kind")
                f.write("%unique: url")
                f.write("%unique: domain")
                f.write("\n")

    def read_all(self) -> dict[str, dict]:
        return {r["id"]: r for r in self.select_all("job_offer", "company")}

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
            elif "error: duplicated key value in field 'name' in record" in first:
                raise FileExistsError(f"Duplicate value {fields['name']}")
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

    def select_all(self, type_, join):
        cmd = ["recsel", "-t", type_, "-j", join, str(self.rec_file_path)]
        process = subprocess.run(cmd, stdout=subprocess.PIPE, universal_newlines=True)

        if (code := process.returncode) != 0:
            raise ValueError(f"select command failed with code {code}")

        dict = {}

        records = []
        for r in process.stdout.split("\n\n"):
            dict = {"skills": [], "tags": []}

            # For some reason the last record always gets an extra line break which translates
            # as an extra empty line in the split
            lines = [l for l in re.split(r"\n(?!\+)", r) if l != ""]
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
