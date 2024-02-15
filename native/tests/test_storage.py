import pytest
from datetime import date, datetime
from job_search.job_storage import (
    JobStorage,
    JobOffer,
    JobOfferOrigin,
    ApplicationProcess,
    CompanyKind,
    ContractType,
    Flexibility,
)


@pytest.fixture(params=[JobStorage])
def job_storage(request, tmp_path):
    return request.param(base_dir=tmp_path)


@pytest.fixture(
    params=[
        JobOffer(
            id="linked_in_3755217595",
            url="https://www.linkedin.com/jobs/view/3755217595",
            title="Job title",
            company="Company",
            origin=JobOfferOrigin.LINKED_IN,
            application_process=ApplicationProcess.REGULAR,
            location="location",
            company_domain="domain",
            company_url="https://www.linkedin.com/company/the-company/life",
            publication_date=date.today(),
        ),
        JobOffer(
            id="linked_in_3755217595",
            url="https://www.linkedin.com/jobs/view/3755217595",
            title="Job title",
            company="Company",
            origin=JobOfferOrigin.LINKED_IN,
            application_process=ApplicationProcess.REGULAR,
            location="location",
            company_domain="domain",
            company_url="https://www.linkedin.com/company/the-company/life",
            publication_date=date.today(),
            skills=["skill1", "skill2"],
            tags=["tag1", "tag2"],
            xp_required=2,
            company_kind=CompanyKind.REGULAR,
            comment="comment",
            description="description",
            contract_type=ContractType.CDD,
            flexibility=Flexibility.HYBRID,
            alternate_url="https://www.anothersite.com/with/the/offer.html",
            application_date=date.today(),
            application_rejection_date=date.today(),
        ),
        JobOffer(
            id="linked_in_3755217595",
            url="https://www.linkedin.com/jobs/view/3755217595",
            title="Job title",
            company="Company",
            origin=JobOfferOrigin.LINKED_IN,
            application_process=ApplicationProcess.REGULAR,
            location="location",
            company_domain="domain",
            company_url="https://www.linkedin.com/company/the-company/life",
            comment="""
multi
line
comment
            """,
            publication_date=date.today(),
        ),
    ]
)
def linked_in_job_offer(request):
    return request.param


class TestJobStorage:
    def test_job_storage_empty_on_startup(self, job_storage):
        assert job_storage.read_all() == {}

    def test_job_addition(self, job_storage, linked_in_job_offer):
        job_storage.insert_record("job_offer", linked_in_job_offer.to_storage())

        all_items = job_storage.read_all().items()
        assert len(all_items) == 1

        [(id, stored_job)] = all_items
        assert isinstance(stored_job.first_seen_date, datetime)
        assert id == stored_job.id

        # Reset the first_seen_date to None, for comparison with the non-stored version
        stored_job.first_seen_date = None
        assert stored_job == linked_in_job_offer

    def test_job_duplicate_addition(self, job_storage, linked_in_job_offer):
        job_storage.insert_record("job_offer", linked_in_job_offer.to_storage())

        with pytest.raises(FileExistsError) as excinfo:
            job_storage.insert_record("job_offer", linked_in_job_offer.to_storage())

        assert linked_in_job_offer.id in str(excinfo.value)
