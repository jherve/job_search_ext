import pytest
from datetime import date, datetime
from job_search.messages import VisitedLinkedInJobPageMessage
from job_search.job_storage import (
    JobOffer,
    JobOfferOrigin,
    ApplicationProcess,
    Flexibility,
)


@pytest.fixture(
    params=[
        (
            VisitedLinkedInJobPageMessage(
                url="https://www.linkedin.com/jobs/view/3755217595",
                job_title="Job title",
                page_title="Page title",
                company="Company",
                location="location",
                company_domain="domain",
                company_url="https://www.linkedin.com/company/the-company/life",
                has_simplified_process=True,
                flexibility=Flexibility.FULL_REMOTE.value,
            ),
            JobOffer(
                url="https://www.linkedin.com/jobs/view/3755217595",
                title="Job title",
                company="Company",
                origin=JobOfferOrigin.LINKED_IN,
                application_process=ApplicationProcess.LINKED_IN_SIMPLIFIED,
                location="location",
                company_domain="domain",
                company_url="https://www.linkedin.com/company/the-company/life",
                flexibility=Flexibility.FULL_REMOTE,
            ),
        ),
    ]
)
def message_job_offer(request):
    return request.param


class TestJobOfferExtraction:
    def test_extract_from_visited_linkedin(self, message_job_offer):
        (message, expected_job_offer) = message_job_offer
        assert message.extract_job_offer() == expected_job_offer
