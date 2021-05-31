import { ICommittee } from "../queries/get-committee-by-id.query";
import { IDonor } from "../queries/search-donors.query";

const runComplianceCheck =
  (amount: number) => (committee: ICommittee) => async (donor: IDonor) => {
    // get contribution limit for donor
    // get the total amount the donor has contributed to the committee
    // the total amount the donor has contributed plus the attempted amount exceeds the limit,
    // return left containing the amount remaining
    // else return right containing the amount remaining
  };
