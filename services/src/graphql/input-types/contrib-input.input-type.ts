import { CreateContributionInput } from "./create-contribution.input-type";
import { AmendContributionInput } from "./amend-contrib.input-type";

export type ContribInput = AmendContributionInput | CreateContributionInput;
