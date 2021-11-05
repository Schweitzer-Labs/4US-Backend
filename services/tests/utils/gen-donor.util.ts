import * as faker from "faker";
import { EntityType } from "../../src/utils/enums/entity-type.enum";
import { genDonorInput } from "./gen-donor-input.util";
import { IDonor } from "../../src/model/donor.type";
import { genFlacspee } from "../../src/utils/model/gen-donor-match.utils";
import { now } from "../../src/utils/time.utils";
import { genTxnId } from "../../src/utils/gen-txn-id.utils";

export const genDonor = (entityType: EntityType): IDonor => {
  const donorInput = genDonorInput(entityType);
  return {
    ...donorInput,
    flacspeeMatch: genFlacspee(donorInput),
    id: genTxnId(),
    createdTimestamp: now(),
  };
};
