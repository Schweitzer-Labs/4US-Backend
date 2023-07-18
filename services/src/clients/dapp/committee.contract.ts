import { dashToUnderscore } from "../../utils/dash-to-underscore.utils";

export const committeeContractWithHash = (committeeId: string) => {
  const committee_id = dashToUnderscore(committeeId);
  return `
pragma solidity ^0.4.25;

contract Transaction_${committee_id} {
    uint256 public index;
    string public id;
    string public committeeId;
    string public direction;
    uint256 public amount;
    string public paymentMethod;
    uint256 public paymentDate;
    address public committeeAddress;
    string public metadata;
    
    constructor  (
        uint256 _index,
        string memory _id,
        string memory _committeeId,
        string memory _direction,
        uint256 _amount,
        string memory _paymentMethod,
        uint256 _paymentDate,
        string memory _metadata
    )  public {
        index = _index;
        id = _id;
        committeeId = _committeeId;
        direction = _direction;
        amount = _amount;
        paymentMethod = _paymentMethod;
        paymentDate = _paymentDate;
        metadata = _metadata;
        
        committeeAddress = msg.sender;
    }
    
}


contract CommitteeContract_${committee_id} {
    
    event MemberAdded(address addr, string encode);
    event MemberRemoved(address addr);

    uint256 txnCount = 0;
    address operator;
    string public committeeId;
    
    Transaction_${committee_id}[] transactions;
    
    // committee meta data
    string public committeeName;
    string public state;
    string public scope;
    string public officeType;
    string public party;
    string public race;
    string public district;
    string public county;
    string public bankName;
    string public ruleVersion;
    string public filerId;
    string public electionId; 
    string public metadata;
    
    constructor(
        string memory _committeeId
    ) public {
        committeeId = _committeeId;
        operator = msg.sender;
    }
    
    function addMember(address _member, string memory _enode) public {
        assert(msg.sender == operator);
        emit MemberAdded(_member, _enode);
    }

    function removeMember(address _member) public {
        assert(msg.sender == operator);
        emit MemberRemoved(_member);
    }
    
    function initialize(
        string memory _committeeId,
        string memory _committeeName,
        string memory _state,
        string memory _scope,
        string memory _officeType,
        string memory _party,
        string memory _race,
        string memory _district,
        string memory _county,
        string memory _bankName,
        string memory _ruleVersion,
        string memory _filerId,
        string memory _electionId,
        string memory _metadata
    ) public returns (bool) {
        committeeId = _committeeId;
        committeeName = _committeeName;
        state = _state;
        scope = _scope;
        officeType = _officeType;
        party = _party;
        race = _race;
        district = _district;
        county = _county;
        bankName = _bankName;
        ruleVersion = _ruleVersion;
        filerId = _filerId;
        electionId = _electionId;
        metadata = _metadata;
        return true;
    }

    function commitTransactionAndGetIndex(
        string memory _id,
        string memory _committeeId,
        string memory _direction,
        uint256 _amount,
        string memory _paymentMethod,
        uint256 _paymentDate,
        string memory _metadata    
    ) public returns (uint256){
        
        uint256 index = txnCount;
        
        Transaction_${committee_id} transaction = new Transaction_${committee_id}(
             index,
             _id,
             _committeeId,
             _direction,
             _amount,
             _paymentMethod,
             _paymentDate,
             _metadata 
        );
        transactions.push(transaction);
        
        txnCount++;
        
        return index;
    }

    function getTransactionId(uint256 _index) public view returns (address){
        assert(_index < txnCount);
        return transactions[_index];
    }
    
    function getTransactionCount() public view returns (uint256){
        return txnCount;
    }
}
`;
};
