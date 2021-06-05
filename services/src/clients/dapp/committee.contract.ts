export const committeeContract = `
pragma solidity ^0.4.25;


contract Transaction {
    uint256 public index;
    string public id;
    string public committeeId;
    string public direction;
    uint256 public amount;
    string public paymentMethod;
    bool public bankVerified;
    bool public ruleVerified;
    uint256 public initiatedTimestamp;
    string public source;
    address public committeeAddress;
    
    constructor  (
        uint256 _index,
        string memory _id,
        string memory _committeeId,
        string memory _direction,
        uint256 _amount,
        string memory _paymentMethod,
        bool _bankVerified,
        bool _ruleVerified,
        uint256 _initiatedTimestamp,
        string memory _source
    )  public {
        index = _index;
        id = _id;
        committeeId = _committeeId;
        direction = _direction;
        amount = _amount;
        paymentMethod = _paymentMethod;
        bankVerified = _bankVerified;
        ruleVerified = _ruleVerified;
        initiatedTimestamp = _initiatedTimestamp;
        source = _source;
        
        committeeAddress = msg.sender;
    }
    
}


contract CommitteeContract {
    
    event MemberAdded(address addr, string encode);
    event MemberRemoved(address addr);

    uint256 txnCount = 0;
    address operator;
    
    Transaction[] transactions;
    
    constructor() public {
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

    function commitTransactionAndGetIndex(
        string memory _id,
        string memory _committeeId,
        string memory _direction,
        uint256 _amount,
        string memory _paymentMethod,
        bool _bankVerified,
        bool _ruleVerified,
        uint256 _initiatedTimestamp,
        string memory _source    
    ) public returns (uint256){
        
        uint256 index = txnCount;
        
        Transaction transaction = new Transaction(
             index,
             _id,
             _committeeId,
             _direction,
             _amount,
             _paymentMethod,
             _bankVerified,
             _ruleVerified,
             _initiatedTimestamp,
             _source 
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
