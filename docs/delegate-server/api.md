## Delegate Server API

### HTTP

| Endpoint | Method | Description | Body | Response |
| --- | --- | --- | --- | --- |
| `/moveBid`  | POST | Move standing bid to L2 |          | [MoveBidResponse](#movebidresponse) |
| `/placeBid` | POST | Place bid on L2         | BidTerms | [PlaceBidResponse](#placebidresponse) |

#### MoveBidResponse
```jsonc
{ "tag": "Success" | "Error"
, "value": <MoveBidSuccess> | <MoveBidError>
}
```

* MoveBidSuccess

  ```jsonc
  { "tag": "SentInitHeadRequest"
  }
  ```
  ```jsonc
  { "tag": "CommittedStandingBid"
  , "value":
      { "standingBid": <BidTerms> | null
      , "txHash": <string>
      }
  }
  ```
  
* MoveBidError

  ```jsonc
  { "tag": "InvalidHeadStatus"
  , "value": <HydraHeadStatus>
  }
  ```
  ```jsonc
  { "tag": "CouldNotCommitStandingBid"
  , "value": <CommitStandingBidError>
  }
  ```

#### PlaceBidResponse
```jsonc
{ "tag": "Success" | "Error"
, "value": <PlaceBidSuccess> | <PlaceBidError>
}
```
* PlaceBidSuccess

  ```jsonc
  { "tag": "SubmittedTransaction"
  }
  ```

* PlaceBidError

  ```jsonc
  { "tag": "CouldNotDecodeBidTerms"
  , "value": <string> // decode error
  }
  ```
  ```jsonc
  { "tag": "ContractError"
  , "value": <PlaceBidL2ContractError>
  }
  ```

#### Miscellaneous
* HydraHeadStatus
  
  ```jsonc
  "Idle" | "Initializing" | "Open" | "Closed" | "FanoutPossible" | "Final" | "Unknown"
  ```

* BidTerms

  ```jsonc
  { "bidder": <BidderInfo>
  , "price": <string> // BigInt
  , "bidderSignature": <string> // ByteArray
  , "sellerSignature": <string> // ByteArray
  }
  ```

* BidderInfo

  ```jsonc
  { "bidderAddress": <string> // Bech32
  , "bidderVk": <string> // ByteArray
  }
  ```

### WebSocket

Clients should connect to one of the delegates' WebSocket servers to receive real-time information about new L2 bids and Hydra Head status.
Delegates broadcast the following messages to all connected clients:

#### HydraHeadStatus
Sent on connection and whenever there is a change in the status of the Hydra Head.
```jsonc
{ "tag": "HydraHeadStatus"
, "value": <HydraHeadStatus>
}
```

#### StandingBid
Sent on connection and whenever a new L2 bid has been confirmed.
```jsonc
{ "tag": "StandingBid"
, "value": <BidTerms> | null
}
```
