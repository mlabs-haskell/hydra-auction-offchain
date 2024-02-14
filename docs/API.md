# Hydra Auction Frontend API

This document provides description of API endpoints available within a frontend glue library that allows Hydra Auction frontend to call auction-related operations (also known as "contracts").

## Executing operations

 The library can be used by importing a set of types and functions from `hydra-auction-offchain` package as shown in the testing page in the `/demo` folder.

Operations are exposed as asyncronous functions (eponymous to names in this document) that yields a value of type `ContractOutput`.

### Wallet integration

Most operations takes a value of `WalletApp`, which is basically the name of the wallet of user's choice as a parameter to initialize the instance of CTL. This brings wallet integration facilities to the operations, so whenever a signature is required for a transaction, the wallet can be accessed automatically by built-in CTL's means.

### ContractOutput

This type is similar to `Either` datatype with `ContractOutputError` being an error and the result of arbitrary type T which is specific to every operation.

```typescript
export type ContractOutput<T> = ContractOutputResult<T> | ContractOutputError;
```

In both cases the value provides field `tag` which can be used to discriminate successful results (`output.tag === "result"`) from failures.

In most cases the result will contain submitted transaction identifier typically in `output.value.txHash` and the auxiliary operation `awaitTxConfirmed` can be used to wait until the transaction settles.

# Supported Operations

The table below summarises auction operations and shows which one can be called by different type of users.

Operations for delegates are provided for the sake of completeness and are not described in the this document.

<table>
  <thead>
      <tr>
          <th rowspan=2 colspan=2>API Endpoints</th>
          <th rowspan=2>All Users</th>
          <th rowspan=2>Sellers</th>
          <th colspan=2 style="text-align:center">Bidders</th>
          <th rowspan=2>Delegates</th>
      </tr>
  <tr>
          <th>Winners</th>
          <th>Losers</th>
  </tr>
  </thead>
  <tbody  align="center">
    <tr>
      <td align="left">queryAuctions</td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">cleanupAuction</td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">queryStandingBidState</td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">announceAuction</td>
      <td></td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">discoverBidders</td>
      <td></td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">authorizeBidders</td>
      <td></td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">startBidding</td>
      <td></td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">claimAuctionLotSeller</td>
      <td></td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">enterAuction</td>
      <td></td>
      <td></td>
      <td></td>
      <td colspan="2">+</td>
      <td></td>
    </tr>
    <tr>
      <td align="left">discoverSellerSignature</td>
      <td></td>
      <td></td>
      <td></td>
      <td colspan="2">+</td>
      <td></td>
    </tr>
    <tr>
      <td align="left">placeBid</td>
      <td></td>
      <td></td>
      <td></td>
      <td colspan="2">+</td>
      <td></td>
    </tr>
    <tr>
      <td align="left">claimAuctionLotBidder</td>
      <td></td>
      <td></td>
      <td></td>
      <td>+</td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td align="left">claimDepositLoser</td>
      <td></td>
      <td></td>
      <td></td>
      <td></td>
      <td>+</td>
      <td></td>
    </tr>
    <tr>
      <td align="left">announceDelegateGroup</td>
      <td></td>
      <td></td>
      <td></td>
      <td></td>
      <td></td>
      <td>+</td>
    </tr>
  </tbody>
</table>


## Avalable for all users

### queryAuctions

Queries all currently existing auctions discoverable on L1, filtering out invalid ones (i.e. having invalid auction terms).

Takes no parameters.

Returns the list of `AuctionInfo` values, which are `AuctionTerms` plus some on-chain auction paramteres, like auction token currency symbol and addresses of auction validators.

### cleanupAuction

Cleanup procedure is supposed to happen after the purchase deadline, burning down all auction-realted assets and thus removing all information from L1.

Takes `AuctionInfo` as an argument.

Returns the hash of a subbmitted cleaning transaction.

### queryStandingBidState

The one and only (but potentially absent) standing bid for an auction can be obtained using this operations.

Takes `AuctionInfo` as an argument.

Returns optionally missing `Maybe BidTerm` value.

## Available to Sellers

### announceAuction

 Announce an auction on-chain by minting tokens, saving the auction metadata on-chain, and placing the auction lot into the auction's escrow.

 Takes:
  1. Auction terms
  2. Optional additional UTxOs that allows the seller to cover auction lot `Value` in some cases. This can be useful if some portion of the lot is, for example, locked at a multi-signature address.

Returns `ActionInfo` along with the hash of an announcing transaction.

This operation can error with a failure enumerated in the type `AnnounceAuctionContractError`.

### discoverBidders

Discovers bidders who have indicated their interest in participating
in an auction by paying a bidder deposit.

Takes `AuctionInfo` as an argument.

Returns possibly empty list of `BidderInfoCandidate` that carries known info about the potential bidder:
1. Bidder's PKH and address.
2. Deposit amount.
3. Flag that indicates whether a bidder is compliant with given auction terms.

### authorizeBidders

Authorizes candidates to participate in the auction by posting a list of signatures on-chain at the personal oracle validator. Bidders can then discover and use these signatures using `discoverSellerSignature` operation.

Takes:
1. Auction token currency symbol.
2. List of PKH belonging to authorized bidders.

Returns the hash of a authorizing transaction.

This operations might error see type `AuthBiddersContractError`.

### startBidding

Starts off the auction, enabling authorized bidders to begin placing their bids.

Takes `AuctionInfo` as an argument.

Returns the hash of the starting transaction.

This operation might error, see type `StartBiddingContractError`.

### claimAuctionLotSeller

Claims the auction lot and deposit if the auction lot has not been purchased by the winning bidder before purchase deadline, distribute the auction fees to the delegates.

Takes `AuctionInfo` as an argument.

Returns the hash of a claiming transaction.

## Available to Bidders

### enterAuction

Used by a bidder for applying for participation in an auction by sending a deposit equal to or greater than the `minDepositAmount` specified in `AuctionTerms`.

Takes:
1. `AuctionInfo`
2. `depositAmount`. If being set to `null`, the minimum deposit specified in auction terms will be used.

Returns the hash of a transaction.

This operation might error see type `EnterAuctionContractError`.

### discoverSellerSignature

For an authorized bidder the operation allows to discover thier own bidder-specific authorization signature produced by a seller, which is required for placing bids on the auction. The operation uses a connected wallet to identify a bidder.

Takes:
1. Auction token currency symbol.
2. The seller's address form auction terms.

Returns a seller's signature for a bidder posted using `authorizeBidders` operation or `null` if the signature was not found.

This operation might error, see tyep `DiscoverSellerSigContractError`.

### placeBid

Places a bid. taking:
1. `AuctionInfo`;
2. A seller signature obtianed with `discoverSellerSignature` that confirms bidder's autorization for an auction;
3. A bid amount.

Returns the hash of a bidding transaction.

This operation might error see type `PlaceBidContractError`.

### claimAuctionLotBidder

Claim the auction lot if a bid placed by a bidder wins, distribute the auction fees to the delegates.

Takes `AuctionInfo` as an argument.

Return a hash of a claiming transaction.

### claimDepositLoser

Reclaim the deposit if a someone else's bid winned.

Takes `AuctionInfo` as an argument.

Return a hash of a claiming transaction.

## Additional Operations

### awaitTxConfirmed

This operation provides a way to wait until a transaction identified by its hash makes it to the blockchain. This is a common element for many use cases resulting in a new transaction being submitted.

# Auction Workflow

This sectrion shows a typical high-level workflow for an auction that demonstrates the use of API operations descibed in the document as well as some additional steps that are expected to be carried out on the frontend of the Hydra auction application (those operations are in uppoercase).

01. __seller__: auctionTerms <- `BUILD_AUCTION_TERMS`()
02. __seller__: auctionInfo <- `announceAuction`(auctionTerms)
03. __anyone__: [auctionInfo] <- `queryAuctions`()
04. __bidder__: auctionInfo = `SELECT_AUCTION`([auctionInfo])
05. __bidder__: depositAmount <- `SELECT_DEPOSIT_AMOUNT`(auctionInfo)
06. __bidder__: `enterAuction`(auctionInfo,` depositAmount`)
07. __seller__: candidates <- `discoverBidders`(auctionInfo)
08. __seller__: bidders <- `SELECT_BIDDERS`(candidates)
09. __seller__: `authorizeBidders`(auctionInfo.auctionId, bidders)
10. __bidder__: sellerSignature <- `discoverSellerSignature`(auctionInfo.auctionId, auctionInfo.auctionTerms.sellerAddress)
11. __seller__: `startBidding`(auctionInfo)
12. __anyone__: bidState <- `queryStandingBidState`(auctionInfo)
13. __bidder__: bidAmount <- `SELECT_BID_AMOUNT`(auctionInfo, bidState.price)
14. __bidder__: `placeBid`(auctionInfo, sellerSignature, bidAmount)
15. __winner__: `claimAuctionLotBidder`(auctionInfo)
16. __loser__: `claimDepositLoser`(auctionInfo)
17. __seller__ (if the winner didn't buy): `claimAuctionLotSeller`(auctionInfo)
18. __anyone__: `cleanupAuction`(auctionInfo)