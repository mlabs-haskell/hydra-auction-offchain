# Fully-Backed Bids

## Problem Statement (Bid Guarantees on L2)

Part of the unique value of auctions is that they provide participants a social system for granular price discovery of the lot. This is useful in a variety of situations where the lot's value may not be algorithmically discoverable.

A key consideration when designing blockchain auction protocols is providing guarantees of bid fulfillment by the bidder. If the winning bidder simply does not pay for the auction after completion, it's not clear what the best solution is.
- The auction lot can be returned to the seller, allowing them to restart the auction. This represents a monetary and time loss to all participants of the first auction and provides several potential negative use cases for bad actors.
- The auction can resolve to the second highest bidder. This breaks the value of price discovery, and provides a clear incentives for bad actors to manipulate the auction for personal gain.

## Current System (Bid Deposits)

The current auction system reinforces bid fulfillment by requiring bidders to provide a deposit of some amount determined by the seller, which can only be claimed back by the winner if they pay the amount they bid. If the bidder wins the auction and fails to pay the amount they bid, the deposit can be claimed by the seller.

This is a good solution, but it isn't without its downsides. It has the benefit of compensating the seller for their lost time, and disincentivizing bad actors, but it doesn't truly guarantee bids, doesn't provide any guarantees for other bidders, and can potentially hurt honest bidders who have simply missed the window to purchase the lot.

## Future Spec (Fully-Backed Bids)

The simplest solution to the bid guarantee problem on L1 is fully-backed bids - requiring bidders to lock the amount of funds they promise in a contract which can be withdrawn by the seller when the bidder wins. To increase his bid, the bidder has to increase his bid again with more funds from his wallet. 

On L2, this scheme runs into problems when the flow between the user's wallet on the L1 and their bid on the L2 isn't fast or consistently available. We decided against fully backed bids in the initial spec because at the time funds could only be moved between L1 and L2 when the Hydra head first starts, clearly breaking both speed and constancy requirements.

Increasing bids on L2 requires incremental commit/decommit, a feature which has recently been rolled out by the Hydra team. Unfortunately, according to our analysis, that feature is not yet mature enough for integration into our development timeline. As such, we have decided to leave Fully-Backed Bids un-implemented.

There are two ways this could be implemented when incremental commits:

### Granular Commit

Each user is granted permission to move funds into L2 at any time. This provides the best user experience by best satisfying the speed and constancy requirements. Potential drawbacks and problems may occur around network over-utilization due to the volume of requests from individual users.

#### L1 Changes

```
Endpoints
 - SendToL2
 - SendToL1

```

#### L2 Changes

### Batch Commits

If throughput between L1/L2 is limited or inconsistent, it may make more sense to batch commits together. This would require a slightly different schema:

#### L1 Changes

#### L2 Changes

## Alternative Solutions

### L2 Multi-Addressing

L1 fully backed bids don't break price discovery because of the uncertainty over the amount of funds users have available, due to the possibility for other sources of funds to participate.

The uncertainty can be recreated by allowing bidders to commit with multiple wallets. It does potentially enable other uses, such as short-term L2 lending. We will withold judgement on the desirability of such a case here.