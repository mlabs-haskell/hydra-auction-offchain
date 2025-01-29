#  Hydra Auctions Optimization and Robustness Improvements

The report covers different parts of Hydra Auctions we examined for possible
optimizations and presents the results.

# Hydra SDK

A significant part of the reusable code in the last milestone has been moved
to the Hydra SDK so we decided to prioritize the work on SDK's performance and
robustness.

Our main concern was reasonable support for multiple auctions
within the same delegate group. Though the system could operate without it,
it's a suboptimal approach that takes a lot of burden on delegates.
So we considered different scenarios and opted for
a slot-based static configuration.

In this approach, all participants of a delegate group agree on a set of possible
slots every participant can run, including all the details of future Hydra Heads.
Each slot corresponds to an auction, i.e. to jump up on L2 an auction should
get the confirmation for a slot reservation from every delegate. Once a head
in a slot is closed, the slot can be reused for the next auction. That approach
is materialized in the extra `AppManager` module within the SDK.

It has the following advantages:
* Static configuration simplifies many procedures within the flow, mostly
the setup for delegates.
* The natural limit of slots and hence Hydra instances prevents OOM issues.
* The details of heads in all slots are configured upfront statically,
so there is no need for a setup ceremony when a head is actually launched.
* Heads are launched when all confirmations are obtained, preventing
waste of resources for auctions that might never happen.
* Confirmations have an expiration time, so an adversary can't lock all slots forever
by occupying them once. This is a good base for further augmenting with
more production-ready anti-DDoS measures.

Additionally, there is a known issue in `hydra-node` that can lead to lost
transactions when closing a Head or contesting a snapshot.
To address it, SDK supports automatic resubmission with different strategies
to ensure these important but problematic transactions go through.

## Front-end Application

The Hydra Auctions front-end application has been thoroughly examined for possible
optimizations and clean-ups. We carried out most of our findings with detrimental 
effects on user or dev experience. Among them:
* Minimize Blockfrost queries that slowed down pages and increased network and API usage.
* Set sensible polling limits that previously were over-eager.
* Cleanup leaking effect dependencies, a subtle bug that may affect the functioning of the application. 
* Update package dependencies and fix all building warnings.
* Eliminate unused code paths.
  
## Off-chain Code: Querying Chain State

Querying chain state is the most common bottleneck when executing off-chain
code in dApps. Bearing this in mind, we opted for parameterized on-chain scripts.
That way every auction gets its own unique contract, and this tremendously simplifies
querying since auctions are isolated. This cuts down the amount of UTxO to filter,
minimizing the number of network interactions with services like
Blockfrost, and finally shortens the waiting time for the user.
This optimisation though comes at its toll - now we have to deploy a script
for every auction, and this requires a separate transaction and locking up a
returnable storage fees which currently are around 40 ADA.

## On-chain Contracts

Despite our initial expectations, we haven't detected any issues with the  performance
of the on-chain code in Hydra Auсtions. We attribute this fact to the following
decisions we've made at very early phases of development:
* Use of Plutarch to implement all validators and policies
* Use of error codes to help keep the size of the script smaller

In terms of robustness, we checked the susceptibilty of contracts to known
vulnerabilities including:
* Checks by-passing
* Leaking protocol tokens
* Unauthorised protocol actions
* Unspendable outputs
* Protocol halting

Though this work can't be regarded as a full-fledged audit, we have
pretty high assurance the scripts are free from critical vulnerabilities
and that fact lies good ground for Hydra Auctions protocol robustness.
