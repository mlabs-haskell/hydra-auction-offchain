# Leveraging PureScript Hydra SDK for Cardano Hydra Application Development

The **PureScript Hydra SDK** was extracted from the initial implementation
of the Hydra Auction service and generalized to support a wider range of
Hydra-based applications. It provides user-friendly interfaces
for essential common tasks such as configuring, spinning up/interacting with a Hydra Node,
handling Hydra Head status changes, managing multiple application instances, and more.
The SDK allows developers to focus more on the domain logic of their applications,
rather than the underlying infrastructure.

The first version of the SDK was introduced in Milestone 5.
It happened after the development of the Hydra Auction service,
and this was the reason why initially the SDK  was not used within the service.
Its adoption for the Hydra Auction service in the current milestone
uncovered various weaknesses in that first version.
All of them have been successfully addressed.

# Development stack and compatibility

The SDK is written in PureScript, a functional programming language
inspired by Haskell that compiles to JavaScript.
The Hydra SDK is currently compatible with the latest released versions of
hydra-node (v0.19.0) and cardano-node (v10.1.4).
It leverages Cardano domain types from the
[`purescript-cardano-types`](https://github.com/mlabs-haskell/purescript-cardano-types)
repository, which, in turn, is built on top of the
[`cardano-serialization-lib`](https://github.com/Emurgo/cardano-serialization-lib) (CSL).
This fact makes it a perfect choice for projects that use
[`cardano-transaction-lib`](https://github.com/Plutonomicon/cardano-transaction-lib)
(CTL) for their off-chain logic, although it's important to note that the Hydra SDK
does not strictly imply the use of any specific backend for transaction building
and also does not depend on CTL.
To highlight that, a minimal working example in the `purescript-hydra-sdk` repository
just demonstrates how natural and easy it is to use CTL in combination with the SDK.

The SDK also provides various Hydra domain-specific types
along with other utility types used by the components of the library,
most of which have the corresponding bi-directional Aeson-based JSON codecs.
As part of our efforts to bring the Hydra SDK to its first stable version, we forked
[`purescript-codec-argonaut`](https://github.com/errfrom/purescript-codec-aeson/)
and replaced its underlying type with
Aeson to address precision issues when handling large integers.
This change allowed us to combine the type safety of Aeson with
the declarative nature of `purescript-codec-argonaut`.

# Basic development workflow

The recommended approach for using the Hydra SDK is to adapt and extend
the minimal example contained in the repository.
However, starting from scratch should also be relatively straightforward.

- First and foremost, we need to launch a Hydra Node, which can be done using
the `spawnHydraNode` function. This function accepts a typed record
with configuration parameters and returns information about the child process,
which can later be used as part of the cleanup procedure.

- Once the Hydra Node process is spawned and ready, the user can connect
to its WebSocket API using the `mkHydraNodeApiWebSocket` function
and attach a message handler.
This handler will serve as the entry point for advancing the application’s state.
  - For example, the Head participant may want to trigger the Hydra Head initialization
upon receiving the `Greetings` message, commit some funds when
the `HeadIsInitializing` message is received,
and finally begin submitting L2 transactions on `HeadIsOpen`.
- When closing a Head or contesting a snapshot, the corresponding `Close` and `Contest`
transactions may be silently dropped by `cardano-node`, which is a known issue in `hydra-node`.
To address this, `mkHydraNodeApiWebSocket` provides a convenient way
to specify retry strategies for these problematic transactions,
ensuring that they are successfully submitted and processed.

To further enhance the developer experience, we introduced an option
to automatically handle Hydra Head statuses, eliminating the need for developers
to explicitly maintain the current Head status by inspecting incoming messages.
Overall, the SDK provides all the essential tools for managing the lifecycle
of a Hydra-based application.

# Managing multiple Hydra Heads within a Delegate Group

In the Hydra Auction application, auctions are hosted by delegate groups.
Due to the requirement that each delegate group should be able
to host multiple auctions simultaneously,
we introduced an opinionated interface called `AppManager`.
Later, we generalized this interface, incorporating it into the Hydra SDK's extras.

The central idea behind `AppManager` is the concept of slots.
Within a delegate group, each slot represents a set of delegate configurations
sufficient to spin up a properly configured Hydra Head.
Delegates are expected to agree on the slot configurations upfront
and are responsible for ensuring their correctness.

In Hydra Auction, slot numbers are implicitly derived from the provided configurations,
with the first configuration corresponding to slot 0, the second to slot 1, and so forth.
Users are expected to reserve slots before making an initial Layer-1 commitment,
such as announcing an auction.
Upon reservation, they receive secrets from each delegate,
which can later be provided to host a Layer-2 application in the reserved slot.
This approach eliminates the need for communication or synchronization
between individual delegates, nor does it rely on a central server
to orchestrate the initialization of Hydra Heads.
Additionally, it enables on-demand hosting of auctions
while ensuring strong guarantees regarding the availability of reserved slots for users.

Two clear drawbacks of this mechanism are its static nature,
where everything must be correctly configured at the start
(which introduces additional complexity for delegates),
and the potential for malicious actors to reserve all available slots within a delegate group,
effectively paralyzing its operations.
The latter concern can be easily addressed with additional preventive measures
commonly used in real-world applications.
