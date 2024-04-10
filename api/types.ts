// AnnounceAuction ---------------------------------------------------

export type AnnounceAuctionContractParams = {
  auctionTerms: AuctionTermsInput;
  delegateInfo: DelegateInfo | null;
  additionalAuctionLotOrefs: Array<TransactionInput>;
};

export type AnnounceAuctionContractOutput = {
  txHash: TransactionHash;
  auctionInfo: AuctionInfo;
};

// QueryAuctions -----------------------------------------------------

export type ActorRole = "ActorRoleSeller" | "ActorRoleBidder";

export type AuctionFilters = {
  myRole?: ActorRole;
};

// EnterAuction ------------------------------------------------------

export type EnterAuctionContractParams = {
  auctionInfo: AuctionInfo;
  depositAmount: BigInt | null;
};

// DiscoverBidders ---------------------------------------------------

export type BidderInfoCandidate = {
  bidderInfo: BidderInfo;
  depositAmount: BigInt;
  isValid: boolean; // complies with given auction terms
};

// AuthorizeBidders --------------------------------------------------

export type AuthorizeBiddersContractParams = {
  auctionCs: CurrencySymbol;
  biddersToAuthorize: Array<VerificationKey>;
};

// PlaceBid ----------------------------------------------------------

export type PlaceBidContractParams = {
  auctionInfo: AuctionInfo;
  sellerSignature: ByteArray;
  bidAmount: BigInt;
};

// DiscoverSellerSignature -------------------------------------------

export type DiscoverSellerSigContractParams = {
  auctionCs: CurrencySymbol;
  sellerAddress: Address;
};

// StartBidding ------------------------------------------------------

export type StartBiddingContractParams = {
  auctionInfo: AuctionInfo;
};

// Auction -----------------------------------------------------------

export type AuctionInfo = {
  auctionId: CurrencySymbol;
  auctionTerms: AuctionTerms;
  delegateInfo: DelegateInfo | null;
  auctionEscrowAddr: Address;
  bidderDepositAddr: Address;
  feeEscrowAddr: Address;
  standingBidAddr: Address;
  metadataOref: TransactionInput | null;
};

export interface AuctionTermsInput {
  auctionLot: Value;
  delegates: Array<PubKeyHash>;
  biddingStart: POSIXTime;
  biddingEnd: POSIXTime;
  purchaseDeadline: POSIXTime;
  cleanup: POSIXTime;
  auctionFeePerDelegate: BigInt;
  startingBid: BigInt;
  minBidIncrement: BigInt;
  minDepositAmount: BigInt;
}

export interface AuctionTerms extends AuctionTermsInput {
  sellerAddress: Address;
  sellerVk: VerificationKey;
}

export type BidderInfo = {
  bidderAddress: Address;
  bidderVk: VerificationKey;
};

export type BidTerms = {
  bidder: BidderInfo;
  price: BigInt;
  bidderSignature: ByteArray;
  sellerSignature: ByteArray;
};

export type DelegateInfo = {
  httpServers: string[];
  wsServers: string[];
};

export type StandingBidState = BidTerms | null;

// Common ------------------------------------------------------------

export type WalletApp =
  | "Nami"
  | "Gero"
  | "Flint"
  | "Eternl"
  | "Lode"
  | "NuFi"
  | "Lace"
  | "Plutip";

export type ContractOutput<T> = ContractOutputResult<T> | ContractOutputError;

export type ContractOutputResult<T> = {
  tag: "result";
  value: T;
};

export type ContractOutputError = {
  tag: "error";
  value: ContractError;
};

export type ContractError = {
  errorCode: string;
  message: string;
};

// Cardano -----------------------------------------------------------

export type Address = string;

export type BigInt = string;

export type ByteArray = string;

export type CurrencySymbol = string;

export type POSIXTime = string;

export type PubKeyHash = string;

export type TokenName = string;

export type TransactionInput = {
  transactionId: TransactionHash;
  index: UInt;
};

export type TransactionHash = string;

export type TxCbor = string;

export type UInt = string;

export type Value = Array<ValueEntry>;

export type ValueEntry = {
  cs: CurrencySymbol;
  tn: TokenName;
  quantity: BigInt;
};

export type VerificationKey = ByteArray;
