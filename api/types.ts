// AnnounceAuction ---------------------------------------------------

export type AnnounceAuctionContractParams = {
  auctionTerms: AuctionTermsInput;
  additionalAuctionLotOrefs: Array<TransactionInput>;
};

export type AnnounceAuctionContractOutput = {
  txHash: TransactionHash;
  auctionInfo: AuctionInfo;
};

// EnterAuction ------------------------------------------------------

export type EnterAuctionContractParams = {
  auctionInfo: AuctionInfo;
  depositAmount: BigInt;
};

// AuthorizeBidders --------------------------------------------------

export type AuthorizeBiddersContractParams = {
  auctionCs: CurrencySymbol;
  biddersToAuthorize: Array<VerificationKey>;
};

// DiscoverSellerSignature -------------------------------------------

export type DiscoverSellerSigContractParams = {
  auctionCs: CurrencySymbol;
  // NOTE: sellerPkh will be replaced with sellerAddress: Address
  sellerPkh: PubKeyHash;
};

export type DiscoverSellerSigContractOutput = {
  txHash: TransactionHash;
  signature: ByteArray;
};

// StartBidding ------------------------------------------------------

export type StartBiddingContractParams = {
  auctionInfo: AuctionInfo;
};

// Auction -----------------------------------------------------------

export type AuctionInfo = {
  auctionId: CurrencySymbol;
  auctionTerms: AuctionTerms;
  auctionEscrowAddr: Address;
  bidderDepositAddr: Address;
  feeEscrowAddr: Address;
  standingBidAddr: Address;
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
  // NOTE: sellerPkh will be replaced with sellerAddress: Address
  sellerPkh: PubKeyHash;
  sellerVk: VerificationKey;
}

export type BidderInfo = {
  // NOTE: bidderPkh will be replaced with bidderAddress: Address
  bidderPkh: PubKeyHash;
  bidderVk: VerificationKey;
};

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
